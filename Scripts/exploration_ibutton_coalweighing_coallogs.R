# Date created: 01 Nov 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# script to combine the ibutton data, coal weighing data and coal-use log data for analysis
# ---------------------------------------- #
# start clean
rm(list = ls())
# ---------------------------------------- #
# source preamble
dropdir <- "C:/Users/Alex H/Dropbox (Nova SA)/"
projdir <- paste(dropdir, "Eskom Offset Study 2014 - Team/R/eop/", sep = "")
source(paste(projdir, "/Scripts/preamble.R", sep = ""))
# ---------------------------------------- #
# declare script constants
VERBOSE <- TRUE
DEBUG <- TRUE
INTERVAL <- "1 day"
# ---------------------------------------- #
# ---------------------------------------- #

# loads
{
load(paste(rdadir, "df_ibutton_data_classified.Rda", sep = ""))
df_ibutton_data_classified <- df_ibutton_data_classified[which(year(df_ibutton_data_classified$date) > 2014),]
## remove unnecessary fields
df_ibutton_data_classified <- df_ibutton_data_classified[, c("date", "stand_number", "energy_type", "insulation_type", "fire")]

# load(paste(rdadir, "df_coal_data_with_calc_cols.Rda", sep = ""))
# ## remove unnecessary fields
# df_coal_data_with_calc_cols <- df_coal_data_with_calc_cols[, c("date", "stand_number", "current_coal_weight", "units_bought_since_last_weighing", "kg_bought", "usage_net_kg", "daily_avg")]

load(paste(rdadir, "df_coal_data.Rda", sep = ""))
## remove unnecessary fields
df_coal_data <- df_coal_data[, c("date", "stand_number", "current_coal_weight", "units_bought_since_last_weighing")]

load(paste(rdadir, "exploration_coal_logs.Rda", sep = ""))
## remove unnecessary fields
colsToRemove <- c("weight_of_wood_batch_kg", "weight_of_a_filled_container_kg", "fieldw_initials", "fieldw_signoff_date", "source_file", "time_of_day", "type")
idxx <- match(x = colsToRemove, table = names(df_coal_logs_exploration))
if (any(!is.na(idxx))) {
  df_coal_logs_exploration <- df_coal_logs_exploration[, -idxx[!is.na(idxx)]]
}
rm(idxx, colsToRemove)
}
# ---------------------------------------- #
# determine the median coal per fire and wood per fire from the coal logs to be used in combination with the ibutton data
{
med_coal_pf <- median(x = df_coal_logs_exploration$total_coal, na.rm = TRUE)
med_wood_pf <- median(x = df_coal_logs_exploration$total_wood, na.rm = TRUE)
if (VERBOSE) {message("med_coal_pf: ", med_coal_pf)}
if (VERBOSE) {message("med_wood_pf: ", med_wood_pf)}
}
# ---------------------------------------- #
# time-sum all three sources to one day intervals (not timeAvg!!!)
## --------------------- ##
## ibutton data
{
ib_stands_and_types <- df_ibutton_data_classified[, c("stand_number", "energy_type", "insulation_type")]
ib_stands_and_types <- ib_stands_and_types[!duplicated(ib_stands_and_types),]

idxx <- match(c("energy_type", "insulation_type"), table = names(df_ibutton_data_classified))
df_ibutton_data_classified <- df_ibutton_data_classified[, -idxx]
rm(idxx)

df_ibutton_data_classified$fire <- gsub(pattern = "1", replacement = "0", x = df_ibutton_data_classified$fire, fixed = TRUE)
df_ibutton_data_classified$fire <- gsub(pattern = "2", replacement = "1", x = df_ibutton_data_classified$fire, fixed = TRUE)
df_ibutton_data_classified$fire <- as.integer(df_ibutton_data_classified$fire)

df_ibutton_data_classified$date <- substr(x = df_ibutton_data_classified$date, start = 1, stop = 10)

standsplits_ibuttons <- split(x = df_ibutton_data_classified, f = df_ibutton_data_classified$stand_number)

standsplits_ibuttons <- lapply(X = standsplits_ibuttons, FUN = function(sib) {
  if (is.null(sib)) {
    if (VERBOSE) {message("No ibutton info for current stand.")}
    return(sib)
  }
  
  # remove the stand_number field so that we can timeSum
  stnd <- unique(as.character(sib$stand_number))
  idx <- match(x = "stand_number", table = names(sib))
  sib <- sib[, -idx]
  
  # split according to day
  daysplits <- split(x = sib, f = sib$date)
  if (length(daysplits) < 1) {
    return(sib)
  }
  
  daysplits <- lapply(X = daysplits, FUN = function(d) {
    # if there is only one record for a particular day, no summing is needed, so return as is
    if (nrow(d) < 2) {
      d$date <- as.character(d$date)
      return(d)
    }
    
    # extract the date and remove the date field
    dte <- unique(as.character(d$date))
    idx <- match(x = "date", table = names(d))
    d <- d[, -idx, drop = FALSE]
    
    # get the sums
    episodes <- table((rle(d$fire))[["values"]])
    if ("1" %in% names(episodes)) {
      d$fire <- episodes[["1"]]
    } else {
      if ("0" %in% names(episodes)) {
          d$fire <- 0
      } else {
          d$fire <- NA_real_
      }
    }
    
    d <- d[!duplicated(d),, drop = FALSE]
    names(d) <- c("num_of_fires")
    
    # put the date field back and return
    d$date <- dte
    return(d)
  })
  
  sib <- do.call("rbind", daysplits)
  rm(daysplits)
  
  # put the stand_number field back
  sib$stand_number <- stnd
  
  # return
  return(sib)
})

df_ibutton_data_classified <- do.call("rbind", standsplits_ibuttons)
rm(standsplits_ibuttons)

df_ibutton_data_classified$date <- as.POSIXct(x = df_ibutton_data_classified$date, tz = TIME_ZONE, format = "%Y-%m-%d")
}
## --------------------- ##
## coal weighing data
{
## we do not have to timeAverage the coal weighing data because there is only one record per stand per day (if any). we do need to do some formatting, however...
df_coal_data$date <- substr(x = as.character(df_coal_data$date), start = 1, stop = 10)
df_coal_data$date <- as.POSIXct(x = df_coal_data$date, tz = TIME_ZONE)
}
## --------------------- ##
## the coal-use logs
{
### combine the stand_number and extension fields and remove the latter
df_coal_logs_exploration$stand_number <- paste(df_coal_logs_exploration$stand_number, 
                                                "x", 
                                                df_coal_logs_exploration$extension, 
                                                sep = "_")

idx <- match(x = "extension", table = names(df_coal_logs_exploration))
df_coal_logs_exploration <- df_coal_logs_exploration[, -idx]

# remove the time info from the date field
df_coal_logs_exploration$date <- as.POSIXct(x = substr(x = df_coal_logs_exploration$date, start = 1, stop = 10), tz <- TIME_ZONE)

# unfactorise the to_cook, to_heat_house, to_heat_water fields
unfactorCols <- c("to_cook", "to_heat_house", "to_heat_water")

for (c in 1:length(unfactorCols)) {
  colName <- unfactorCols[[c]]
  df_coal_logs_exploration[[colName]] <- as.character(df_coal_logs_exploration[[colName]])
  df_coal_logs_exploration[[colName]] <- gsub(pattern = "no", replacement = "0", x = df_coal_logs_exploration[[colName]], fixed = TRUE)
  df_coal_logs_exploration[[colName]] <- gsub(pattern = "yes", replacement = "1", x = df_coal_logs_exploration[[colName]], fixed = TRUE)
  df_coal_logs_exploration[[colName]] <- as.numeric(df_coal_logs_exploration[[colName]])
}

# transform the "new_or_refill" field to a numeric field and then remove it
df_coal_logs_exploration$num_of_fires <- NA_real_
df_coal_logs_exploration$new_or_refill <- as.character(df_coal_logs_exploration$new_or_refill)
idxx <- grep(pattern = "new", x = df_coal_logs_exploration$new_or_refill, fixed = TRUE)
if (length(idxx) > 0) {
  df_coal_logs_exploration[idxx, "num_of_fires"] <- 1
}
idxx <- grep(pattern = "refill", x = df_coal_logs_exploration$new_or_refill, fixed = TRUE)
if (length(idxx) > 0) {
  df_coal_logs_exploration[idxx, "num_of_fires"] <- 1
}
rm(idxx)

idx <- match(x = "new_or_refill", table = names(df_coal_logs_exploration))
if (!is.na(idx)) {df_coal_logs_exploration <- df_coal_logs_exploration[, -idx]}

standsplits_coalLogs <- split(x = df_coal_logs_exploration, f = df_coal_logs_exploration$stand_number)

# we shouldn't be averaging here, we should be summing...
standsplits_coalLogs <- lapply(X = standsplits_coalLogs, FUN = function(scl) {
  
  # fail to safe
  if (is.null(scl)) {return(scl)}
  
  # remove the stand_number field
  stnd <- unique(as.character(scl$stand_number))
  if (VERBOSE) {message("Now summing for stand ", stnd)}
  idx <- match(x = "stand_number", table = names(scl))
  scl <- scl[, -idx]
  
  # split the stand date according to day
  daysplits <- split(x = scl, f = scl$date)
  if (VERBOSE) {message("Length of daysplits: ", length(daysplits))}
  
  # fail to safe if daysplitting was unsuccessful
  if (length(daysplits) < 1) {
    rm(daysplits)
    scl$stand_number <- stnd
    return(scl)
  }
  
  # now sum by day
  daysplits <- lapply(X = daysplits, FUN = function(d) {
    # if there is only one record for a given day, no summing is needed, so return the record as is
    if (nrow(d) < 2) {
      d$date <- as.character(d$date)
      return(d)
    }
    
    # remove the date field
    dte <- unique(d$date)
    if (VERBOSE) {message("Summing day ", dte)}
    idx <- match(x = "date", table = names(d))
    d <- d[, -idx]
    
    # do the summing
    d <- colSums(x = d, na.rm = TRUE)
    
    # put the date field back and return
    d$date <- as.character(dte)
    return(d)
  })
  
  # Humpty Dumpty...
  if (length(daysplits) < 2) {
    scl <- daysplits[[1]]
  } 
  else {
    scl <- do.call("rbind", daysplits)
  }
  
  rm(daysplits)
  
  # put the stand_number field back and return
  scl$stand_number <- (strsplit(x = stnd, split = "_x_", fixed = TRUE)[[1]])[[1]]
  scl$extension <- (strsplit(x = stnd, split = "_x_", fixed = TRUE)[[1]])[[2]]
  
  return(scl)
})

# currently the names of standsplits_coalLogs are in the format of stndno__x_extension; fix it
stnds <- apply(X = as.array(names(standsplits_coalLogs)), MARGIN = 1, FUN = function(nm) {
  nm <- ((strsplit(x = nm, split = "_x_", fixed = TRUE))[[1]])[[1]]
  return(nm)
}) 
names(standsplits_coalLogs) <- stnds
rm(stnds)

df_coal_logs_exploration <- do.call("rbind", standsplits_coalLogs)
rm(standsplits_coalLogs)
df_coal_logs_exploration$date <- as.POSIXct(x = df_coal_logs_exploration$date, tz = TIME_ZONE, format = "%Y-%m-%d")


## --------------------- ##
## expand each coal log so that it contains one record for each day between the start date and end date of the coal log. If no record exists for a specific day, simply add a null-record for that day (i.e. we assume that the household simply didn't make any fires on that day).

standsplits_coalLogs <- split(x = df_coal_logs_exploration, f = df_coal_logs_exploration$stand_number)

standsplits_coalLogs <- lapply(X = standsplits_coalLogs, FUN = function(s) {
  
  # extract the stand and extension numbers for s
  stnd <- unique(s$stand_number)
  if (any(!is.na(stnd))) {stnd <- stnd[!is.na(stnd)]} else {
    stnd <- NA_character_
    warning("No stand number found for the current df...")
  }
  extension <- unique(s$extension)
  if (any(!is.na(extension))) {extension <- extension[!is.na(extension)]} else {
    extension <- NA_integer_
  }
  
  # construct the expanded date sequence for s
  minDate <- as.Date(x = min(s$date, na.rm = TRUE))
  maxDate <- as.Date(x = max(s$date, na.rm = TRUE))
  sDays <- seq.Date(from = minDate, to = maxDate, by = "1 day")
  
  # construct a new, yet still empty, data frame for s with the expanded date sequence
  expandedS <- as.data.frame(x = matrix(nrow = length(sDays), ncol = ncol(s)))
  
  # initialise the new, expanded data frame
  names(expandedS) <- names(s)
  expandedS$date <- as.POSIXct(x = substr(x = sDays, start = 1, stop = 10), format = "%Y-%m-%d")
  expandedS$stand_number <- stnd
  expandedS$extension <- extension
  
  otherCols <- names(expandedS)[(!(names(expandedS) %in% c("stand_number", "extension", "date")))]
  if (length(otherCols) > 0) {
    expandedS[, otherCols] <- 0
  }
  
  # copy the records in old s over to expandedS
  matches <- match(x = expandedS$date, table = s$date)
  if (any(!is.na(matches))) {
    expandedS[which(!is.na(matches)),] <- s[matches[!is.na(matches)],]
  }
  
  # clean up
  rm(minDate, maxDate, sDays, otherCols, matches)
  
  # return
  return(expandedS)
})

df_coal_logs_exploration <- do.call("rbind", standsplits_coalLogs)
rm(standsplits_coalLogs)
}
## --------------------- ##

# ---------------------------------------- #
# determine the min and max dates for the combined df
minDate <- min(min(df_coal_logs_exploration$date, na.rm = TRUE), 
               min(df_coal_data$date, na.rm = TRUE), 
               min(df_ibutton_data_classified$date, na.rm = TRUE), na.rm = TRUE)
minDate <- as.Date(substr(x = minDate, start = 1, stop = 10))

maxDate <- max(max(df_coal_logs_exploration$date, na.rm = TRUE), 
               max(df_coal_data$date, na.rm = TRUE), 
               max(df_ibutton_data_classified$date, na.rm = TRUE), na.rm = TRUE)
maxDate <- as.Date(substr(x = maxDate, start = 1, stop = 10))

# ---------------------------------------- #
# now construct an empty data frame with one row for each day between minDate and maxDate
days <- seq.Date(from = minDate, to = maxDate, by = "1 day")
colNames <- c("day", 
              "stand_number",
              "coal_weight_measured", 
              "units_bought_since_last_weighing", 
              "fires_pd_log", 
              "fires_pd_ibutton",
              "no_of_coal_fires_log", 
              "no_of_wood_fires_log", 
              "no_of_coal_containers_log", 
              "coal_pd_log", 
              "no_of_wood_batches_log", 
              "wood_pd_log")

mtrx_ib_cw_cl <- matrix(nrow = length(days), ncol = length(colNames))
colnames(x = mtrx_ib_cw_cl) <- colNames
df_ib_cw_cl <- as.data.frame(x = mtrx_ib_cw_cl, stringsAsFactors = FALSE)
df_ib_cw_cl[["day"]] <- days

rm(mtrx_ib_cw_cl, colNames, days)

# ---------------------------------------- #
# get a list of all the stands across the three sources
stands <- c(unique(as.character(df_coal_data$stand_number)),
            unique(as.character(df_coal_logs_exploration$stand_number)),
            unique(as.character(df_ibutton_data_classified$stand_number)))

# remove duplicates
stands <- stands[!duplicated(stands)]

# ---------------------------------------- #
# construct a list to hold a df of nature df_ib_cw_cl for each of the stands in stands
list_df_ib_cw_cl <- vector(mode = "list", length = length(stands))
names(list_df_ib_cw_cl) <- stands

# now populate that list
standsplits_ibuttons <- split(x = df_ibutton_data_classified, f = df_ibutton_data_classified$stand_number)
standsplits_coalLogs <- split(x = df_coal_logs_exploration, f = df_coal_logs_exploration$stand_number)
standsplits_coalWeighing <- split(x = df_coal_data, f = df_coal_data$stand_number)

list_df_ib_cw_cl <- apply(X = as.array(stands), MARGIN = 1, FUN = function(snm) {
  # construct the stand's empty df
  df <- df_ib_cw_cl
  df$stand_number <- snm
  
  # populate with data from the ibutton data
  idx <- match(x = snm, table = names(standsplits_ibuttons))
  if (!is.na(idx)) {
    ib_data <- standsplits_ibuttons[[idx]]
    matches <- match(x = as.character(df$day), table = as.character(ib_data$date))
    if (any(!is.na(matches))) {
      df[which(!is.na(matches)), "fires_pd_ibutton"] <- ib_data[matches[!is.na(matches)], "num_of_fires"]
    }
  }
  
  # populate with data from the coal-use log data
  idx <- match(x = snm, table = names(standsplits_coalLogs))
  if (!is.na(idx)) {
    cl_data <- standsplits_coalLogs[[idx]]
    matches <- match(x = as.character(df$day), table = as.character(cl_data$date))
    if (any(!is.na(matches))) {
      df[which(!is.na(matches)), "fires_pd_log"] <- cl_data[matches[!is.na(matches)], "num_of_fires"]
      df[which(!is.na(matches)), "coal_pd_log"] <- cl_data[matches[!is.na(matches)], "total_coal"]
      df[which(!is.na(matches)), "wood_pd_log"] <- cl_data[matches[!is.na(matches)], "total_wood"]
      df[which(!is.na(matches)), "no_of_wood_fires_log"] <- cl_data[matches[!is.na(matches)], "wood_fire"]
      df[which(!is.na(matches)), "no_of_coal_fires_log"] <- cl_data[matches[!is.na(matches)], "coal_fire"]
      df[which(!is.na(matches)), "no_of_wood_batches_log"] <- cl_data[matches[!is.na(matches)], "no_of_wood_batches"]
      df[which(!is.na(matches)), "no_of_coal_containers_log"] <- cl_data[matches[!is.na(matches)], "no_of_coal_containers"]
      # df_ib_cw_cl[which(!is.na(matches)), "extension"] <- cl_data[matches[!is.na(matches)], "extension"]
    }
  }
  
  # populate with data from the coal weighing data
  idx <- match(x = snm, table = names(standsplits_coalWeighing))
  if (!is.na(idx)) {
    cw_data <- standsplits_coalWeighing[[idx]]
    matches <- match(x = as.character(df$day), table = as.character(cw_data$date))
    if (any(!is.na(matches))) {
      df[which(!is.na(matches)), "coal_weight_measured"] <- cw_data[matches[!is.na(matches)], "current_coal_weight"]
      df[which(!is.na(matches)), "units_bought_since_last_weighing"] <- cw_data[matches[!is.na(matches)], "units_bought_since_last_weighing"]
      #df[which(!is.na(matches)), "coal_pd_weighing"] <- cw_data[matches[!is.na(matches)], "daily_avg"]
    }
  }
  
  return(df)
})

df_ib_cw_cl <- do.call("rbind", list_df_ib_cw_cl)
rm(list_df_ib_cw_cl, standsplits_ibuttons, standsplits_coalLogs, standsplits_coalWeighing, df_coal_data, df_coal_logs_exploration, df_ibutton_data_classified, ib_stands_and_types)

# ---------------------------------------- #
## add "coal_pd_ibutton" and "wood_pd_ibutton" fields to the df_ib_cw_cl
# df_ib_cw_cl$coal_pd_ibutton <- NA_real_
# df_ib_cw_cl$wood_pd_ibutton <- NA_real_

# ---------------------------------------- #
# remove the records that are absolutely empty
temp <- df_ib_cw_cl[, c("coal_weight_measured", "units_bought_since_last_weighing", "fires_pd_log", "fires_pd_ibutton", "coal_pd_log", "wood_pd_log")]
isAllNa <- apply(X = as.array(1:nrow(temp)), MARGIN = 1, FUN = function(r) {return(all(is.na(temp[r,])))})
df_ib_cw_cl <- df_ib_cw_cl[which(!isAllNa),]
rm(temp, isAllNa)

# ---------------------------------------- #
# add a 'type' field
df_ib_cw_cl$type <- NA_character_

load(paste(rdadir, "stands_and_types.Rda", sep = ""))

df_ib_cw_cl$stand_number <- as.character(df_ib_cw_cl$stand_number)
stands_and_types$stand_number <- as.character(stands_and_types$stand_number)

matches <- match(x = df_ib_cw_cl$stand_number, table = stands_and_types$stand_number)
df_ib_cw_cl[which(!is.na(matches)), "type"] <- stands_and_types[matches[!is.na(matches)], "type"]
rm(matches, stands_and_types)

# ---------------------------------------- #
# put the fields in a better order
df_ib_cw_cl <- move.col(df = df_ib_cw_cl, colName = "type", colIdx = 3)

# ---------------------------------------- #
# save
archive(fileName = "df_ib_cw_cl.Rda", currentDir = rdadir, verbose = TRUE)
save(df_ib_cw_cl, file = paste(rdadir, "df_ib_cw_cl.Rda", sep = ""))


# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...

# how many stands do we have of each type?
stnds <- unique(df_ib_cw_cl$stand_number)
types <- df_ib_cw_cl[match(x = stnds, table = df_ib_cw_cl$stand_number), "type"]
temp <- data.frame(stnds, types)
table(temp$types)
