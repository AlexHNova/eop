# Date created: 16 October 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# script to read and prepare the EOP ibutton data
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
# ---------------------------------------- #
# ---------------------------------------- #
# read the data
input_path_parent <- paste(dropdir, "Eskom Offset Study 2014 - Team/Temperature Buttons/", sep = "")
input_subfolders <- c("COAL/", "CONTROL/", "ELECTRICITY/", "LPG/")

data_by_energytype <- apply(X = as.array(input_subfolders), MARGIN = 1, FUN = function(f) {
  fullpathname <- paste(input_path_parent, f, sep = "")
  subdirs <- dir(path = fullpathname, full.names = FALSE, recursive = TRUE) 
  
  if (length(subdirs) < 1) {warning("No files found in this directory."); return(NULL)}
  
  data_by_ibutton <- apply(X = as.array(subdirs), MARGIN = 1, FUN = function(x) {
    filename <- paste(fullpathname, x, sep = "")
    if (VERBOSE) {message("Reading ", filename)}
    df <- read.csv(file = filename, header = FALSE, sep = ',', stringsAsFactors = FALSE, colClasses = c("character", "character", "character"))
    df <- df[, -3]
    names(df) <- c("date", "temperature")
    df$date <- gsub(pattern = "ï»¿", replacement = "", x = df$date, fixed = TRUE)
    return(df)
  })
  
  names(data_by_ibutton) <- tolower(subdirs)
  
  return(data_by_ibutton)
})

names(data_by_energytype) <- tolower(gsub(pattern = '/', replacement = "", x = input_subfolders, fixed = TRUE))

# ---------------------------------------- #
# split the button names etc. and compile everything into one big data frame
df_list <- apply(X = as.array(1:length(data_by_energytype)), MARGIN = 1, FUN = function(i) {
  energy_type <- names(data_by_energytype)[[i]]
  list_all <- data_by_energytype[[i]]
  
  list_all <- apply(X = as.array(1:length(list_all)), MARGIN = 1, FUN = function(ibidx) {
    filename <- names(list_all)[[ibidx]]
    df_ib <- list_all[[ibidx]]
    insulation_type <- (strsplit(x = filename, split = '/', fixed = TRUE))[[1]]
    insulation_type <- ifelse(test = length(insulation_type) > 1, yes = insulation_type[[1]], no = "none")
    filename <- gsub(pattern = "basic", replacement = "", x = filename, fixed = TRUE)
    filename <- gsub(pattern = "full", replacement = "", x = filename, fixed = TRUE)
    filename <- gsub(pattern = '/', replacement = "", x = filename, fixed = TRUE)
    filename <- gsub(pattern = "eop_", replacement = "", x = filename, fixed = TRUE)
    filename <- gsub(pattern = ".csv", replacement = "", x = filename, fixed = TRUE)
    filename <- gsub(pattern = "[[:blank:]]", replacement = "", x = filename)
    filename <- gsub(pattern = "[[:punct:]]", replacement = "_", x = filename)
    str_elements <- (strsplit(x = filename, split = "_", fixed = TRUE))[[1]]
    if (DEBUG) {message("str_elements: ")}
    if (DEBUG) {message("length of str_elements: ", length(str_elements))}
    if (DEBUG) {print(str_elements)}
    stand_number <- str_elements[[1]]
    button_id <- str_elements[[2]]
    read_date <- str_elements[[3]]
    button_shortname <- str_elements[[4]]
    button_place <- substr(x = button_shortname, start = nchar(button_shortname), stop = nchar(button_shortname))
    
    df_ib$stand_number <- stand_number
    df_ib$energy_type <- energy_type
    df_ib$insulation_type <- insulation_type
    df_ib$button_place <- button_place
    df_ib$button_id <- button_id
    df_ib$button_shortname <- button_shortname
    df_ib$read_date <- read_date
    
    return(df_ib)
  })
  
  df <- do.call("rbind", list_all)
  rm(list_all)
  
  return(df)
})

df_ibutton_data <- do.call("rbind", df_list)
rm(df_list, data_by_energytype)

## fix the errors
idxx <- grep(pattern = "e65c", x = as.character(df_ibutton_data$button_shortname))
if (length(idxx) > 0) {
  df_ibutton_data[idxx, "button_shortname"] <- "e65w"
  df_ibutton_data[idxx, "button_place"] <- "w"
}

# ---------------------------------------- #
# fix the errors in the date field
df_ibutton_data$date <- as.character(df_ibutton_data$date)
df_ibutton_data$date <- gsub(pattern = "°C", replacement = "", x = df_ibutton_data$date, fixed = TRUE)

idxx <- which(substr(x = df_ibutton_data$date, start = 2, stop = 2) == "/")
if (length(idxx) > 0) {

  ctch <- apply(X = as.array(idxx), MARGIN = 1, FUN = function(r) {
    
    elements <- (strsplit(as.character(df_ibutton_data[r, "date"]), split = "[[:blank:]]"))[[1]]
    if (length(elements) != 3) {stop("Houston (#1), not three elements found at ", r, ": ", elements)}
    dte <<- elements[[1]]
    tme <<- elements[[2]]
    tmp <<- elements[[3]]
    
    elements <- (strsplit(x = dte, split = "/", fixed = TRUE))[[1]]
    if (length(elements) != 3) {stop("Houston (#2), not three elements found at ", r, ": ", elements)}
    mo <- elements[[1]]
    if (nchar(mo) < 2) {mo <- paste("0", mo, sep = "")}
    mday <- elements[[2]]
    if (nchar(mday) < 2) {mday <- paste("0", mday, sep = "")}
    yr <- elements[[3]]
    dte <- paste(yr, mo, mday, sep = "-")
    
    elements <- (strsplit(x = tme, split = ":", fixed = TRUE))[[1]]
    hr <- elements[[1]]
    if (nchar(hr) < 2) {hr <- paste("0", hr, sep = "")}
    min <- elements[[2]]
    if (nchar(min) < 2) {min <- paste("0", min, sep = "")}
    tme <- paste(hr, min, "00", sep = ":")
    
    df_ibutton_data[r, "date"] <<- paste(dte, tme, sep = " ")
    df_ibutton_data[r, "temperature"] <<- tmp

  })
  rm(ctch)
}

rm(idxx)

## --------------------- #

# ---------------------------------------- #


# now format each field
# --------------------- #
## the 'date' field
## okay, so this is a particularly fragile field. Readings were supposed to happen every 20 minutes, i.e. xx:00, xx:20, xx:40.... Some ibuttons, however, measured on xx:01, xx:21 and xx:41, and others measured on xx:02, xx:22, xx:42. The timeAverage function in the openair library just messes things up if you ask it to average the times so that they all follow the xx:00, xx:20 and xx:40 pattern, so we will simply have to move some of the times back with 1 min and others by 2 mins.

df_ibutton_data$date <- gsub(pattern = "/", replacement = "-", x = df_ibutton_data$date, fixed = TRUE)
df_ibutton_data$date <- as.POSIXct(x = df_ibutton_data$date, tz = TIME_ZONE)

df_ibutton_data$minute <- minute(df_ibutton_data$date)
df_ibutton_data$date <- as.POSIXct(ifelse(minute(df_ibutton_data$date) %in% c(1, 21, 41), (df_ibutton_data$date +(-60)), ifelse(minute(df_ibutton_data$date) %in% c(2, 22, 42), (df_ibutton_data$date +(-60*2)), df_ibutton_data$date)), origin = "1970-01-01 00:00:00")

# --------------------- #
# ## now remove records that came in only after the 3rd of Sept (because at this point in time there are too few)
# maxdate <- as.POSIXct("2015-09-03 23:59:59")
# df_ibutton_data <- df_ibutton_data[which(df_ibutton_data$date < maxdate),]

# --------------------- #
## the 'temperature' field
df_ibutton_data$temperature <- as.numeric(df_ibutton_data$temperature)

# --------------------- #
## the 'stand_number' field
## first fix the errors
df_ibutton_data$stand_number <- as.character(df_ibutton_data$stand_number)

df_ibutton_data$stand_number <- gsub(pattern = "2949", replacement = "2149", x = df_ibutton_data$stand_number, fixed = TRUE)
df_ibutton_data$stand_number <- gsub(pattern = "2697", replacement = "3679", x = df_ibutton_data$stand_number, fixed = TRUE)
df_ibutton_data$stand_number <- gsub(pattern = "3977", replacement = "3974", x = df_ibutton_data$stand_number, fixed = TRUE)

df_ibutton_data$stand_number <- as.factor(df_ibutton_data$stand_number)

# --------------------- #
## the 'energy_type' field
df_ibutton_data$energy_type <- gsub(pattern = "electricity", replacement = "elec", x = df_ibutton_data$energy_type, fixed = TRUE)

# fix the energy_type for stand 3327
idx <- match(x = "3327", table = as.character(df_ibutton_data$stand_number))
if (!is.na(idx)) {
  df_ibutton_data[which(as.character(df_ibutton_data$stand_number) == "3327"), "energy_type"] <- "lpg"
}

# fix the energy_type for stand 2859
idx <- match(x = "2859", table = as.character(df_ibutton_data$stand_number))
if (!is.na(idx)) {
  df_ibutton_data[which(as.character(df_ibutton_data$stand_number) == "2859"), "energy_type"] <- "elec"
}

df_ibutton_data$energy_type <- as.factor(df_ibutton_data$energy_type)

# --------------------- #
## the 'insulation_type' field
## fix the errors for stand 3185 - Thembi says it is a full-retro fit
idxx <- grep(pattern = "3185", x = as.character(df_ibutton_data$stand_number))
if (length(idxx) > 0) {
  df_ibutton_data[idxx, "insulation_type"] <- "full"
}

df_ibutton_data$insulation_type <- as.factor(df_ibutton_data$insulation_type)

# --------------------- #
## the 'button_place' field
df_ibutton_data$button_place <- as.character(df_ibutton_data$button_place)
df_ibutton_data[which(df_ibutton_data$button_place == "1" | df_ibutton_data$button_place == "3"), "button_place"] <- "w"
df_ibutton_data$button_place <- as.factor(df_ibutton_data$button_place)

# --------------------- #
## now split the temperature and other columns into two columns - one for chimney data (c) and one for wall data (w)
df_c <- df_ibutton_data[which(as.character(df_ibutton_data$button_place) == "c"),]
idx <- match(x = "temperature", table = names(df_c))
if (!is.na(idx)) {names(df_c)[[idx]] <- "c"}
idx <- match(x = "button_place", table = names(df_c))
if (!is.na(idx)) { df_c <- df_c[, - idx]}
idx <- match(x = "button_id", table = names(df_c))
if (!is.na(idx)) {names(df_c)[[idx]] <- "c_id"}
idx <- match(x = "button_shortname", table = names(df_c))
if (!is.na(idx)) {names(df_c)[[idx]] <- "c_shortname"}
idx <- match(x = "read_date", table = names(df_c))
if (!is.na(idx)) {names(df_c)[[idx]] <- "c_read_date"}

df_w <- df_ibutton_data[which(as.character(df_ibutton_data$button_place) == "w"),]
idx <- match(x = "temperature", table = names(df_w))
if (!is.na(idx)) {names(df_w)[[idx]] <- "w"}
idx <- match(x = "button_place", table = names(df_w))
if (!is.na(idx)) { df_w <- df_w[, - idx]}
idx <- match(x = "button_id", table = names(df_w))
if (!is.na(idx)) {names(df_w)[[idx]] <- "w_id"}
idx <- match(x = "button_shortname", table = names(df_w))
if (!is.na(idx)) {names(df_w)[[idx]] <- "w_shortname"}
idx <- match(x = "read_date", table = names(df_w))
if (!is.na(idx)) {names(df_w)[[idx]] <- "w_read_date"}

df_c$stand_number <- as.character(df_c$stand_number)
df_c$tempid <- paste(df_c$stand_number, df_c$date, sep = "_")
df_w$stand_number <- as.character(df_w$stand_number)
df_w$tempid <- paste(df_w$stand_number, df_w$date, sep = "_")
df_w$c <- NA_real_
df_w$c_id <- NA_character_
df_w$c_shortname <- NA_character_
df_w$c_read_date <- NA_real_

matches <- match(x = df_w$tempid, table = df_c$tempid)
if (any(!is.na(matches))) {
  df_w[which(!is.na(matches)), "c"] <- df_c[matches[!is.na(matches)], "c"]
  df_w[which(!is.na(matches)), "c_id"] <- df_c[matches[!is.na(matches)], "c_id"]
  df_w[which(!is.na(matches)), "c_shortname"] <- df_c[matches[!is.na(matches)], "c_shortname"]
  df_w[which(!is.na(matches)), "c_read_date"] <- df_c[matches[!is.na(matches)], "c_read_date"]
}

# simply append the chimney button readings for which there are no correlating wall button readings to the end of the data frame
if ((length(matches[!is.na(matches)]) < nrow(df_c))) {
  no_w_matches <- df_c[-matches[!is.na(matches)],]
  no_w_matches$w <- NA_real_
  no_w_matches$w_id <- NA_character_
  no_w_matches$w_shortname <- NA_character_
  no_w_matches$w_read_date <- NA_real_
  df_w <- do.call("rbind", list(df_w, no_w_matches))
  rm(no_w_matches)
}

df_ibutton_data <- df_w
rm(df_w, df_c)

# remove the 'tempid' column
df_ibutton_data <- remove.col(df = df_ibutton_data, colName = "tempid")

# fix the shortname issue for button e73w
idxx <- grep(pattern = "e73", x = as.character(df_ibutton_data$w_shortname), fixed = TRUE)
if (length(idxx) > 0) {
  df_ibutton_data[idxx, "w_shortname"] <- "e73w"
}

# factorise the c_id, w_id, c_shortname, w_shortname fields
df_ibutton_data$c_id <- as.factor(df_ibutton_data$c_id)
df_ibutton_data$c_shortname <- as.factor(df_ibutton_data$c_shortname)
df_ibutton_data$w_id <- as.factor(df_ibutton_data$w_id)
df_ibutton_data$w_shortname <- as.factor(df_ibutton_data$w_shortname)

# --------------------- #
## the 'read_date' fields
df_ibutton_data$c_read_date <- gsub(pattern = "[[:blank:]]", replacement = "", x = df_ibutton_data$c_read_date)
df_ibutton_data$c_read_date <- gsub(pattern = "^2005", replacement = "2015", x = df_ibutton_data$c_read_date)
df_ibutton_data$c_read_date <- gsub(pattern = "^20015", replacement = "2015", x = df_ibutton_data$c_read_date)
df_ibutton_data$c_read_date <- gsub(pattern = "2015$", replacement = "15", x = df_ibutton_data$c_read_date)
df_ibutton_data$c_read_date <- as.character(df_ibutton_data$c_read_date)

df_ibutton_data$w_read_date <- gsub(pattern = "[[:blank:]]", replacement = "", x = df_ibutton_data$w_read_date)
df_ibutton_data$w_read_date <- gsub(pattern = "^2005", replacement = "2015", x = df_ibutton_data$w_read_date)
df_ibutton_data$w_read_date <- gsub(pattern = "^20015", replacement = "2015", x = df_ibutton_data$w_read_date)
df_ibutton_data$w_read_date <- gsub(pattern = "2015$", replacement = "15", x = df_ibutton_data$w_read_date)
df_ibutton_data$w_read_date <- as.character(df_ibutton_data$w_read_date)

# ---------------------------------------- #
# some button readings read "2014" instead of "2015"; fix it.
year(df_ibutton_data$date[which(year(df_ibutton_data$date) < 2015)]) <- 2015

# ---------------------------------------- #
# put the columns in a better order
df_ibutton_data <- df_ibutton_data[, c("date", "stand_number", "energy_type", "insulation_type", "w", "c", "w_id", "w_shortname", "w_read_date", "c_id", "c_shortname", "c_read_date")]

# ---------------------------------------- #
# put in a field to distinguish the measurements from the three different monitoring periods
df_ibutton_data$mon_per <- NA_integer_
mindate2 <- as.POSIXct("2015-09-15 00:00:00", tz = TIME_ZONE)
mindate1 <- as.POSIXct("2015-08-01 00:00:00", tz = TIME_ZONE)

df_ibutton_data$mon_per <- ifelse(df_ibutton_data$date < mindate1, 1, ifelse(df_ibutton_data$date < mindate2, 2, 3))

# ---------------------------------------- #
# tailor the date range of each field so that we exclude box measurements
monpersplits <- split(x = df_ibutton_data, f = df_ibutton_data$mon_per)

monpersplits <- lapply(X = monpersplits, FUN = function(mpdf) {
  
  standsplits <- split(x = mpdf, f = mpdf$stand_number)
  
  mindates <- sapply(X = standsplits, FUN = function(sdf) {
    return(min(sdf$date, na.rm = TRUE))
  })
  mindates <- as.POSIXct(x = mindates, tz = TIME_ZONE, origin = "1970-01-01 00:00:00")
  maxdates <- sapply(X = standsplits, FUN = function(sdf) {
    return(max(sdf$date, na.rm = TRUE))
  })
  maxdates <- as.POSIXct(x = maxdates, tz = TIME_ZONE, origin = "1970-01-01 00:00:00")
  rm(standsplits)
  
  mindate <- max(mindates, na.rm = TRUE)
  print(mindate)
  maxdate <- min(maxdates, na.rm = TRUE)
  print(maxdate)
  rm(mindates, maxdates)
  
  mpdf <- mpdf[which(mpdf$date >= mindate & mpdf$date <= maxdate),]
  
  return(mpdf)
})


df_ibutton_data <- do.call("rbind", monpersplits)
rm(monpersplits)

# ---------------------------------------- #
# one last time
df_ibutton_data$stand_number <- as.factor(df_ibutton_data$stand_number)
# ---------------------------------------- #
# save the data
archive(fileName = "df_ibutton_data.Rda", currentDir = rdadir, verbose = TRUE)
save(df_ibutton_data, file = paste(rdadir,"df_ibutton_data.Rda", sep = ""))

# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...

# df_ibutton_data$type <- paste(as.character(df_ibutton_data$energy_type), as.character(df_ibutton_data$insulation_type), sep = "_")
# 
# s3185 <- df_ibutton_data[which(as.character(df_ibutton_data$stand_number) == "3185"),]
# unique(s3185$type)


# # CORRELATING THE STANDS AND TYPES WITH THE LIST IN stands_and_types.Rda
# stnds <- unique(x = as.character(df_ibutton_data$stand_number))
# matches <- match(x = stnds, table = as.character(df_ibutton_data$stand_number))
# tps <- as.character(df_ibutton_data[matches, "type"])
# stnds_and_tps <- data.frame(stnds, tps)
# stands_and_types$type_ib <- NA_character_
# matches <- match(stands_and_types$stand_number, table = stnds_and_tps$stnds)
# stands_and_types[which(!is.na(matches)), "type_ib"] <- as.character(stnds_and_tps[matches[!is.na(matches)], "tps"])


# # SOMETHING IS NOT RIGHT IN THE w_id AND w_shortname FIELDS, BECAUSE THERE ARE 69 IDS, BUT 70 SHORTNAMES...
# wshortnms <- unique(as.character(df_ibutton_data$w_shortname))
# matches <- match(x = wshortnms, table = as.character(df_ibutton_data$w_shortname))
# wids <- as.character(df_ibutton_data[matches[!is.na(matches)], "w_id"])
# df_widsn <- data.frame(wshortnms, wids)
# dups <- as.character(df_widsn[duplicated(df_widsn$wids), "wids"])
# dups
# # okay, so its e73w that had a slip-up somewhere and only came through as e73 on some occasions...

# QUICK VISUAL EXPLORATION...
# df_ibutton_data <- df_ibutton_data[which(year(df_ibutton_data$date) > 2014 & df_ibutton_data$mon_per == 2),]
# df_ibutton_data$hod <- hour(x = df_ibutton_data$date)
# p <- ggplot(data = df_ibutton_data, mapping = aes(x = hod, y = c, group = stand_number)) + facet_wrap(facets = ~ type) + geom_smooth() #+ geom_smooth(aes(x = date, y = c, group = stand_number))
# p
