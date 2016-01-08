# Date created: 01 Nov 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# script to prepare the data from the coal logs kept by the households
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

# get the data
inputDir <- paste(dropdir, "Eskom Offset Study 2014 - Team/Coal use log in excel/", sep = "")
fileList <- dir(path = inputDir, all.files = FALSE)
## remove temporary system files from the file list
idx <- grep(pattern = "~$", x = fileList, fixed = TRUE)
if (length(idx) > 0) {
  fileList <- fileList[-idx]
}

rawData <- apply(X = as.array(fileList), MARGIN = 1, FUN = function(f) {
  fullName <- paste(inputDir, f, sep = "")
  if (VERBOSE) {message("Now reading: ", fullName)}
  dta <- read.xlsx(xlsxFile = fullName, sheet = 1, colNames = TRUE)
  dta$source_file <- f
  return(dta)
})

rm(inputDir, fileList)

# ---------------------------------------- #
# standardise field names
rawData <- lapply(X = rawData, FUN = function(dta) {
  names(dta) <- fixname(names(dta))
  names(dta) <- gsub(pattern = "stand", replacement = "stand_number", x = names(dta), fixed = TRUE)
  return(dta)
})

# ---------------------------------------- #
# combine them
df_coal_logs <- do.call("rbind", rawData)
rm(rawData)

# ---------------------------------------- #
# format the fields
numericCols <- c("weight_of_wood_batch_kg", "weight_of_a_filled_container", "no_of_wood_batches", "no_of_coal_containers")
for (c in 1:length(numericCols)) {
  colName <- numericCols[[c]]
  idx <- match(x = colName, table = names(df_coal_logs))
  if (is.na(idx)) {next}
  df_coal_logs[[idx]] <- as.numeric(df_coal_logs[[idx]])
}

integerCols <- c("stand_number", "extension", "date_year", "date_month", "date_day", "time_hour", "time_min")
for (c in 1:length(integerCols)) {
  colName <- integerCols[[c]]
  idx <- match(x = colName, table = names(df_coal_logs))
  if (is.na(idx)) {next}
  df_coal_logs[[idx]] <- as.integer(df_coal_logs[[idx]])
}

characterCols <- c("fieldw_initials", "source_file", "fieldw_signoff_date")
for (c in 1:length(characterCols)) {
  colName <- characterCols[[c]]
  idx <- match(x = colName, table = names(df_coal_logs))
  if (is.na(idx)) {next}
  df_coal_logs[[idx]] <- tolower(as.character(df_coal_logs[[idx]]))
}

yesNoCols <- c("to_cook", "to_heat_house", "to_heat_water")
for (c in 1:length(yesNoCols)) {
  colName <- yesNoCols[[c]]
  idx <- match(x = colName, names(df_coal_logs))
  if (is.na(idx)) {next}
  df_coal_logs[[idx]] <- tolower(as.character(df_coal_logs[[idx]]))
  df_coal_logs[[idx]] <- gsub(pattern = "^n$", replacement = "no", x = df_coal_logs[[idx]])
  df_coal_logs[[idx]] <- gsub(pattern = "^y$", replacement = "yes", x = df_coal_logs[[idx]])
  df_coal_logs[[idx]] <- as.factor(df_coal_logs[[idx]])
}

factorCols <- c("stand_number", "new_or_refill")
for (c in 1:length(factorCols)) {
  colName <- factorCols[[c]]
  idx <- match(x = colName, table = names(df_coal_logs))
  if (is.na(idx)) {next}
  df_coal_logs[[idx]] <- as.factor(tolower(as.character(df_coal_logs[[idx]])))
}

rm(numericCols, integerCols, characterCols, yesNoCols, factorCols, c, idx, colName)

# ---------------------------------------- #
# assemble the date field from its parts
doubleDigitCols <- c("date_month", "date_day", "time_hour", "time_min")
for (c in 1:length(doubleDigitCols)) {
  colName <- doubleDigitCols[[c]] 
  df_coal_logs[[colName]] <- ifelse(nchar(df_coal_logs[[colName]]) > 1, df_coal_logs[[colName]], paste("0", df_coal_logs[[colName]], sep = ""))
}
rm(doubleDigitCols, c, colName)

dte <- paste(df_coal_logs$date_year, df_coal_logs$date_month, df_coal_logs$date_day, sep = "-")
tme <- paste(df_coal_logs$time_hour, df_coal_logs$time_min, "00", sep = ":")
df_coal_logs$date <- as.POSIXct(x = paste(dte, tme, sep = " "), tz = TIME_ZONE, format = "%Y-%m-%d %H:%M:%S")
rm(dte, tme)

colsToRemove <- c("date_year", "date_month", "date_day", "time_hour", "time_min")
idxx <- match(x = colsToRemove, table = names(df_coal_logs))
if (any(!is.na(idxx))) {
  df_coal_logs <- df_coal_logs[, -idxx[!is.na(idxx)]]
}
rm(colsToRemove, idxx)

# ---------------------------------------- #
# put the fields in a better order
colOrder <- c("date", "stand_number", "extension", "no_of_wood_batches", "weight_of_wood_batch_kg", "no_of_coal_containers", "weight_of_a_filled_container_kg", "new_or_refill", "to_cook", "to_heat_house", "to_heat_water", "fieldw_initials", "fieldw_signoff_date", "source_file")
otherCols <- any(!(names(df_coal_logs) %in% colOrder))
if (otherCols) {
  otherCols <- names(df_coal_logs)[which(!(names(df_coal_logs) %in% colOrder))]
  colOrder <- c(colOrder, otherCols)
}
df_coal_logs <- df_coal_logs[, colOrder]
rm(colOrder, otherCols)

# ---------------------------------------- #
# make sure there are no duplicates!
## first remove the source_file field
idx <- match(x = "source_file", table = names(df_coal_logs))
if (!is.na(idx)) {
  df_coal_logs <- df_coal_logs[, -idx]
}
rm(idx)

## now remove the duplicates
df_coal_logs <- df_coal_logs[which(!duplicated(df_coal_logs)),]

standsplits <- split(x = df_coal_logs, f = df_coal_logs$stand_number)
standsplits <- lapply(X = standsplits, FUN = function(sdf) {
  dups <- duplicated(sdf$date)
  print(table(dups))
  sdf <- sdf[!dups,]
  return(sdf)
})
df_coal_logs <- do.call("rbind", standsplits)
rm(standsplits)

# ---------------------------------------- #
# fix some identified errors
## for stand 1933
idx <- which((as.character(df_coal_logs$stand_number) == "1933") &
             (month(df_coal_logs$date) == 6) &
             (day(df_coal_logs$date) == 15))

if (length(idx) > 1) {
  stop("More than one record found when only one was expected.")
}

if (length(idx) == 1) {
  df_coal_logs[idx, "no_of_coal_containers"] <- 1
}

## for stand 431
idxx <- which(as.character(df_coal_logs$stand_number) == "431" & df_coal_logs$no_of_wood_batches == 11)
if (length(idxx) > 0) {
  df_coal_logs[idxx, "no_of_wood_batches"] <- 1
}
idxx <- which(as.character(df_coal_logs$stand_number) == "431")
if (length(idxx) > 0) {
  df_coal_logs[idxx, "weight_of_a_filled_container_kg"] <- NA
}

## for stand 178
idxx <- which(as.character(df_coal_logs$stand_number) == "178" & is.na(df_coal_logs$date))
if (length(idxx) > 0) {
  df_coal_logs <- df_coal_logs[-idxx,]
}

## for stand 2167
idx <- which(as.character(df_coal_logs$stand_number) == "2167" & is.na(df_coal_logs$date))
if (length(idx) > 0) {
  df_coal_logs[idx, "date"] <- "2015-06-21 16:00:00"
}

## for stand 434
idx <- which(as.character(df_coal_logs$stand_number) == "434" & is.na(df_coal_logs$date))
if (length(idx) > 0) {
  df_coal_logs[idx, "date"] <- "2015-07-28 17:00:00"
}

## for stand 3152
idxx <- which(as.character(df_coal_logs$stand_number) == "3152" & df_coal_logs$date == "2015-08-26 17:00:00")
if (length(idxx) == 2) {
  df_coal_logs[idxx[2], "date"] <- "2015-08-27 17:00:00"
}
idxx <- which(as.character(df_coal_logs$stand_number) == "3152")
if (length(idxx) > 0) {
  df_coal_logs[idxx, "weight_of_a_filled_container_kg"] <- 7.45
}

## for stand 383
idx <- which(as.character(df_coal_logs$stand_number) == "383" & df_coal_logs$date == "2015-07-07 10:00:00")
if (length(idx) == 1) {
  df_coal_logs[idx, "new_or_refill"] <- "refill"
}

## for stand 3994
idxx <- which(as.character(df_coal_logs$stand_number) == "3994")
if (length(idxx) > 0) {
  df_coal_logs[idxx, "weight_of_wood_batch_kg"] <- 1.5
}

# ---------------------------------------- #
# TRY TO FILL IN THE MISSING VALUES!
# 1. some stands do not have values for their filled coal containers or the weight of their wood batches. So, determine the MEDIAN of those measurement among the stands that do have values for these parameters and use it for those stands that do not have values
# 2. some records do not have something in the "new_or_refill" field; fix it.
# 3. some records do not indicate the number of wood batches or coal containers used for the fire making; fix it

med_container_weight <- median(x = df_coal_logs$weight_of_a_filled_container_kg, na.rm = TRUE)
med_woodbatch_weight <- median(x = df_coal_logs$weight_of_wood_batch_kg, na.rm = TRUE)
med_no_containers <- median(x = df_coal_logs$no_of_coal_containers, na.rm = TRUE)
med_no_woodbatches <- median(x = df_coal_logs$no_of_wood_batches, na.rm = TRUE)

standsplits <- split(x = df_coal_logs, f = df_coal_logs$stand_number)

standsplits <- lapply(X = standsplits, FUN = function(s) {
  # ---------- #
  # for the new_or_refill field; simply put in "new" (we assume that each record corresponds to one fire event, not to the absence thereof
  if (any(is.na(s$new_or_refill))) {
    s[which(is.na(s$new_or_refill)), "new_or_refill"] <- "new"
  }
  
  # ---------- #
  # for the wood weight
  ## all values are missing
  if (all(is.na(s$weight_of_wood_batch_kg))) {
    s$weight_of_wood_batch_kg <- med_woodbatch_weight
  }
  
  ## only some values are missing
  if (any(is.na(s$weight_of_wood_batch_kg))) {
    ## get the median among the non-missing values
    med_wb <- median(x = s$weight_of_wood_batch_kg, na.rm = TRUE)
    s[which(is.na(s$weight_of_wood_batch_kg)), "weight_of_wood_batch_kg"] <- med_wb
  }
  
  # ---------- #
  # for the filled coal container weight
  ## all values are missing
  if (all(is.na(s$weight_of_a_filled_container_kg))) {
    s$weight_of_a_filled_container_kg <- med_container_weight
  }
  
  ## only some values are missing
  if (any(is.na(s$weight_of_a_filled_container_kg))) {
    med_ccw <- median(x = s$weight_of_a_filled_container_kg, na.rm = TRUE)
    s[which(is.na(s$weight_of_a_filled_container_kg)), "weight_of_a_filled_container_kg"] <- med_ccw
  }
  
  # ---------- #
  # for the number of wood batches
  ## all values are missing
  if (all(is.na(s$no_of_wood_batches))) {
    s$no_of_wood_batches <- med_no_woodbatches
  }
  
  ## only some values are missing
  if (any(is.na(s$no_of_wood_batches))) {
    med_no_wb <- median(x = s$no_of_wood_batches, na.rm = TRUE)
    s[which(is.na(s$no_of_wood_batches)), "no_of_wood_batches"] <- med_no_wb
  }
  
  # ---------- #
  # for the number of coal containers
  ## all values are missing
  if (all(is.na(s$no_of_coal_containers))) {
    s$no_of_coal_containers <- med_no_containers
  }
  
  ## only some values are missing
  if (any(is.na(s$no_of_coal_containers))) {
    med_no_cc <- median(s$no_of_coal_containers, na.rm = TRUE)
    s[which(is.na(s$no_of_coal_containers)), "no_of_coal_containers"] <- med_no_cc
  }
  
  # ---------- #
  
  return(s)
})

df_coal_logs <- do.call("rbind", standsplits)
rm(standsplits)

# ---------------------------------------- #
# order chronologically
df_coal_logs <- df_coal_logs[order(df_coal_logs$date),]
rownames(df_coal_logs) <- c(1:nrow(df_coal_logs))

# ---------------------------------------- #
# save!
archive(fileName = "df_coal_logs.Rda", currentDir = rdadir, verbose = TRUE)
save(df_coal_logs, file = paste(rdadir, "df_coal_logs.Rda", sep = ""))
  

# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...

# h383 <- df_coal_logs[which(as.character(df_coal_logs$stand_number) == "383"),]
# idxx <- which(yday(h383$date) == 188)
# h383[idxx,]
# 
# 
# h3791 <- df_coal_logs[which(as.character(df_coal_logs$stand_number) == "3791"),]
# idxx <- which(yday(h3791$date) == 185)
# h3791[idxx,]


