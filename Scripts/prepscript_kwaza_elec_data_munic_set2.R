# Date created: 05 November 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# script to prepare the 2nd set of municipality-sourced electricity data of the Eskom project
# ---------------------------------------- #
# start clean
rm(list = ls())
# ---------------------------------------- #
# source preamble
dropdir <- "C:/Users/Alex H/Dropbox (Nova SA)/"
projdir <- paste(dropdir, "Eskom Offset Study 2014 - Team/R/eop/", sep = "")
source(paste(projdir, "/Scripts/preamble.R", sep = ""))
perlPath <- "C:/Perl/bin/perl.exe"
# ---------------------------------------- #
# declare script constants
VERBOSE <- TRUE
DEBUG <- TRUE
# ---------------------------------------- #
# ---------------------------------------- #

# get the data
dirPath <- paste(datadir, "Electricity/Originals/Kwaza/", sep = "")
fileNames <- dir(path = dirPath)

rawData <- apply(X = as.array(fileNames), MARGIN = 1, FUN = function(fnm) {
  
  accountData <- read.xls(xls = paste(dirPath, fnm, sep = ""), sheet = 1, perl = perlPath, stringsAsFactors = FALSE, na.strings = c(""))
  
  if (ncol(accountData) < 4) {
    if (VERBOSE) {message("Empty file: ", fnm)}
    return(NULL)
  }
  
  colNames <- accountData[3,]
  accountData <- accountData[4:nrow(accountData),]
  names(accountData) <- colNames
  names(accountData) <- fixname(names(accountData))
  
  isAllNa <- NULL
  
  for (c in 1:ncol(accountData)) {
    if (all(is.na(accountData[[c]]))) {
      isAllNa <- c(isAllNa, TRUE)
    } else {isAllNa <- c(isAllNa, FALSE)}
  }
  
  accountData <- accountData[!isAllNa]
  
  return(accountData)
})

names(rawData) <- fileNames
names(rawData) <- gsub(pattern = ".xls", replacement = "", x = names(rawData), fixed = TRUE)
names(rawData) <- gsub(pattern = "^0{1,}", replacement = "", x = names(rawData))

# ---------------------------------------- #
# assign empty data frames to the stands that bought no electricity
## ------------------- ##
## copy the data frame format and structure as a template from the first non-null item in rawData
isNull <- sapply(X = rawData, FUN = is.null)
idxx <- which(!isNull)
dfTemplate <- rawData[[idxx[1]]]

rm(isNull, idxx)

## ------------------- ##
## clean the template
for (c in 1:ncol(dfTemplate)) {
  dfTemplate[[c]] <- NA
}
dfTemplate <- dfTemplate[1,]

makeZero <- c("amount", "vat", "cost_of_units", "units")
ctch <- apply(X = as.array(makeZero), MARGIN = 1, FUN = function(cnm) {
  dfTemplate[[cnm]] <<- 0
})

rm(ctch, makeZero)

## ------------------- ##
## now assign the template to the did-not-buy-electricity stands and fill their meter numbers into the template
rawData <- apply(X = as.array(names(rawData)), MARGIN = 1, FUN = function(nm) {
  df <- rawData[[nm]]
  if (is.null(df)) {df <- dfTemplate}
  df$meter_no <- nm
  return(df)
})

rm(dfTemplate)

df_elec_data_set3 <- do.call("rbind", rawData)
rm(dirPath, fileNames, rawData)

# ---------------------------------------- #
# format field contents, remove unnecessary columns etc.
names(df_elec_data_set3) <- gsub(pattern = "tranno", replacement = "tran_number", x = names(df_elec_data_set3), fixed = TRUE)
names(df_elec_data_set3) <- gsub(pattern = "tran_date_time", replacement = "date", x = names(df_elec_data_set3), fixed = TRUE)
names(df_elec_data_set3) <- gsub(pattern = "meter_no", replacement = "meter_number", x = names(df_elec_data_set3), fixed = TRUE)
names(df_elec_data_set3) <- gsub(pattern = "account_no", replacement = "account_number", x = names(df_elec_data_set3), fixed = TRUE)
df_elec_data_set3 <- remove.col(df = df_elec_data_set3, colName = "ststoken")
df_elec_data_set3 <- move.cols(df = df_elec_data_set3, colNames = c("date", "account_number", "user", "meter_number", "tran_type", "tran_number", "pos"), colIdxx = c(1:6, 13))

numericCols <- c("amount", "vat", "cost_of_units", "units", "tran_type", "account_number", "meter_number")
for (c in 1:length(numericCols)) {
  colName <- numericCols[c]
  df_elec_data_set3[[colName]] <- as.numeric(df_elec_data_set3[[colName]])
}
  
characterCols <- c("pos", "tran_number", "user", "tran_type", "account_number", "meter_number", "tariff", "meter_ti")
for (c in 1:length(characterCols)) {
  colName <- characterCols[c]
  df_elec_data_set3[[colName]] <- fixname(as.character(df_elec_data_set3[[colName]]))
}

factorCols <- c("tran_type", "account_number", "meter_number", "tariff", "meter_ti")
for (c in 1:length(factorCols)) {
  colName <- factorCols[c]
  df_elec_data_set3[[colName]] <- as.factor(df_elec_data_set3[[colName]])
}

df_elec_data_set3$date <- paste(df_elec_data_set3$date, "00", sep = ":")
df_elec_data_set3$date <- strptime(x = df_elec_data_set3$date, format = "%d-%m-%Y %H:%M:%S", tz = TIME_ZONE)
df_elec_data_set3$date <- as.POSIXct(x = df_elec_data_set3$date)

# ---------------------------------------- #
# make sure we have no duplicates
df_elec_data_set3 <- df_elec_data_set3[!duplicated(df_elec_data_set3), ]

# ---------------------------------------- #
# match stand numbers and stand types to each
df_elec_data_set3$stand_number <- NA_character_

load(paste(rdadir, "df_stands_info.Rda", sep = ""))

matches <- match(x = as.character(df_elec_data_set3$meter_number), table = as.character(df_stands_info$meter_number))
df_elec_data_set3[which(!is.na(matches)), "stand_number"] <- as.character(df_stands_info[matches[!is.na(matches)], "stand_number"])
df_elec_data_set3$stand_number <- as.character(df_elec_data_set3$stand_number)

rm(df_stands_info)

## ------------------- ##
df_elec_data_set3$type <- NA_character_

load(paste(rdadir, "stands_and_types.Rda", sep = ""))

matches <- match(x = as.character(df_elec_data_set3$stand_number), table = as.character(stands_and_types$stand_number))
df_elec_data_set3[which(!is.na(matches)), "type"] <- as.character(stands_and_types[matches[!is.na(matches)], "type"])

rm(matches, stands_and_types)

# ---------------------------------------- #
# rename some fields (again)
names(df_elec_data_set3) <- gsub(pattern = "tran_type", replacement = "transaction_type", x = names(df_elec_data_set3), fixed = TRUE)
names(df_elec_data_set3) <- gsub(pattern = "tran_number", replacement = "transaction_number", x = names(df_elec_data_set3), fixed = TRUE)
names(df_elec_data_set3) <- gsub(pattern = "^units$", replacement = "kwh_units", x = names(df_elec_data_set3))

# ---------------------------------------- #
# save
df_elec_data_munic_set2 <- df_elec_data_set3
rm(df_elec_data_set3)
archive(fileName = "df_elec_data_munic_set2.Rda", currentDir = rdadir, verbose = TRUE)
save(df_elec_data_munic_set2, file = paste(rdadir, "df_elec_data_munic_set2.Rda", sep = ""))




# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...

# # how many stands of each type do we have
# stnds <- unique(df_elec_data_munic_set2$stand_number)
# matches <- match(x = stnds, table = df_elec_data_munic_set2$stand_number)
# tps <- df_elec_data_munic_set2[matches, "type"]
# howmany <- data.frame(stnds, tps)
# table(howmany$tps, exclude = NULL)
