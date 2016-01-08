# -------------------------------------- #
# Script to read and prepare the Eskom electrification data for pre-lim analysis
# -------------------------------------- #

# start clean
rm(list=ls())

# define directories
datadir <- paste(getwd(), "/Data/", sep = "")
figdir <- paste(getwd(), "/Figs/", sep = "")
reportdir <- paste(getwd(), "/Reports/", sep = "")
scriptdir <- paste(getwd(), "/ScriptsAndRdas/", sep = "")
rdadir <- paste(getwd(), "/ScriptsAndRdas/", sep = "")
dropdir <- "C:/Users/Alex H/Dropbox (Nova SA)/" #AH
rfunctdir <- paste(dropdir, "Rfunctions/", sep = "")

# load necessary libraries
library(stringr)
library(lubridate)
library(openair)
library(dplyr)
library(reshape2)

# source addtional functions
source(file = paste(rfunctdir, "fixname.R", sep = ""))

# declare constants
TZ <- "Africa/Johannesburg"

# define auxiliary functions

# -------------------------------------- #
# load the data
df_electrification_data <- read.csv(file = paste(datadir, "Kwaza_electrification_data_sales.csv", sep = ""), sep = ';', header = TRUE, stringsAsFactors = FALSE)

mtrx_stand_meter <- read.csv(file = paste(datadir, "Kwaza_electrification_data_stand_meter_matrix.csv", sep = ""), header = TRUE, sep = ';', strip.white = TRUE, stringsAsFactors = FALSE)

# -------------------------------------- #
# standardise the column names
names(df_electrification_data) <- fixname(names(df_electrification_data))
names(mtrx_stand_meter) <- fixname(names(mtrx_stand_meter))

idx <- match(x = "badge_meter_number", table = names(df_electrification_data))
if (!is.na(idx)) {names(df_electrification_data)[[idx]] <- "meter_badge_nr"}

idx <- match(x = "mtr_badge_nbr", table = names(mtrx_stand_meter))
if (!is.na(idx)) {names(mtrx_stand_meter)[[idx]] <- "meter_badge_nr"}

# -------------------------------------- #
# remove unnecessary columns

## from df_electrification_data
dups <- table(duplicated(df_electrification_data$trx_id))
if (!("TRUE" %in% names(dups))) {
  df_electrification_data <- df_electrification_data[, - match("trx_id", table = names(df_electrification_data))]
} else {
  if (dups[["TRUE"]] < 1) {
    df_electrification_data <- df_electrification_data[, - match("trx_id", table = names(df_electrification_data))]
  } else { warning("Duplicates found in df_electrification_data$trx_id") }
}

## from mtrx_stand_meter
idxx <- match(c("stand_number", "prem_id", "meter_badge_nr"), table = names(mtrx_stand_meter))
if (any(is.na(idxx))) { stop("Primary columns not found in mtrx_stand_meter") }
mtrx_stand_meter <- mtrx_stand_meter[, idxx]

# -------------------------------------- #
# standardise the column types
## for df_electrification_data
### the 'trx_dttm' column
temp <- table(nchar(df_electrification_data$trx_dttm))
if (length(temp) > 1) { stop("Length (nchar) of fields in df_electrification_data$trx_dttm not consistent") }
df_electrification_data$trx_dttm <- as.POSIXct(x = df_electrification_data$trx_dttm, format = "%Y-%m-%d", tz = TZ)

### the 'svc_qty_kwh' column
df_electrification_data$svc_qty_kwh <- gsub(pattern = ",", replacement = ".", x = df_electrification_data$svc_qty_kwh, fixed = TRUE)
df_electrification_data$svc_qty_kwh <- as.numeric(df_electrification_data$svc_qty_kwh)

### the 'trx_amt' column
df_electrification_data$trx_amt <- gsub(pattern = ",", replacement = ".", x = df_electrification_data$trx_amt, fixed = TRUE)
df_electrification_data$trx_amt <- gsub(pattern = "[[:space:]]", replacement = "", x = df_electrification_data$trx_amt)
df_electrification_data$trx_amt <- gsub(pattern = "R", replacement = "", x = df_electrification_data$trx_amt, fixed = TRUE)
df_electrification_data$trx_amt <- gsub(pattern = "^[[:punct:]]$", replacement = "0.00", x = df_electrification_data$trx_amt, fixed = FALSE)
df_electrification_data$trx_amt <- as.numeric(x = df_electrification_data$trx_amt)

## for mtrx_stand_meter

## for both data sets
lvls <- c(names(table(df_electrification_data$stand_number)), names(table(mtrx_stand_meter$stand_number)))
lvls <- levels[!duplicated(levels)]
df_electrification_data$stand_number <- factor(x = df_electrification_data$stand_number, levels = lvls, exclude = NULL, ordered = FALSE)
mtrx_stand_meter$stand_number <- factor(x = mtrx_stand_meter$stand_number, levels = lvls, exclude = NULL, ordered = FALSE)
