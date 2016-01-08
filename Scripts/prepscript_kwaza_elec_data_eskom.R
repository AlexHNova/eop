# Date created: 8 October 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# script to prepare the electricity usage data of the stands that are supplied with electricity directly from Eskom
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
df_elec_data_eskom <- read.xlsx(xlsxFile = paste(datadir, "Electricity/KwaZa_elec_data_Eskom.xlsx", sep = ""), colNames = TRUE, skipEmptyRows = FALSE)

# ---------------------------------------- #
# standardise the column names
names(df_elec_data_eskom) <- fixname(names(df_elec_data_eskom))

# ---------------------------------------- #
# standardise the meter number field
df_elec_data_eskom$badge_meter_number <- as.character(df_elec_data_eskom$badge_meter_number)
df_elec_data_eskom$badge_meter_number <- gsub(pattern = "^0{1,}", replacement = "", x = df_elec_data_eskom$badge_meter_number)
df_elec_data_eskom$badge_meter_number <- gsub(pattern = "[[:punct:]]", replacement = "", x = df_elec_data_eskom$badge_meter_number)
df_elec_data_eskom$badge_meter_number <- gsub(pattern = "[[:blank:]]", replacement = "", x = df_elec_data_eskom$badge_meter_number)

# ---------------------------------------- #
# try to fill the missing values in the 'stand_number' field
df_elec_data_eskom$stand_number <- as.character(df_elec_data_eskom$stand_number)

metersplits <- split(x = df_elec_data_eskom, f = df_elec_data_eskom$badge_meter_number)

metersplits <- lapply(X = metersplits, FUN = function(m) {
  stnd <- unique(m$stand_number)
  #message("before: ", stnd)
  
  if ((length(stnd) > 1) & (any(is.na(stnd)))) {
    stnd <- stnd[!is.na(stnd)]
    #message("after: ", stnd)
    m$stand_number <- stnd
    return(m)
  }
  
  if (is.na(stnd)) {
    #message("No unique stand number associated with the current meter number (", unique(m$badge_meter_number), ")")
    m$stand_number <- "unknown"
    return(m)
  }

  return(m)
})

df_elec_data_eskom <- do.call("rbind", metersplits)
rm(metersplits)

# -------------------- #
# format the stand_number field
df_elec_data_eskom$stand_number <- as.character(df_elec_data_eskom$stand_number)
df_elec_data_eskom$stand_number <- gsub(pattern = "^0{1,}", replacement = "", x = df_elec_data_eskom$stand_number)
df_elec_data_eskom$stand_number <- gsub(pattern = "[[:punct:]]", replacement = "", x = df_elec_data_eskom$stand_number)
df_elec_data_eskom$stand_number <- gsub(pattern = "[[:blank:]]", replacement = "", x = df_elec_data_eskom$stand_number)

# ---------------------------------------- #
# correct the formatting of the other fields
df_elec_data_eskom$trx_amt <- as.numeric(df_elec_data_eskom$trx_amt)

df_elec_data_eskom$trx_dttm <- df_elec_data_eskom$trx_dttm * (24*60*60)
df_elec_data_eskom$trx_dttm <- as.POSIXct(x = df_elec_data_eskom$trx_dttm, origin = "1900-01-01 00:00:00", tz = TIME_ZONE)

df_elec_data_eskom$trx_id <- as.character(df_elec_data_eskom$trx_id)

# ---------------------------------------- #
# put the columns in a better order and change their names for publication
wrong_names <- c("trx_id", "trx_dttm", "badge_meter_number", "svc_qty_kwh", "trx_amt")
right_names <- c("transaction_id", "date", "meter_number", "kwh_units", "amount")
matches <- match(x = wrong_names, table = names(df_elec_data_eskom))
if (any(!is.na(matches))) {
  names(df_elec_data_eskom)[matches[!is.na(matches)]] <- right_names[which(!is.na(matches))]
}

df_elec_data_eskom <- move.cols(df = df_elec_data_eskom, colNames = c("date", "stand_number", "meter_number", "transaction_id", "amount", "kwh_units"), colIdxx = c(1:6))

rm(wrong_names, right_names, matches)

# ---------------------------------------- #
# check for duplicates
df_elec_data_eskom <- df_elec_data_eskom[which(!duplicated(df_elec_data_eskom)),]

# ---------------------------------------- #
# we only need the data from the beginning of 2014, so remove the older data
df_elec_data_eskom <- df_elec_data_eskom[which(year(df_elec_data_eskom$date) > 2013),]

# ---------------------------------------- #
# save the data
archive(fileName = "df_elec_data_eskom.Rda", currentDir = rdadir, verbose = TRUE)
save(df_elec_data_eskom, file = paste(rdadir, "df_elec_data_eskom.Rda", sep = ""))

# ---------------------------------------- #
# ---------------------------------------- #
# NOTES

df_elec_data_eskom$mo <- (month(df_elec_data_eskom$date, label = TRUE))



library(doBy)
sums <- summaryBy(formula = kwh_units ~ mo, data = df_elec_data_eskom[which(!(df_elec_data_eskom$stand_number %in% c("1502", "unknown"))),], FUN = mean)

ggplot(data = df_elec_data_eskom[which(!(df_elec_data_eskom$stand_number %in% c("1502", "unknown"))),], mapping = aes(x = mo, y = kwh_units)) + geom_histogram(stat = "identity") + facet_grid(. ~ stand_number)












# there is this one stand, 1502 which has some 250602 records associated with it (i.e. 98.64% of the data in this df). this stand does not appear in Thembi's lists of stands. THIS DATA, HOWEVER, CONTAINS THE RECORDS OF 10 METER NUMBERS THAT ARE INDEED ASSOCIATED WITH SOME OF OUR STANDS, SO WE ARE NOT GOING TO EXCLUDE THIS DATA.

# there are some duplicates in the 'trx_id' column, but the rest of the fields in the "duplicated" records do differ from their "original" counterparts

# it seems that each stand has multiple metres associated with it

# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...

# table(duplicated(x = df_elec_data_eskom$trx_id))
# table(df_elec_data_eskom$badge_meter_number)
# table(df_elec_data_eskom$stand_number, df_elec_data_eskom$badge_meter_number, exclude = NULL)

# ---------------------------------------- #
# MATCH STANDS AND TYPES
# stnds <- unique(as.character(df_elec_data_eskom$stand_number))
# matches <- match(x = stnds, table = as.character(df_stands_info$stand_number))
# if (any(!is.na(matches))) {
#   e_tps <- df_stands_info[matches[!is.na(matches)], "energy_type"]
#   i_tps <- df_stands_info[matches[!is.na(matches)], "insulation_type"]
# }
# stnds <- stnds[!is.na(matches)]
# 
# df_stnds_tps <- data.frame(stnds, e_tps, i_tps, stringsAsFactors = FALSE)
# 
# df_elec_data_eskom$energy_type <- NA_character_
# df_elec_data_eskom$insulation_type <- NA_character_
# matches <- match(x = as.character(df_elec_data_eskom$stand_number), table = as.character(df_stnds_tps$stnds))
# if (any(!is.na(matches))) {
#   df_elec_data_eskom[which(!is.na(matches)), "energy_type"] <- as.character(df_stnds_tps[matches[!is.na(matches)], "e_tps"])
#   df_elec_data_eskom[which(!is.na(matches)), "insulation_type"] <- as.character(df_stnds_tps[matches[!is.na(matches)], "i_tps"])
# }

# ---------------------------------------- #
# QUICK VISUAL EXPLORATION

# ggplot(data = df_elec_data_eskom, mapping = aes(x = trx_dttm, y = svc_qty_kwh, group = stand_number, colour = stand_number)) + facet_wrap(facets = energy_type ~ insulation_type) + geom_line()
# 
# mindate <- as.POSIXct("2014-01-01 00:00:00")
# df_2014ff <- df_elec_data_eskom[which(df_elec_data_eskom$trx_dttm > mindate),]
# 
# ggplot(data = df_2014ff, mapping = aes(x = trx_dttm, y = svc_qty_kwh, group = stand_number, colour = stand_number)) + facet_wrap(facets = energy_type ~ insulation_type) + geom_line()

# ---------------------------------------- #
# SEE IF THERE ARE ANY MATCHES BETWEEN THE MYSTERIOUS STAND 1502'S MULTIPLE METER NUMBERS AND OUR STANDS' METER NUMBERS
# load(paste(rdadir, "df_stands_info.Rda", sep = ""))
# matches <- match(x = as.character(df_stands_info$meter_number), table = as.character(s1502$badge_meter_number))
# table(is.na(matches))
# # whaaat! there actually are 10 matches...
# awesomeness <- df_stands_info[which(!is.na(matches)),]

# ---------------------------------------- #
# HOW CAN THERE BE NEGATIVE TRANSACTION AMOUNTS AND SVC QUANTITIES??? 
# > summary(df_elec_data_eskom$trx_amt)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# -200.00   20.00   20.00   40.05   50.00  800.00    8347 
# > summary(df_elec_data_eskom$svc_qty_kwh)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -301.50   21.00   30.90   48.08   58.50  904.50

# Don't worry, these are from 2009 and we don't need that 2009 data.
# negatives <- df_elec_data_eskom[which(df_elec_data_eskom$amount < 0),]

# ---------------------------------------- #


# ---------------------------------------- #
# # check that the stand numbers in the data correspond to our finalised list of stand numbers. If not fix it where appropriate
# load(paste(rdadir, "stands_and_types.Rda", sep = ""))
# 
# matches <- match(x = as.character(stands_and_types$stand_number), table = as.character(df_elec_data_eskom$stand_number))
# matches <- match(x = as.character(df_elec_data_eskom$stand_number), table = as.character(stands_and_types$stand_number))
