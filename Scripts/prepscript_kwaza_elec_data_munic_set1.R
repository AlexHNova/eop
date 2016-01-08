# Date created: 
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# script to prepare the first set of electricity usage data of the stands in KwaZamokuhle, as provided by the municipality
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
file_path <- paste(datadir, "Electricity/KwaZa_elec_data_munic.xlsx", sep = "")
sheet_names <- getSheetNames(file_path)

lst_elec_data_munic <- vector(mode = "list", length = length(sheet_names))
lst_elec_data_munic <- apply(X = as.array(sheet_names), MARGIN = 1, FUN = function(x) {
  
  temp <- read.xlsx(xlsxFile = file_path, sheet = x, colNames = TRUE)   
  names(temp) <- fixname(names(temp))
  
  if ("sts_meter_no" %in% names(temp)) {
    temp$sts_meter_no <- as.character(temp$sts_meter_no)
  }
  
  if ("transaction_type" %in% names(temp)) {
    temp$transaction_type <- tolower(temp$transaction_type)
  }
  
  return(temp)
  
})

names(lst_elec_data_munic) <- sheet_names
names(lst_elec_data_munic) <- gsub(pattern = "[[:blank:]]{1,}", replacement = "", x = names(lst_elec_data_munic))
names(lst_elec_data_munic) <- gsub(pattern = "[[:punct:]]{1,}", replacement = "", x = names(lst_elec_data_munic))
names(lst_elec_data_munic) <- gsub(pattern = "^0{1,}", replacement = "", x = names(lst_elec_data_munic))
names(lst_elec_data_munic) <- gsub(pattern = "^127", replacement = "1027", x = names(lst_elec_data_munic))
names(lst_elec_data_munic) <- gsub(pattern = "17139359", replacement = "1027139359", x = names(lst_elec_data_munic), fixed = TRUE)
names(lst_elec_data_munic) <- gsub(pattern = "423082997", replacement = "4230820997", x = names(lst_elec_data_munic), fixed = TRUE)

rm(file_path, sheet_names)

# ---------------------------------------- #
# standardise the column names and types of the first sheet's data
## ------------------ ##
## extract it as meta data and remove it from the main list
df_temp <- lst_elec_data_munic[[1]]
lst_elec_data_munic[[1]] <- NULL
#lst_elec_data_munic <- lst_elec_data_munic[2:length(lst_elec_data_munic)]

## ------------------ ##
## fix the field names
names(df_temp) <- gsub(pattern = "respondent_address_standnumber", replacement = "stand_number", x = names(df_temp), fixed = TRUE)
names(df_temp) <- gsub(pattern = "prepaid_number", replacement = "meter_number", x = names(df_temp), fixed = TRUE)
names(df_temp) <- gsub(pattern = "x4", replacement = "ext", x = names(df_temp), fixed = TRUE)
names(df_temp) <- gsub(pattern = "respondent_name", replacement = "user", x = names(df_temp), fixed = TRUE)

## ------------------ ##
## remove unnecessary fields
df_temp <- remove.col(df = df_temp, colName = "x3")

## ------------------ ##
## format the stand_number and meter_number fields. NB! make sure there are no spaces, punctuation marks or leading zeros
df_temp$stand_number <- as.character(df_temp$stand_number)
df_temp$stand_number <- gsub(pattern = "[[:blank:]]", replacement = "", x = df_temp$stand_number)
df_temp$stand_number <- gsub(pattern = "[[:punct:]]", replacement = "", x = df_temp$stand_number)
df_temp$stand_number <- gsub(pattern = "^0{1,}", replacement = "", x = df_temp$stand_number)

df_temp$meter_number <- as.character(df_temp$meter_number)
df_temp$meter_number <- gsub(pattern = "[[:blank:]]", replacement = "", x = df_temp$meter_number)
df_temp$meter_number <- gsub(pattern = "[[:punct:]]", replacement = "", x = df_temp$meter_number)
df_temp$meter_number <- gsub(pattern = "^0{1,}", replacement = "", x = df_temp$meter_number)

## ------------------ ##
## format the 'user' field
df_temp$user <- tolower(as.character(df_temp$user))
df_temp$user <- gsub(pattern = "[[:blank:]]{2,}", replacement = "", x = df_temp$user)
df_temp$user <- gsub(pattern = "[[:punct:]]", replacement = "", x = df_temp$user)

## ------------------ ##
## make sure the 'ext' field is clean
df_temp$ext <- as.character(df_temp$ext)
df_temp$ext <- gsub(pattern = "[[:blank:]]", replacement = "", x = df_temp$ext)
df_temp$ext <- gsub(pattern = "[[:punct:]]", replacement = "", x = df_temp$ext)
df_temp$ext <- gsub(pattern = "^0{1,}", replacement = "", x = df_temp$ext)
df_temp$ext <- as.integer(df_temp$ext)

## ------------------ ##
df_meta_elec_data_munic <- df_temp
rm(df_temp)

# ---------------------------------------- #
# remove useless records from the meta data
df_meta_elec_data_munic <- df_meta_elec_data_munic[which(as.character(df_meta_elec_data_munic$meter_number) != ""),]

# ---------------------------------------- #
# remove useless fields from the meta data
df_meta_elec_data_munic <- remove.cols(df = df_meta_elec_data_munic, colNames = c("quantity", "eskom_council"))

# ---------------------------------------- #
# fix the error for meter 1027487329 in the meta data which currently displays as 10227487329
df_meta_elec_data_munic$meter_number <- gsub(pattern = "10227487329", replacement = "1027487329", x = df_meta_elec_data_munic$meter_number)

# fix the error for stand 370 - it's meter number was given to the munic okes as 1036620367, but it should have been 1026620367
df_meta_elec_data_munic$meter_number <- gsub(pattern = "1036620367", replacement = "1026620367", x = df_meta_elec_data_munic$meter_number)





# ---------------------------------------- #
# the meta data currently only has one meter number associated with each stand. There were, however, two stands that had two meter numbers associated with each of them. These are:
# 1027151883, 4230820997 were associated with the same stand (2859)
# 1027157864, 4197380613 were associated with the same stand (3466)
# fix this! so that those records do not go to waste later when we only store those that have a meter number match in the meta data...

idxx <- which(df_meta_elec_data_munic$stand_number %in% c("2859", "3466"))
temp <- df_meta_elec_data_munic[idxx,]
temp$meter_number <- gsub(pattern = "4230820997", replacement = "1027151883", x = temp$meter_number, fixed = TRUE)
temp$meter_number <- gsub(pattern = "4197380613", replacement = "1027157864", x = temp$meter_number, fixed = TRUE)

df_meta_elec_data_munic <- do.call("rbind", list(df_meta_elec_data_munic, temp))
rm(temp)

# ---------------------------------------- #
# ---------------------------------------- #
# meta data done, now return to formatting/processing the main list
# ---------------------------------------- #
# for later use, add the list name as a field to each item in the main list
lst_elec_data_munic <- apply(X = as.array(names(lst_elec_data_munic)), MARGIN = 1, FUN = function(nm) {
  ds <- lst_elec_data_munic[[nm]]
  ds$file_name <- nm 
  return(ds)
})

# ---------------------------------------- #
# now bind them all together in one b*g data frame 
df_elec_data_munic <- do.call("rbind", lst_elec_data_munic)
rm(lst_elec_data_munic)

# ---------------------------------------- #
# fix the problem of 'N/A''s in the 'sts_meter_no' column of each stand/item in the list - simply remove these records
idxx <- grep(pattern = "N/A", x = df_elec_data_munic$sts_meter_no, fixed = TRUE)
if (length(idxx) > 0) {
  df_elec_data_munic <- df_elec_data_munic[-idxx,]
}

# ---------------------------------------- #
# process the 'no purchases' occuring in the sts_meter_no field. NB, do not throw these records away, because we need to know if one of our stands did not buy any electricity.
idxx <- grep(pattern = "no purchases ", x = tolower(df_elec_data_munic$sts_meter_no), fixed = TRUE) ## notice the space at the end!
if (length(idxx) > 0) {
  ctch <- apply(X = as.array(idxx), MARGIN = 1, FUN = function(idx) {
    df_elec_data_munic[idx, "sts_meter_no"] <<- df_elec_data_munic[idx, "file_name"]
    df_elec_data_munic[idx, c("amount", "kwh_units")] <<- 0
  })
}

# ---------------------------------------- #
# remove unnecessary fields
df_elec_data_munic <- remove.col(df = df_elec_data_munic, colName = "file_name")

# ---------------------------------------- #
# rename some fields
names(df_elec_data_munic) <- gsub(pattern = "sts_meter_no", replacement = "meter_number", x = names(df_elec_data_munic), fixed = TRUE)
names(df_elec_data_munic) <- gsub(pattern = "transaction_date_time", replacement = "date", x = names(df_elec_data_munic), fixed = TRUE)

# ---------------------------------------- #
# correct the format of the date column
df_elec_data_munic$date <- as.POSIXct(x = (df_elec_data_munic$date *24*60*60), origin = "1900-01-01 00:00:00", tz = TIME_ZONE)

# ---------------------------------------- #
# make sure the meter_number field is clean
df_elec_data_munic$meter_number <- gsub(pattern = "[[:blank:]]", replacement = "", x = df_elec_data_munic$meter_number)
df_elec_data_munic$meter_number <- gsub(pattern = "[[:punct:]]", replacement = "", x = df_elec_data_munic$meter_number)
df_elec_data_munic$meter_number <- gsub(pattern = "^0{1,}", replacement = "", x = df_elec_data_munic$meter_number)

# ---------------------------------------- #
# make sure the transaction_type field is clean
df_elec_data_munic$transaction_type <- tolower(as.character(df_elec_data_munic$transaction_type))
df_elec_data_munic$transaction_type <- gsub(pattern = "[[:blank:]]{2,}", replacement = "", x = df_elec_data_munic$transaction_type)
df_elec_data_munic$transaction_type <- gsub(pattern = "[[:punct:]]", replacement = "", x = df_elec_data_munic$transaction_type)

# ---------------------------------------- #
# reorder the fields a bit
df_elec_data_munic <- move.cols(df = df_elec_data_munic, colNames = c("date", "transaction_type"), colIdxx = c(1,3))

# ---------------------------------------- #
# ---------------------------------------- #
# match the meta data to the rest of the data so that we can combine it all into one df
## first make sure that the names of the two dfs are compatible
names(df_meta_elec_data_munic) <- gsub(pattern = "ext", replacement = "extension", x = names(df_meta_elec_data_munic), fixed = TRUE)

## get the names of the columns to add
cols_to_add <- names(df_meta_elec_data_munic)[!(names(df_meta_elec_data_munic) %in% names(df_elec_data_munic))]

## find the records in df_elec_data_munic which has a meter_number match in df_meta_elec_data_munic
matches <- match(x = as.character(df_elec_data_munic$meter_number), table = as.character(df_meta_elec_data_munic$meter_number))

## match
ctch <- apply(X = as.array(cols_to_add), MARGIN = 1, FUN = function(c) {
  df_elec_data_munic[, c] <<- NA
  df_elec_data_munic[which(!is.na(matches)), c] <<- df_meta_elec_data_munic[matches[!is.na(matches)],c]
})
rm(ctch)

## the records in df_meta_elec_data_munic which have no meter_number match in df_elec_data_munic should simply removed, because it means that there is no electricity data whatsoever for those meters in df_elec_data_munic. Their absence does not say that they didn't buy any electricity.

rm(cols_to_add, df_meta_elec_data_munic, matches)

# ---------------------------------------- #
# factorise the stand_number field
df_elec_data_munic$stand_number <- as.factor(df_elec_data_munic$stand_number)

# ---------------------------------------- #
# put the columns in a better order and fix the row names
rownames(df_elec_data_munic) <- as.character(c(1:nrow(df_elec_data_munic)))

df_elec_data_munic <- move.cols(df = df_elec_data_munic, colNames = c("date", "stand_number", "extension", "meter_number", "user", "transaction_type", "amount", "kwh_units"), colIdxx = c(1:8))

# ---------------------------------------- #
# save the data
df_elec_data_munic_set1 <- df_elec_data_munic
rm(df_elec_data_munic)
archive(fileName = "df_elec_data_munic_set1.Rda", currentDir = rdadir, verbose = TRUE)
save(df_elec_data_munic_set1, file = paste(rdadir, "df_elec_data_munic_set1.Rda", sep = ""))

# clean up
rm(idxx)
# rubble <- c("cols_to_add", "idx", "idxx", "records_to_add")
# if (any(exists(rubble))) {
#   rm(rubble[exists(rubble)])
# }
# ---------------------------------------- #



# ---------------------------------------- #
# ---------------------------------------- #
# NOTES
# 1027162088 as prepaid number occurs twice in the meta data; so does 4114940747...

# There are two cases in which it seems that one stand had two meters - not at the same time, but as if one started to malfunction and then they replaced it with another one:
# 1027151883, 4230820997 were associated with the same stand
# 1027157864, 4197380613 were associated with the same stand

# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...

# df_elec_data_munic$corr <- df_elec_data_munic$sts_meter_no == df_elec_data_munic$name
# table(df_elec_data_munic$corr, exclude = NULL)
# 
# temp18 <- lst_elec_data_munic[[18]]
# names(lst_elec_data_munic)[[18]]


# matches <- match(x = as.character(df_meta_elec_data_munic$meter_number_number), table = as.character(x = df_elec_data_munic$sts_meter_no))
