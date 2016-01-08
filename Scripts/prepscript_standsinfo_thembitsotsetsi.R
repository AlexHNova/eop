# Date created: 6 October 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# script to prepare and combine the stand info of the test houses in KwaZamokuhle, using the lists provided by Thembi Tsotsetsi
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
filename_vector <- c("test_houses_coal.xlsx",
                     "test_houses_electricity.xlsx",
                     "test_houses_lpg.xlsx",
                     "test_houses_control.xlsx")

wrong_names <- c("stand_no", "respondent_address_standnumber", "name_of_respondent", "willing_to_take_part", "kitchen_king_demonstration", "demo_attended", "contact_no", "electricity_prepaid_no", "prepaid_no", "stove_no", "stove", "house_status_re_trombe_wall")

replacements <- c("stand_number", "stand_number", "respondent_name", "willing", "kk_demo", "demo", "contact_number", "prepaid_number", "prepaid_number", "stove_number", "stove_number", "trombe_wall")

lists <- apply(X = as.array(filename_vector), MARGIN = 1, FUN = function(f) {
  f <- paste(datadir, "Standnumbers/", f, sep = "")
  if (DEBUG) {message(f)}
  sheetnames <- as.array(getSheetNames(f))
  if (DEBUG) {message(sheetnames)}
  
  lists_list <- apply(X = as.array(1:length(sheetnames)), MARGIN = 1, FUN = function(idx) {
    shtnm <- sheetnames[idx]
    lst <- read.xlsx(xlsxFile = f, sheet = shtnm, colNames = TRUE)
    
    # standardise the column names in lst (which is a df)
    if (!is.null(lst)) {
      names(lst) <- fixname(names(lst))
      matches <- match(x = names(lst), table = wrong_names)
      if (any(!is.na(matches))) {
        names(lst)[which(!is.na(matches))] <- replacements[na.omit(matches)]
      }
    }
    
    # return
    return(lst)
  })    
  
  # remove empty sheets
  nulls <- sapply(X = lists_list, FUN = is.null)
  lists_list <- lists_list[!nulls]
  
  # standardise the names of lists_list
  names(lists_list) <- sheetnames[!nulls]
  names(lists_list) <- fixname(names(lists_list))
  names(lists_list) <- gsub(pattern = "ceiling", replacement = "basic", x = names(lists_list))
  names(lists_list) <- gsub(pattern = "stove", replacement = "", x = names(lists_list))
  
  # add a 'type' column to each of the dataframes in lists_list
  for (i in 1:length(lists_list)) {
    lists_list[[i]]$type <- names(lists_list)[i]
  }
  
  return(lists_list) 
})

# ---------------------------------------- #
# now put them all together in one biiig data frame...
info_stands <- rbind.fill(lapply(X = lists, FUN = function(lst) {
  return(rbind.fill(lst))
}))

rm(lists)

# ---------------------------------------- #
# format the contents of all the columns 
for (cidx in 1:ncol(info_stands)) {
  if (is.character(info_stands[, cidx])) {
    info_stands[, cidx] <- tolower(info_stands[, cidx])
  }
}

# ---------------------------------------- #
# split the stand_number field into two fields - one for stand nr, one for extension
info_stands$stand_number <- gsub(pattern = "[[:space:]]", replacement = "", x = info_stands$stand_number)
info_stands$extension <- NA_integer_

for (r in 1:nrow(info_stands)) {
  if (is.na(info_stands[r, "stand_number"])) {next}
  splits <- as.array((strsplit(x = info_stands[r, "stand_number"], split = "ext", fixed = TRUE))[[1]])
  if (DEBUG) {print(splits)}
  if (length(splits) < 2) {next}
  info_stands[r, "extension"] <- gsub(pattern = "[[:punct:]]", replacement = "", x = splits[[2]])
  info_stands[r, "stand_number"] <- splits[[1]]
}

# fix the record for stand 378
idx <- match(x = "378et3", table = info_stands$stand_number)
if (!is.na(idx)) {
  info_stands[idx, "stand_number"] <- "378"
  info_stands[idx, "extension"] <- "3"
}

info_stands$stand_number <- as.factor(info_stands$stand_number)

# ---------------------------------------- #
# split the 'type' column into two fields - energy_type and insulation_type
info_stands$energy_type <- NA_character_
info_stands$insulation_type <- NA_character_

for (r in 1:nrow(info_stands)) {
  if (is.na(info_stands[r, "type"])) {next}
  splits <- as.array((strsplit(x = info_stands[r, "type"], split = "_", fixed = TRUE))[[1]])
  if (DEBUG) {print(splits)}
  if (length(splits) < 2) {next}
  info_stands[r, "insulation_type"] <- splits[[1]]
  info_stands[r, "energy_type"] <- splits[[2]]
  info_stands[r, "energy_type"] <- gsub(pattern = "gas", replacement = "lpg", x = info_stands[r, "energy_type"], fixed = TRUE)
  info_stands[r, "energy_type"] <- gsub(pattern = "electricity", replacement = "elec", x = info_stands[r, "energy_type"], fixed = TRUE)
}

if (any(is.na(info_stands$insulation_type)) | any(is.na(info_stands$energy_type))) {
  warning("Records detected with missing insulation type or energy type.")
} else {
    info_stands$energy_type <- as.factor(info_stands$energy_type)
    info_stands$insulation_type <- as.factor(info_stands$insulation_type)
    
    idx <- match("type", table = names(info_stands))
    if (!is.na(idx)) {
      info_stands <- info_stands[, -idx]
    }
}
# ---------------------------------------- #
# correct the 'energy_type' error for stand 2881
idx <- match(x = "2881", table = as.character(info_stands$stand_number))
if (!is.na(idx)) {
  info_stands[idx, "energy_type"] <- "lpg"
}

# correct the 'energy_type' error for stand 2859
idx <- match(x = "2859", table = as.character(info_stands$stand_number))
if (!is.na(idx)) {
  info_stands[idx, "energy_type"] <- "elec"
}

# ---------------------------------------- #
# remove the duplicate records for stand 3927 and 3330
#df_stands_info[duplicated(as.character(df_stands_info$stand_number)), "stand_number"] 

idxx <- grep(pattern = "3927", x = as.character(info_stands$stand_number), fixed = TRUE)
if (length(idxx) > 1) {
  info_stands <- info_stands[-idxx[[2]],]
}

idx <- which(as.character(info_stands$stand_number) == "3330" & as.character(info_stands$willing) == "not at home")
if (length(idx) > 0) {
  info_stands <- info_stands[-idx,]
}

# ---------------------------------------- #
# fix the record for stand 370 - its meter_number currently displays as 1036620367, but should be 1026620367
idx <- which(as.character(info_stands$stand_number) == "370")
if (!is.na(idx)) {
  info_stands[idx, c("prepaid_number")] <- "1026620367"
}

# ---------------------------------------- #
# combine the prepaid_number and meter_no fields, because they are exactly the same
info_stands$prepaid_number <- gsub(pattern = "^0", replacement = "", x = info_stands$prepaid_number)
info_stands$prepaid_number <- gsub(pattern = "^[[:punct:]]{1}", replacement = "", x = info_stands$prepaid_number)

info_stands$meter_no <- gsub(pattern = "^0", replacement = "", x = info_stands$meter_no)
info_stands$meter_no <- gsub(pattern = "^[[:punct:]]{1}", replacement = "", x = info_stands$meter_no)

info_stands$meter_number <- NA_character_
for (r in 1:nrow(info_stands)) {
  if (DEBUG) {message("r = ",r)}
  temp <- c(info_stands[r, "prepaid_number"], info_stands[r, "meter_no"])
  if (any(!is.na(temp))) {
    info_stands[r, "meter_number"] <- temp[!is.na(temp)]
  }
}

## -------------------- #
## remove them olde fields
idxx <- match(x = c("prepaid_number", "meter_no"), table = names(info_stands))
if (any(!is.na(idxx))) {
  info_stands <- info_stands[, - idxx[!is.na(idxx)]]
}

## -------------------- #
## take some more zeros away in frot
info_stands$meter_number <- gsub(pattern = "^0{1,}", replacement = "", x = info_stands$meter_number)

## -------------------- #
## fix the error for 104183267980
idxx <- grep(pattern = "104183267980", x = info_stands$meter_number, fixed = TRUE)
if (length(idxx) > 0) {
  info_stands[idxx, "meter_number"] <- "4183267980"
}

## -------------------- #
## fix the error for 415666654
## ???

# ---------------------------------------- #
# put them columns in a better order
first_cols <- c("stand_number", "extension", "energy_type", "insulation_type")
idxx_firsts <- match(x = first_cols, table = names(info_stands))
 
if (any(!is.na(idxx_firsts))) {
  other_cols <- names(info_stands)[-na.omit(idxx_firsts)]
  idxx_others <- match(x = other_cols, table = names(info_stands))
  info_stands <- info_stands[, c(na.omit(idxx_firsts), na.omit(idxx_others))]
}

# ---------------------------------------- #
# now update this data with a more recent stand_number-meter_number list that Thembi sent through
updatedInfo <- read.xlsx(xlsxFile = paste(datadir, "Electricity/basic stand no info 280102015.xlsx", sep = ""), sheet = 1, colNames = TRUE)
updatedInfo$stand_number <- as.character(updatedInfo$stand_number)
updatedInfo$meter_number <- as.character(updatedInfo$meter_number)

isNa <- which(is.na(info_stands$meter_number))
temp <- info_stands[isNa,]
info_stands <- info_stands[-isNa,]
rm(isNa)

matches <- match(x = temp$stand_number, table = updatedInfo$stand_number)
temp[which(!is.na(matches)), "meter_number"] <- updatedInfo[matches[!is.na(matches)], "meter_number"]

temp$meter_number <- gsub(pattern = "not at home", replacement = NA, x = temp$meter_number)

info_stands <- do.call("rbind", list(info_stands, temp))
rm(temp, updatedInfo, matches)

# ---------------------------------------- #
# save them data
df_stands_info <- info_stands
rm(info_stands)
save(df_stands_info, file = paste(rdadir, "df_stands_info.Rda", sep = ""))

# write it to csv for people to play with
write.csv(x = df_stands_info, file = paste(outputdir, "df_stands_info.csv", sep = ""))

# clean up
rm(cidx, filename_vector, first_cols, idx, idxx, idxx_firsts, idxx_others, r, replacements, splits, wrong_names)
# ---------------------------------------- #




# ---------------------------------------- #
# ---------------------------------------- #
# DOODLE CODE...

# table(duplicated(df_stands_info$stand_number))
# table(duplicated(df_stands_info$meter_number))
# df_stands_info[duplicated(df_stands_info$meter_number), "meter_number"]








