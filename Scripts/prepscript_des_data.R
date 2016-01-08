# Script to prepare the kwaZamokuhle Detailed Energy Survey (DES) data from BEFORE the intervention
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
PRE1 <- FALSE
PRE2 <- FALSE
POST <- TRUE
# ---------------------------------------- #
# ---------------------------------------- #

# read the data
if (PRE1) {fileDir <- paste(datadir, "DES/Pre/Kwazamokuhle_Detailed_Energy_Survey__20151116-101513/", sep = "")}
if (PRE2) {fileDir <- paste(datadir, "DES/huh/Kwazamokuhle_Detailed_Energy_Survey_2015_20151118-102227/", sep = "")}
if (POST) {fileDir <- paste(datadir, "DES/Post/Kwazamokuhle_Detailed_Energy_Survey_2015_POST_20151116-102501/", sep = "")}

files <- dir(path = fileDir, full.names = FALSE)

des_by_section <- apply(X = as.array(files), MARGIN = 1, FUN = function(fnm) {
  section_df <- read.csv(file = paste(fileDir, fnm, sep = ""), header = TRUE, sep = ",", stringsAsFactors = FALSE)
  names(section_df) <- fixname(names(section_df))
  names(section_df) <- gsub(pattern = "ï_submission_id", replacement = "submission_id", x = names(section_df), fixed = TRUE)
  
  return(section_df)
})

names(des_by_section) <- files
names(des_by_section) <- gsub(pattern = ".csv", replacement = "", x = names(des_by_section), fixed = TRUE)
names(des_by_section) <- gsub(pattern = "[[:digit:]]", replacement = "", x = names(des_by_section))
names(des_by_section) <- fixname(names(des_by_section))
rm(files, fileDir)

# ---------------------------------------- #
# remove unnecessary list items
idxx <- which(names(des_by_section) %in% c("questions", "coal_ignition_time_winter", "coal_ignition_time_summer", "house"))
if (length(idxx) > 0) {
  des_by_section <- des_by_section[-idxx]
}

# ---------------------------------------- #
# remove unnecessary fields from each of the sections
for (idx in 2:length(des_by_section)) {
    des_by_section[[idx]] <- remove.cols(df = des_by_section[[idx]], colNames = c("fieldworker_name", "fieldworker_id", "received"))
}

# ---------------------------------------- #
# extract and format the code_book
codeBook <- des_by_section[["code_book"]]
idx <- match(x = "code_book", table = names(des_by_section))
if (!is.na(idx)) {
  des_by_section <- des_by_section[-idx]
}

names(codeBook) <- gsub(pattern = "ï_question", replacement = "question", x = names(codeBook), fixed = TRUE)

for (c in 1:ncol(codeBook)) {
  if (is.character(codeBook[[c]])) {
    codeBook[[c]] <- format.char(codeBook[[c]])
  }
}

# fix the label field
idxx <- grep(pattern = "y_", x = codeBook$label, fixed = TRUE)
codeBook[idxx, "label"] <- "yes"
idxx <- grep(pattern = "n_", x = codeBook$label, fixed = TRUE)
codeBook[idxx, "label"] <- "no"

# ---------------------------------------- #
# extract and format the metadata (the "household_identification_and_selection" and "submissions" sections)
# put the two sets together
metaInfoA <- des_by_section[["household_identification_and_selection"]]
metaInfoA <- remove.col(df = metaInfoA, colName = c("received"))
metaInfoB <- des_by_section[["submissions"]]
metaInfoB <- remove.cols(df = metaInfoB, colNames = c("device", "start", "duration_seconds", "language", "modified_by", "modified_on"))
metaInfo <- cbind(metaInfoA, metaInfoB)
rm(metaInfoA, metaInfoB)

# remove the second occurence of the "submission id" field due to the cbind action
idxx <- grep(pattern = "submission_id", x = names(metaInfo), fixed = TRUE)
idxx <- idxx[[2]]
metaInfo <- remove.col(df = metaInfo, colIdx = idxx)
rm(idxx)

wrongNames <- c("end", 
                "respondent_location_standnumber", 
                "respondent_qualification_wiling", 
                "respondent_concent_sober",
                "respondent_resident_9month")

rightNames <- c("date", 
                "stand_number", 
                "willing", 
                "sober",
                "resident_9months")

idxx <- match(x = names(metaInfo), table = wrongNames)
if (any(!is.na(idxx))) {
  names(metaInfo)[which(!is.na(idxx))] <- rightNames[idxx[!is.na(idxx)]]
}

rm(wrongNames, rightNames)

for (c in 1:ncol(metaInfo)) {
  if (is.character(metaInfo[[c]])) {
    metaInfo[[c]] <- format.char(metaInfo[[c]])
  }
}

metaInfo$date <- strptime(x = metaInfo$date, format = "%e_%m_%Y_%H_%M_%S", tz = TIME_ZONE)
metaInfo$date <- as.POSIXct(metaInfo$date, tz = TIME_ZONE)

metaInfo$stand_number <- format.int.char(metaInfo$stand_number)
metaInfo$survey_version <- format.int.char(metaInfo$survey_version)

# change the binary fields to yes/no fields
yesNoFields <- c("sober", "resident_9months", "agreement_to_continue")
for (c in 1:length(yesNoFields)) {
  fldnm <- yesNoFields[c]
  metaInfo[[fldnm]] <- format.yesno(field = metaInfo[[fldnm]], yes = 1, no = 2)
}

rm(yesNoFields)

# remove all the records that have something else than '1' (yes) in the 'willing' field
metaInfo$willing <- as.integer(metaInfo$willing)
metaInfo <- metaInfo[which(metaInfo$willing == 1),]
metaInfo <- remove.col(df = metaInfo, colName = "willing")

# remove all the records of the drunk respondents
metaInfo <- metaInfo[which(metaInfo$sober == "yes"),]
metaInfo <- remove.col(df = metaInfo, colName = "sober")

# remove all the records of the respondents who did not give permission to continue
metaInfo <- metaInfo[which(metaInfo$agreement_to_continue == "yes"),]
metaInfo <- remove.col(df = metaInfo, colName = "agreement_to_continue")

# remove the records of the respondents who have not been staying in Kwaza for at least nine months???
# no

# convert the numbers in the place field to words
dfPlace <- codeBook[which(codeBook$variable == "place"), c("variable", "value", "label")]
metaInfo$place <- format.int.char(metaInfo$place)
matches <- match(x = metaInfo$place, table = dfPlace$value)
if (any(!is.na(matches))) {
  metaInfo[which(!is.na(matches)), "place"] <- dfPlace[matches[!is.na(matches)], "label"]
}
rm(dfPlace, matches)

# format the respondent_age field
metaInfo$respondent_age <- format.int.char(metaInfo$respondent_age)

# reorder the fields
metaInfo <- move.cols(df = metaInfo, 
                      colNames = c("submission_id", "date", "stand_number", "latitude", "longitude", "place", "resident_9months", "respondent_age", "survey_version"), colIdxx = c(1:9))

# put the metaInfo df back into the des_by_section list, replacing the "household_identification_and_selection" section and removing the "submissions" section
des_by_section[["household_identification_and_selection"]] <- metaInfo
des_by_section <- des_by_section[- which(names(des_by_section) == "submissions")]
names(des_by_section)[which(names(des_by_section) == "household_identification_and_selection")] <- "meta_info"

# ---------------------------------------- #
# do standard formatting across all the other sections and remove the submissions that no longer have records in our metaInfo
for (idx in 2:length(des_by_section)) {
  sectionDf <- des_by_section[[idx]]
  
  for (c in 1:ncol(sectionDf)) {
    if (is.character(sectionDf[[c]])) {
      sectionDf[[c]] <- format.char(sectionDf[[c]])
    }
  }
  
  matches <- match(x = sectionDf$submission_id, table = metaInfo$submission_id)
  if (any(!is.na(matches))) {
    sectionDf <- sectionDf[which(!is.na(matches)),]
  } else {sectionDf <- NULL}
  
  des_by_section[[idx]] <- sectionDf
  rm(sectionDf)
}

# ---------------------------------------- #
# replace the integer values in all fields with their corresponding word labels from the code book
codeBook$variable <- fixname(codeBook$variable)
codeBook_by_var <- split(x = codeBook, f = codeBook$variable)

des_by_section <- lapply(X = des_by_section, FUN = function(sdf) {
  
  for (c in 1:ncol(sdf)) {
    if (!is.integer(sdf[[c]])) {next}
    
    idx <- match(x = names(sdf)[c], table = names(codeBook_by_var))
    if (is.na(idx)) {
      warning("variable encountered with no match in code book: ", names(sdf)[c])
      next
    }
    
    vardf <- codeBook_by_var[[idx]]
    matches <- match(x = sdf[[c]], table = vardf$value)
    if (all(is.na(matches))){
      warning("No matches found for the contents of variable ", names(sdf)[c], " in code book.")
      next
    }
    
    sdf[which(!is.na(matches)), c] <- vardf[matches[!is.na(matches)], "label"]
    rm(idx, vardf, matches)
  }
  
  return(sdf)
})

# ---------------------------------------- #
# put them all together in one big data frame and rm des_by_section
df_des_data <- des_by_section[[1]]
for (i in 2:length(des_by_section)) {
  df_des_data <- merge(x = df_des_data, y = des_by_section[[i]], by = "submission_id")
}


# ---------------------------------------- #
# fix the costs fields, if any
costFields <- grep(pattern = "energy_cost", x = names(df_des_data), fixed = TRUE)

if (length(costFields) > 0) {
  
  ctch <- apply(X = as.array(costFields), MARGIN = 1, FUN = function(fidx) {
    field <- df_des_data[[fidx]]
    field[which(is.na(field))] <- "r0_r0" # the field will be any if they previously indicated that they do not use this specific carrier, so we can change that to an unambiguous indicator of R0.00 expenditure per month
    field <- gsub(pattern = "no", replacement = NA, x = field, fixed = TRUE) # the field will be 'no' if the respondent declined to answer, so we can change that to NA, indicating uncertain.
    
    fieldsplits <- strsplit(x = field, split = "_", fixed = TRUE)
    fieldMin <- sapply(X = fieldsplits, FUN = function(ee) {return(ee[[1]])})
    fieldMax <- sapply(X = fieldsplits, FUN = function(ee) {
      if (length(ee) > 1) { return(ee[[2]]) }
      return(NA)
    })
    fieldMin <- format.num(fieldMin)
    fieldMax <- format.num(fieldMax)
    fieldAvg <- apply(X = as.array(1:length(fieldMin)), MARGIN = 1, FUN = function(idx) {
      if (any(is.na(c(fieldMin[[idx]], fieldMax[[idx]])))) {
        return(NA)
      }
      return((fieldMin[[idx]] + fieldMax[[idx]])/2)
    })
    df_des_data[[fidx]] <<- fieldAvg
    rm(field, fieldsplits, fieldMin, fieldMax, fieldAvg)
  })
  
  rm(ctch)
}


# ---------------------------------------- #
# make the consumption and cost values zero for the (seasonal) energy carriers that are not used by each household
# for LPG
idxx <- which(df_des_data$energy_lpg_use == "no")
df_des_data[idxx, c("energy_lpg_comsumption_kgwintermonth", "energy_lpg_comsumption_kgsummermonth")] <- 0.0
if ("energy_cost_winter_gas" %in% names(df_des_data)) {
  df_des_data[idxx, c("energy_cost_summer_gas", "energy_cost_winter_gas")] <- 0.0
}

idxx <- which(df_des_data$energy_lpg_seasonal_winter == "no")
df_des_data[idxx, "energy_lpg_comsumption_kgwintermonth"] <- 0.0 
if ("energy_cost_winter_gas" %in% names(df_des_data)) {
  df_des_data[idxx, c("energy_cost_summer_gas")] <- 0.0
}

idxx <- which(df_des_data$energy_lpg_seasonal_summer == "no")
df_des_data[idxx, "energy_lpg_comsumption_kgsummermonth"] <- 0.0 
if ("energy_cost_summer_gas" %in% names(df_des_data)) {
  df_des_data[idxx, c("energy_cost_winter_gas")] <- 0.0
}

# ------------------ #
# for COAL
idxx <- which(df_des_data$energy_coal_use == "no")
df_des_data[idxx, c("energy_coal_consumption_wintercurrentunits", "energy_coal_consumption_summercurrentunits")] <- 0.0
if ("energy_cost_winter_coal" %in% names(df_des_data)) {
  df_des_data[idxx, c("energy_cost_summer_coal", "energy_cost_winter_coal")] <- 0.0
}

idxx <- which(df_des_data$energy_coal_seasonal_winter == "no")
df_des_data[idxx, "energy_coal_consumption_wintercurrentunits"] <- 0.0 
if ("energy_cost_winter_coal" %in% names(df_des_data)) {
  df_des_data[idxx, c("energy_cost_winter_coal")] <- 0.0
}

idxx <- which(df_des_data$energy_coal_seasonal_summer == "no")
df_des_data[idxx, "energy_coal_consumption_summercurrentunits"] <- 0.0 
if ("energy_cost_summer_coal" %in% names(df_des_data)) {
  df_des_data[idxx, c("energy_cost_summer_coal")] <- 0.0
}

# ------------------ #
# for WOOD
idxx <- which(df_des_data$energy_wood_use == "no")
df_des_data[idxx, c("energy_wood_consumption_wintercurrentunits", "energy_wood_consumption_summercurrentunits")] <- 0.0
if ("energy_cost_winter_wood" %in% names(df_des_data)) {
  df_des_data[idxx, c("energy_cost_winter_wood", "energy_cost_summer_wood")] <- 0.0
}

idxx <- which(df_des_data$energy_wood_seasonal_winter == "no")
df_des_data[idxx, "energy_wood_consumption_wintercurrentunits"] <- 0.0 
if ("energy_cost_winter_wood" %in% names(df_des_data)) {
  df_des_data[idxx, c("energy_cost_winter_wood")] <- 0.0
}

idxx <- which(df_des_data$energy_wood_seasonal_summer == "no")
df_des_data[idxx, "energy_wood_consumption_summercurrentunits"] <- 0.0 
if ("energy_cost_summer_wood" %in% names(df_des_data)) {
  df_des_data[idxx, c("energy_cost_summer_wood")] <- 0.0
}

# ------------------ #
# for ELECTRICITY
if ("energy_cost_summer_electricity" %in% names(df_des_data)) {
  df_des_data[which(df_des_data$energy_electricity_use == "no"), c("energy_cost_winter_electricity", "energy_cost_summer_electricity")] <- 0.0
}

# ------------------ #
# for PARAFFIN
idxx <- which(df_des_data$energy_paraffin_use == "no")
df_des_data[idxx, c("energy_paraffin_comsumption_litreswinterweek", "energy_paraffin_comsumption_litressummerweek")] <- 0.0
if ("energy_cost_winter_paraffin" %in% names(df_des_data)) {
  df_des_data[idxx, c("energy_cost_winter_paraffin", "energy_cost_summer_paraffin")] <- 0.0
}

idxx <- which(df_des_data$energy_paraffin_seasonal_winter == "no")
df_des_data[idxx, "energy_paraffin_comsumption_litreswinterweek"] <- 0.0 
if ("energy_cost_winter_paraffin" %in% names(df_des_data)) {
  df_des_data[idxx, c("energy_cost_winter_paraffin")] <- 0.0
}

idxx <- which(df_des_data$energy_paraffin_seasonal_summer == "no")
df_des_data[idxx, "energy_paraffin_comsumption_litressummerweek"] <- 0.0 
if ("energy_cost_summer_paraffin" %in% names(df_des_data)) {
  df_des_data[idxx, c("energy_cost_summer_paraffin")] <- 0.0
}

# ------------------ #
# for DUNG
df_des_data[which(df_des_data$energy_dung_use == "no"), c("energy_dung_volumne")] <- 0.0

# ---------------------------------------- #
# remove fields which are not needed for now (but might be needed later)
timeFields <- grep(pattern = "time", x = names(df_des_data), fixed = TRUE, value = TRUE)
df_des_data <- remove.cols(df = df_des_data, colNames = timeFields)
repeatFields <- grep(pattern = "repeat", x = names(df_des_data), fixed = TRUE, value = TRUE)
df_des_data <- remove.cols(df = df_des_data, colNames = repeatFields)
rm(timeFields, repeatFields)

## now remove the duplicate records, caused by the above looping fields
## nadaify the fields so that NAs will be detected by the 'duplicated' function
for (c in 1:ncol(df_des_data)) {
  df_des_data[[c]] <- nada(df_des_data[[c]])
}

df_des_data <- df_des_data[which(!duplicated(df_des_data)),]

# ---------------------------------------- #
# fix other errors
if (POST) {
  df_des_data[which(df_des_data$submission_id == "466ce10f_1841_4b27_89bd_30c81a1c0dd9"), "respondent_name"] <- "anna_nkosi"
  df_des_data[which(df_des_data$submission_id == "195400b8_e7bc_4a48_8953_269d6ba0571a"), "respondent_name"] <- "elizabeth_sukwini"
  df_des_data[which(df_des_data$submission_id == "14e031be_70f1_4eba_bdd0_4d27058261f9"), "respondent_phone"] <- "762958353"
  df_des_data[which(df_des_data$submission_id == "f404e72b_4c55_4ee7_ba42_3abee98677c6"), "place"] <- "kwazamokuhle_sp"
  df_des_data[which(df_des_data$submission_id == "5f675a5f_de83_4ae2_b155_f808e9843013"), "respondent_name"] <- "anna_nkosi"
  df_des_data <- df_des_data[which(!(df_des_data$submission_id %in% c("466ce10f_1841_4b27_89bd_30c81a1c0dd9", "cd1cfec9_5eef_4e5a_87cb_10777b7b2336"))),]
}

if (PRE2) {
  df_des_data <- df_des_data[which(df_des_data$submission_id != "39a0b411_f907_472f_9d4e_8c29c50a3a3c"),]
}

# ---------------------------------------- #
# remove training records
idxx <- which(df_des_data$respondent_name %in% df_des_data$fieldworker_name)
if (length(idxx) > 0) {
  df_des_data <- df_des_data[-idxx,]
}

if (PRE1) {
  df_des_data <- df_des_data[which(df_des_data$date > "2014-09-30 23:59:59"),]
}

# ---------------------------------------- #
# identify different submissions for the same household, and keep only the most recent or the most complete submission (see note on 24 Nov 2015 in Notes file)
# two submissions will be regarded as belonging to the same household if:
# the stand numbers are the same and 3 of the following 5 are the same:
# household_structure_number, place, household_surname, respondent_name, contact.

for (k in 1:(nrow(df_des_data) - 1)) {
  for (m in (k+1):nrow(df_des_data)) {
    areSimilar <- matching(record1 = df_des_data[k,], 
                           record2 = df_des_data[m,], 
                           fieldsToMatch = c("stand_number", 
                                             "household_structure_number", 
                                             "place", 
                                             "respondent_name", 
                                             "respondent_household_surname", 
                                             "respondent_phone"), 
                           numRequiredMatches = 4)
    if (!areSimilar) {next}
    
    # reaching this point means the records are similar and thus in all probability belonging to the same household, so either keep the most complete or the most recent one and discard the other (we will essentially be replacing the less complete record with an exact duplicate of the more complete record, so these duplicates will have to be removed later)
    completeness_rec1 <- apply(X = df_des_data[k,], MARGIN = 1, FUN = is.na)
    completeness_rec1 <- (length(completeness_rec1[!completeness_rec1]) / length(completeness_rec1)) * 100
    completeness_rec2 <- apply(X = df_des_data[m,], MARGIN = 1, FUN = is.na)
    completeness_rec2 <- (length(completeness_rec2[!completeness_rec2]) / length(completeness_rec2)) * 100
    
    # if neither of the records are more recent than the other, choose the most complete one
    if (df_des_data[k, "date"] == df_des_data[m, "date"]) {
      if (completeness_rec1 > completeness_rec2) {
        df_des_data[m,] <- df_des_data[k,]
        message("Replacing record ", df_des_data[m, "submission_id"], " with record ", df_des_data[k, "submission_id"], ".")
        next
      }
      if (completeness_rec2 > completeness_rec1) {
        df_des_data[k,] <- df_des_data[m,]
        message("Replacing record ", df_des_data[k, "submission_id"], " with record ", df_des_data[m, "submission_id"], ".")
        next
      }
      
      # reaching this point means that both are equally complete, so simply choose the first one
      df_des_data[m,] <- df_des_data[k,]
      message("Replacing record ", df_des_data[m, "submission_id"], " with record ", df_des_data[k, "submission_id"], ".")
      next
    }
    
    # reaching this point, means the dates do differ, so choose the most recent one
    maxDate <- max(df_des_data[k, "date"], df_des_data[m, "date"], na.rm = TRUE)
    if (df_des_data[m, "date"] == maxDate) {
      df_des_data[k,] <- df_des_data[m,]
      message("Replacing record ", df_des_data[k, "submission_id"], " with record ", df_des_data[m, "submission_id"], ".")
      next
    } 
    df_des_data[m,] <- df_des_data[k,]
    message("Replacing record ", df_des_data[m, "submission_id"], " with record ", df_des_data[k, "submission_id"], ".")
  }
}

# now remove the duplicates caused by the above process
for (c in 1:ncol(df_des_data)) {
  df_des_data[[c]] <- nada(df_des_data[[c]])
}
df_des_data <- df_des_data[!duplicated(df_des_data),]

# ---------------------------------------- #
# reorder the fields
respondentDetailFields <- names(des_by_section[["respondent_details"]])
respondentDetailFields <- respondentDetailFields[which(respondentDetailFields != "submission_id")]
df_des_data <- move.cols(df = df_des_data, 
                         colNames = respondentDetailFields, 
                         colIdxx = c(7:(length(respondentDetailFields) + 6)))

df_des_data <- move.cols(df = df_des_data, colNames = c("submission_id", "stand_number", "household_structure_number", "latitude", "longitude", "place", "date", "respondent_name", "respondent_household_surname", "fieldworker_name", "fieldworker_id"), colIdxx = c(1:11))

# ---------------------------------------- #
# save
rm(metaInfo, codeBook, codeBook_by_var)

if (PRE1) {
  archive(fileName = "des_data_pre_1.Rda", currentDir = rdadir, verbose = TRUE)
  df_des_data_pre_1 <- df_des_data
  rm(df_des_data, des_by_section)
  save(df_des_data_pre_1, file = paste(rdadir,"des_data_pre_1.Rda", sep = ""))
}

if (PRE2) {
  archive(fileName = "des_data_pre_2.Rda", currentDir = rdadir, verbose = TRUE)
  df_des_data_pre_2 <- df_des_data
  rm(df_des_data, des_by_section)
  save(df_des_data_pre_2, file = paste(rdadir,"des_data_pre_2.Rda", sep = ""))
}

if (POST) {
  archive(fileName = "des_data_post.Rda", currentDir = rdadir, verbose = TRUE)
  df_des_data_post <- df_des_data
  rm(df_des_data, des_by_section)
  save(df_des_data_post, file = paste(rdadir,"des_data_post.Rda", sep = ""))
}

# ---------------------------------------- #
# ---------------------------------------- #
# CODE TO COMBINE THE TWO PRE-DESs
if (PRE2) {
  load(paste(rdadir, "des_data_pre_1.Rda", sep = ""))
  load(paste(rdadir, "des_data_pre_2.Rda", sep = ""))
  
  df_des_data_pre <- rbind.fill(df_des_data_pre_1, df_des_data_pre_2)
  rm(df_des_data_pre_1, df_des_data_pre_2)
  
  df_des_data <- df_des_data_pre
  
  for (k in 1:(nrow(df_des_data) - 1)) {
    for (m in (k+1):nrow(df_des_data)) {
      areSimilar <- matching(record1 = df_des_data[k,], 
                             record2 = df_des_data[m,], 
                             fieldsToMatch = c("stand_number", 
                                               "household_structure_number", 
                                               "place", 
                                               "respondent_name", 
                                               "respondent_household_surname", 
                                               "respondent_phone"), 
                             numRequiredMatches = 4)
      if (!areSimilar) {next}
      
      # reaching this point means the records are similar and thus in all probability belonging to the same household, so either keep the most complete or the most recent one and discard the other (we will essentially be replacing the less complete record with an exact duplicate of the more complete record, so these duplicates will have to be removed later)
      completeness_rec1 <- apply(X = df_des_data[k,], MARGIN = 1, FUN = is.na)
      completeness_rec1 <- (length(completeness_rec1[!completeness_rec1]) / length(completeness_rec1)) * 100
      completeness_rec2 <- apply(X = df_des_data[m,], MARGIN = 1, FUN = is.na)
      completeness_rec2 <- (length(completeness_rec2[!completeness_rec2]) / length(completeness_rec2)) * 100
      
      # if neither of the records are more recent than the other, choose the most complete one
      if (df_des_data[k, "date"] == df_des_data[m, "date"]) {
        if (completeness_rec1 > completeness_rec2) {
          df_des_data[m,] <- df_des_data[k,]
          message("Replacing record ", df_des_data[m, "submission_id"], " with record ", df_des_data[k, "submission_id"], ".")
          next
        }
        if (completeness_rec2 > completeness_rec1) {
          df_des_data[k,] <- df_des_data[m,]
          message("Replacing record ", df_des_data[k, "submission_id"], " with record ", df_des_data[m, "submission_id"], ".")
          next
        }
        
        # reaching this point means that both are equally complete, so simply choose the first one
        df_des_data[m,] <- df_des_data[k,]
        message("Replacing record ", df_des_data[m, "submission_id"], " with record ", df_des_data[k, "submission_id"], ".")
        next
      }
      
      # reaching this point, means the dates do differ, so choose the most recent one
      maxDate <- max(df_des_data[k, "date"], df_des_data[m, "date"], na.rm = TRUE)
      if (df_des_data[m, "date"] == maxDate) {
        df_des_data[k,] <- df_des_data[m,]
        message("Replacing record ", df_des_data[k, "submission_id"], " with record ", df_des_data[m, "submission_id"], ".")
        next
      } 
      df_des_data[m,] <- df_des_data[k,]
      message("Replacing record ", df_des_data[m, "submission_id"], " with record ", df_des_data[k, "submission_id"], ".")
    }
  }
  
  # now remove the duplicates caused by the above process
  for (c in 1:ncol(df_des_data)) {
    df_des_data[[c]] <- nada(df_des_data[[c]])
  }
  df_des_data <- df_des_data[!duplicated(df_des_data),]
  
  df_des_data_pre <- df_des_data
  rm(df_des_data)
  
  archive(fileName = "des_data_pre_1.Rda", currentDir = rdadir, verbose = TRUE)
  archive(fileName = "des_data_pre_2.Rda", currentDir = rdadir, verbose = TRUE)
  archive(fileName = "des_data_pre.Rda", currentDir = rdadir, verbose = TRUE)
  save(df_des_data_pre, file = paste(rdadir, "des_data_pre.Rda", sep = ""))
}
# ---------------------------------------- #
# ---------------------------------------- #








# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...

# load(paste(rdadir, "des_data_pre_1.Rda", sep = ""))
# load(paste(rdadir, "des_data_pre_2.Rda", sep = ""))
# stnds_1 <- unique(df_des_data_pre_1$stand_number)
# stnds_2 <- unique(df_des_data_pre_2$stand_number)
# matches <- match(x = stnds_1, table = stnds_2)
# table(is.na(matches), exclude = NULL)
# matches <- matches[!is.na(matches)]
# inboth <- stnds_2[matches]
# inboth
# df_des_data_pre <- rbind(df_des_data_pre_1, df_des_data_pre_2)
# 
# pre1inboth <- df_des_data_pre_1[which(df_des_data_pre_1$stand_number %in% inboth),]
# pre1inboth$src <- "pre_1"
# pre1inboth <- move.col(df = pre1inboth, colName = "src", colIdx = 1)
# pre1inboth <- pre1inboth[, c("src", "date","stand_number", "household_structure_number", "respondent_household_surname", "respondent_name")]
# 
# pre2inboth <- df_des_data_pre_2[which(df_des_data_pre_2$stand_number %in% inboth),]
# pre2inboth$src <- "pre_2"
# pre2inboth <- move.col(df = pre2inboth, colName = "src", colIdx = 1)
# pre2inboth <- pre2inboth[, c("src", "date", "stand_number", "household_structure_number", "respondent_household_surname", "respondent_name")]
# 
# preInboth <- rbind(pre1inboth, pre2inboth)
# preInboth <- preInboth[which(!duplicated(preInboth)),]
#  
# 
# cidxxTime <- grep(pattern = "time", x = names(df_des_data_pre_1), fixed = TRUE, value = TRUE)

# # how does des_pre_2 have 100 fields more than des_pre_1?? # different survey versions
# # extraFields <- names(df_des_data_pre_2)[!(names(df_des_data_pre_2) %in% names(df_des_data_pre_1))]
# 
# # are there houses that were done twice between the two pre-surveys?
# df_des_data_pre_1$temp_id <- paste(df_des_data_pre_1$stand_number, df_des_data_pre_1$household_structure_number, sep = "_")
# df_des_data_pre_2$temp_id <- paste(df_des_data_pre_2$stand_number, df_des_data_pre_2$household_structure_number, sep = "_")
# uniqs1 <- unique(df_des_data_pre_1$temp_id)
# uniqs2 <- unique(df_des_data_pre_2$temp_id)
# dups <- intersect(uniqs1, uniqs2)


# dups <- unique(df_des_data_pre_1[duplicated(df_des_data_pre_1$stand_number), "stand_number"])

# unique(df_des_data_pre_2[which(duplicated(df_des_data_pre_2$stand_number)), "stand_number"])
