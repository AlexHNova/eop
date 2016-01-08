# Date created: 
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# script to prepare the final list of stand numbers and types for KwaZamokuhle by merging the lists of stand numbers found in (1) the ibutton data, (2) coal weighing data and (3) Thembi Tsotetsi's stand info.
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
# extract the data from Thembi Tsotetsi's info
load(paste(rdadir, "df_stands_info.Rda", sep = ""))
df_stands_info$type <- paste(as.character(df_stands_info$energy_type), as.character(df_stands_info$insulation_type), sep = "_")
df_stands_info$stand_number <- as.character(df_stands_info$stand_number)
stands_tt <- df_stands_info[, c("stand_number", "type")]
rm(df_stands_info)

# ---------------------------------------- #
# extract the data from the classified ibutton data
load(paste(rdadir, "df_ibutton_data_classified.Rda", sep = ""))
idxx <- grep(pattern = "3327", x = as.character(df_ibutton_data_classified$stand_number))
if (length(idxx) > 0) {
  df_ibutton_data_classified[idxx, "energy_type"] <- "lpg"
}
df_ibutton_data_classified$type <- paste(as.character(df_ibutton_data_classified$energy_type), as.character(df_ibutton_data_classified$insulation_type), sep = "_")
df_ibutton_data_classified$stand_number <- as.character(df_ibutton_data_classified$stand_number)

stands_ib <- as.data.frame(unique(df_ibutton_data_classified$stand_number), stringsAsFactors = FALSE)
names(stands_ib) <- "stand_number"
matches <- match(x = stands_ib$stand_number, table = df_ibutton_data_classified$stand_number)
stands_ib$type <- df_ibutton_data_classified[matches, "type"]

rm(df_ibutton_data_classified)

# ---------------------------------------- #
# extract the data from the coal weighing data
load(paste(rdadir, "df_coal_data_with_calc_cols.Rda", sep = ""))
stands_coal <- as.data.frame(unique(as.character(df_coal_data_with_calc_cols$stand_number)), stringsAsFactors = FALSE)
names(stands_coal) <- "stand_number"
rm(df_coal_data_with_calc_cols)

# ---------------------------------------- #
# now try to put them all together
## first get a list of all the stand numbers that we have in total, without duplicates
stands_and_types <- as.data.frame(c(stands_tt$stand_number, stands_ib$stand_number, stands_coal$stand_number), stringsAsFactors = FALSE)
names(stands_and_types) <- "stand_number"
dups <- duplicated(stands_and_types$stand_number)
stands_and_types <- as.data.frame(stands_and_types[!dups,], stringsAsFactors = FALSE)
names(stands_and_types) <- "stand_number"

# now add their types
stands_and_types$type_tt <- NA_character_
matches <- match(x = stands_and_types$stand_number, table = stands_tt$stand_number)
stands_and_types[which(!is.na(matches)), "type_tt"] <- stands_tt[matches[!is.na(matches)], "type"]  

stands_and_types$type_ib <- NA_character_
matches <- match(x = stands_and_types$stand_number, table = stands_ib$stand_number)
stands_ib$type <- tolower(stands_ib$type)
stands_ib$type <- gsub(pattern = "electricity", replacement = "elec", x = stands_ib$type, fixed = TRUE)
stands_and_types[which(!is.na(matches)), "type_ib"] <- stands_ib[matches[!is.na(matches)], "type"]

# stands_and_types$correlate <- stands_and_types$type_tt == stands_and_types$type_ib

stands_and_types$type <- NA_character_

for (r in 1:nrow(stands_and_types)) {
  tps <- c(stands_and_types[r, "type_ib"], stands_and_types[r, "type_tt"])
  if (any(!is.na(tps))) {
    tps <- unique(tps[!is.na(tps)])
    if (length(tps) > 1) {stop("Houston, two different stand types found at index ", r)}
    stands_and_types[r, "type"] <- tps
  }
}

idxx <- match(x = c("type_tt", "type_ib"), table = names(stands_and_types))
if (any(!is.na(idxx))) {
  stands_and_types <- stands_and_types[,-idxx[!is.na(idxx)]]
}

# ---------------------------------------- #
# save the list
save(stands_and_types, file = paste(rdadir, "stands_and_types.Rda", sep = ""))

# clean up
rm(stands_coal, stands_ib, stands_tt, dups, idxx, matches, r, tps)

# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...