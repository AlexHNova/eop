# Date created: 22 October 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# script to combine the three sets of elec data (2x munic, 1 eskom) for KwaZamokuhle (Proj: EOP)
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

# load the data
load(paste(rdadir, "df_elec_data_eskom.Rda", sep = ""))
load(paste(rdadir, "df_elec_data_munic_set1.Rda", sep = ""))
load(paste(rdadir, "df_elec_data_munic_set2.Rda", sep = ""))

# ---------------------------------------- #
# add a supplier column to each
df_elec_data_eskom$supplier <- "eskom"
df_elec_data_munic_set1$supplier <- "munic"
df_elec_data_munic_set2$supplier <- "munic"

# ---------------------------------------- #
# remove unnecessary fields from each, move some other fields, and rename thingies
df_elec_data_eskom <- remove.col(df = df_elec_data_eskom, 
                                 colName = "transaction_id")

df_elec_data_munic_set2 <- remove.cols(df = df_elec_data_munic_set2, 
                                       colNames = c("account_number", 
                                                    "transaction_number", 
                                                    "meter_ti", 
                                                    "pos", 
                                                    "user"))

df_elec_data_munic_set2 <- move.col(df = df_elec_data_munic_set2, 
                                    colName = "stand_number", 
                                    colIdx = 2)


# ---------------------------------------- #
# put them together
df_elec_data_eskom$stand_number <- as.character(df_elec_data_eskom$stand_number)
df_elec_data_munic_set1$stand_number <- as.character(df_elec_data_munic_set1$stand_number)
df_elec_data_munic_set2$stand_number <- as.character(df_elec_data_munic_set2$stand_number)

## MAKE SURE THERE ARE NO MORE FACTORED FIELDS IN ANY OF THE DATA SETS
dataSets <- list(df_elec_data_eskom, df_elec_data_munic_set1, df_elec_data_munic_set2)
rm(df_elec_data_eskom, df_elec_data_munic_set1, df_elec_data_munic_set2)

dataSets <- lapply(X = dataSets, FUN = function(ds) {
  for (c in 1:ncol(ds)) {
    if (is.factor(ds[[c]])) {
      ds[[c]] <- as.character(ds[[c]])
    }
  }
  return(ds)
})


df_elec_data_all <- do.call("rbind.fill", dataSets)

rm(dataSets)

# ---------------------------------------- #
# rearrange some fields
df_elec_data_all <- move.cols(df = df_elec_data_all, colNames = c("extension", "user", "type", "supplier", "transaction_type"), colIdxx = c(3, 4, 5, 6, 13))

# ---------------------------------------- #
# now try to add the stand types to the combined df
df_elec_data_all$type <- NA_character_

# --------------------- #
## first try it using the stand numbers to match records
load(paste(rdadir, "stands_and_types.Rda", sep = ""))

matches <- match(x = as.character(df_elec_data_all$stand_number), table = as.character(stands_and_types$stand_number))
if (any(!is.na(matches))) {
  df_elec_data_all[which(!is.na(matches)), "type"] <- as.character(stands_and_types[matches[!is.na(matches)], "type"])
}

rm(matches, stands_and_types)

# --------------------- #
## now see if there are any records that do not match according to the stand number but match according to the meter number
load(paste(rdadir, "df_stands_info.Rda", sep = ""))
df_stands_info$type <- paste(as.character(df_stands_info$energy_type), as.character(df_stands_info$insulation_type), sep = "_")
df_elec_data_all$stand_2 <- NA

matches <- match(x = as.character(df_elec_data_all$meter_number), table = as.character(df_stands_info$meter_number))
if (any(!is.na(matches))) {
  df_elec_data_all[which(!is.na(matches)), "type"] <- as.character(df_stands_info[matches[!is.na(matches)],"type"])
  df_elec_data_all[which(!is.na(matches)), "stand_2"] <- as.character(df_stands_info[matches[!is.na(matches)],"stand_number"])
}

# after inspection it became clear that the above records did not match according to stand_number because their stand_numbers in the Eskom data were wrong; fix it. 
df_elec_data_all[which(!is.na(matches)), "stand_number"] <- as.character(df_stands_info[matches[!is.na(matches)],"stand_number"])
 
df_elec_data_all <- remove.col(df = df_elec_data_all, colName = "stand_2")

rm(matches, df_stands_info)

# ---------------------------------------- #
# remove the records from df_elec_data_all which are not associated with  or relevant to any of our stands
idxx <- which(is.na(df_elec_data_all$stand_number))
if (length(idxx) > 0) {
  df_elec_data_all <- df_elec_data_all[-idxx,]
}

idxx <- which(is.na(df_elec_data_all$type))
if (length(idxx) > 0) {
  df_elec_data_all <- df_elec_data_all[-idxx,]  
}

df_elec_data_all <- df_elec_data_all[which(year(df_elec_data_all$date) > 2013),]

# ---------------------------------------- #
# save the data
archive(fileName = "df_elec_data_all.Rda", currentDir = rdadir, verbose = TRUE)
save(df_elec_data_all, file = paste(rdadir, "df_elec_data_all.Rda", sep = ""))
archive(fileName = "df_elec_data_all.xlsx", currentDir = outputdir, verbose = TRUE)
write.xlsx(x = df_elec_data_all, file = paste(outputdir, "df_elec_data_all.xlsx", sep = ""))

# clean up
rm(idxx)


# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...


# ---------------------------------------- #
# HOW MANY STANDS DO WE HAVE OF EACH TYPE?
stnds <- unique(as.character(df_elec_data_all$stand_number))
load(paste(rdadir, "stands_and_types.Rda", sep = ""))
matches <- match(x = stnds, table = as.character(stands_and_types$stand_number))
tps <- stands_and_types[matches, "type"]
temp <- data.frame(stnds, tps)
# View(temp)
table(temp$tps)
rm(stands_and_types, temp)








# ---------------------------------------- #
# # DETERMINE THE STANDS FOR WHICH WE STILL NEED ELEC DATA
# load(paste(rdadir, "df_stands_info.Rda", sep = ""))
# matches <- match(as.character(df_stands_info$stand_number), table = df_elec_data_all$stand_number)
# stnds_without <- df_stands_info[which(is.na(matches)),]
# stnds_without <- stnds_without[, c("stand_number", "extension", "energy_type", "insulation_type", "meter_number")]
# stnds_without$type <- paste(stnds_without$energy_type, stnds_without$insulation_type, sep = "_")
# stnds_without <- stnds_without[, c("stand_number", "extension", "meter_number", "type")]
# archive(fileName = "elec_data_still_needed.csv", currentDir = outputdir, verbose = TRUE)
# write.csv(x = stnds_without, file = paste(outputdir, "elec_data_still_needed.csv", sep = ""))
# rm(df_stands_info)

# ---------------------------------------- #
# INSPECT THE DIFFERENCES BETWEEN THE STAND NUMBERS IN THE ELEC DATA AND OUR STAND LISTS FOR THOSE STANDS THAT DID NOT HAVE ANY STAND_NUMBER MATCHES BUT DID HAVE METER_NUMBER MATCHES
# cheksns <- df_elec_data_all[which(!is.na(as.character(df_elec_data_all$stand_number)) & !is.na(df_elec_data_all$sn_eskomelec)),]
# checksns$corrtype <- checksns$type == checksns$tp_eskomelec
# checksns$corrstand <- checksns$stand_number == checksns$sn_eskomelec
# problemstnds <- checksns[which(!checksns$corrstand),]c

# ---------------------------------------- #
# # QUICK VISUAL EXPLORATION
# ggplot(data = df_elec_data_all, mapping = aes(x = transaction_date_time, y = kwh_units, group = stand_number, colour = stand_number)) + facet_grid(facets = . ~ type) + geom_line() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'right') 
# 
# ggplot(data = df_elec_data_all, mapping = aes(x = transaction_date_time, y = kwh_units, group = stand_number, colour = stand_number)) + facet_grid(facets = . ~ type) + geom_smooth() + theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = 'right') 
