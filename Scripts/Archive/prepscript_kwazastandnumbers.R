# script to read the KwaZamokuhle stand numbers of all seven test groups into one data frame and standardise their formatting
# -------------------------------------------- #
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
library(openxlsx)

# source additional functions
source(file = paste(rfunctdir, "fixname.R", sep = ""))

# declare constants

# define additional functions

# -------------------------------------------- #
# read the data
df_standnumbers <- read.xlsx(xlsxFile = paste(datadir, "Standnumbers/Standnumbers_KwaZamokuhle_combined.xlsx", sep = ""), colNames = TRUE, sheet = "Sheet2")

# -------------------------------------------- #
# standardise the column names
names(df_standnumbers) <- fixname(names(df_standnumbers))

# standardise the column types
df_standnumbers$stand_number <- as.integer(df_standnumbers$stand_number)
df_standnumbers$extension <- as.integer(df_standnumbers$extension)

# standardise the cell contents
for (c in 1:ncol(df_standnumbers)) {
  if (!is.character(df_standnumbers[, c])) {next}
  df_standnumbers[, c] <- tolower(df_standnumbers[, c])
}

# # separate the "ext..." info from the stand numbers
# df_standnumbers$extension <- df_standnumbers$stand_number
# 
# for (r in 1:length(df_standnumbers$stand_number)) {
#   x <- df_standnumbers[r, "stand_number"]
# 
#   df_standnumbers[r, "stand_number"] <- gsub(pattern = ext, replacement = "", x = df_standnumbers[r, "stand_number"], fixed = TRUE)
#   df_standnumbers[r, "extension"] <- ext
# }

# -------------------------------------------- #
# match the stand numbers from "stands_and_types" (obtained from the ibutton data) with the stand numbers above (derived from the lists in Dropbox (Nova SA)\Eskom Offset Study 2014 - Team\R\EOP\Data\Standnumbers\)

df_standtypes <- read.xlsx(xlsxFile = paste(datadir, "stands_and_types.xlsx", sep = ""), sheet = "Sheet1", colNames = FALSE)
names(df_standtypes) <- c("stand_number", "type")

matches <- match(df_standtypes$stand_number, df_standnumbers$stand_number)
has_match <- df_standnumbers[matches[!is.na(matches)], c("stand_number", "type")]
names(has_match)[which(names(has_match) == "type")] <- "type_sndf"
matches <- match(has_match$stand_number, df_standtypes$stand_number)
has_match$type_tpdf <- df_standtypes[matches, "type"]

# now match the matching stand numbers to the stand numbers for which we have electricity usage data
## first get the stands that are supplied with electricity directly from Eskom
df_eskom_elec <- read.csv(file = paste(datadir, "Kwaza_electrification_data_sales.csv", sep = ""), header = TRUE, stringsAsFactors = FALSE, sep = ';')

eskom_elec_stands <- df_eskom_elec$Stand.number
eskom_elec_stands <- eskom_elec_stands[!is.na(eskom_elec_stands)]
# o vet, hier is duplikate in hierdie kolom...hoekom???
eskom_elec_stands <- eskom_elec_stands[!duplicated(eskom_elec_stands)]

matches <- match(x = eskom_elec_stands, table = has_match$stand_number)
has_match$supplier <- NA_character_
has_match[which(!is.na(matches)), "supplier"] <- "eskom"

## now get the numbers of the stands that are supplied with prepaid electricity via the municipality
df_munic_elec <- read.xlsx(xlsxFile = paste(datadir, "KwaZa_elec_data_munic.xlsx", sep = ""), sheet = "stands_munic_elec", colNames = FALSE)

# now match them to the has_match stands
munic_elec_stands <- df_munic_elec$stand_number
matches <- match(munic_elec_stands, table = as.character(has_match$stand_number))
has_match[which(!is.na(matches)), "supplier"] <- "munic"







# -------------------------------------------- #
fileList <- c("Coal stove-Ceiling+Full+trombe frame 15July2015_test houses sampled (Thembi Tsotetsi's conflicted copy 2015-09-21).xlsx",
              "Electricity-Ceiling+Full+trombe frame 15 July 2015_test houses sampled (Thembi Tsotetsi's conflicted copy 2015-09-21).xlsx",
              "LPG-Ceiling+Full+trombe frame 15 July 2015_test houses sampled (Thembi Tsotetsi's conflicted copy 2015-09-21).xlsx")


df_cf <- read.xlsx(xlsxFile = paste(datadir, "Standnumbers/Coal stove-Ceiling+Full+trombe frame 15July2015_test houses sampled (Thembi Tsotetsi's conflicted copy 2015-09-21).xlsx", sep = ""), sheet = "Full.coal stove", colNames = TRUE)
df_cf$type <- "COAL_FULL"
names(df_cf)[[1]] <- "stand_number"
idx <- match("Electricity.prepaid.no.", table = names(df_cf))
names(df_cf)[[idx]] <- "meter_no"
df_cf <- df_cf[, c("stand_number", "meter_no")]
df_cf$type <- "COAL_FULL"


df_cb <- read.xlsx(xlsxFile = paste(datadir, "Standnumbers/Coal stove-Ceiling+Full+trombe frame 15July2015_test houses sampled (Thembi Tsotetsi's conflicted copy 2015-09-21).xlsx", sep = ""), sheet = "Ceiling.coal stove", colNames = TRUE)
df_cb$type <- "COAL_BASIC"
names(df_cb)[[1]] <- "stand_number"
idx <- match("Electricity.prepaid.no.", table = names(df_cb))
names(df_cb)[[idx]] <- "meter_no"
df_cb <- df_cb[, c("stand_number", "meter_no")]
df_cb$type <- "COAL_BASIC"




df_ef <- read.xlsx(xlsxFile = paste(datadir, "Standnumbers/Electricity-Ceiling+Full+trombe frame 15 July 2015_test houses sampled (Thembi Tsotetsi's conflicted copy 2015-09-21).xlsx", sep = ""), sheet = "Full.Electricity", colNames = TRUE)
df_ef$type <- "ELEC_FULL"
names(df_ef)[[1]] <- "stand_number"
idx <- match("Prepaid.no.", table = names(df_ef))
names(df_ef)[[idx]] <- "meter_no"
df_ef <- df_ef[, c("stand_number", "meter_no")]
df_ef$type <- "ELEC_FULL"



df_eb <- read.xlsx(xlsxFile = paste(datadir, "Standnumbers/Electricity-Ceiling+Full+trombe frame 15 July 2015_test houses sampled (Thembi Tsotetsi's conflicted copy 2015-09-21).xlsx", sep = ""), sheet = "Ceiling.Electricity", colNames = TRUE)
df_eb$type <- "ELEC_BASIC"
names(df_eb)[[1]] <- "stand_number"
idx <- match("Prepaid.number", table = names(df_eb))
names(df_eb)[[idx]] <- "meter_no"
df_eb <- df_eb[, c("stand_number", "meter_no")]
df_eb$type <- "ELEC_BASIC"




df_lf <- read.xlsx(xlsxFile = paste(datadir, "Standnumbers/LPG-Ceiling+Full+trombe frame 15 July 2015_test houses sampled (Thembi Tsotetsi's conflicted copy 2015-09-21).xlsx", sep = ""), sheet = "Full.gas", colNames = TRUE)
df_lf$type <- "LPG_FULL"
names(df_lf)[[1]] <- "stand_number"
idx <- match("Prepaid.number", table = names(df_lf))
names(df_lf)[[idx]] <- "meter_no"
df_lf <- df_lf[, c("stand_number", "meter_no")]
df_lf$type <- "LPG_FULL"



df_lb <- read.xlsx(xlsxFile = paste(datadir, "Standnumbers/LPG-Ceiling+Full+trombe frame 15 July 2015_test houses sampled (Thembi Tsotetsi's conflicted copy 2015-09-21).xlsx", sep = ""), sheet = "Basic.gas", colNames = TRUE)
df_lb$type <- "LPG_BASIC"
names(df_lb)[[1]] <- "stand_number"

idx <- match("Prepaid.number", table = names(df_lb))
names(df_lb)[[idx]] <- "meter_no"
df_lb <- df_lb[, c("stand_number", "meter_no")]
df_lb$type <- "LPG_BASIC"

df_all <- rbind(df_cb, df_cf, df_eb, df_ef, df_lb, df_lf)


df_all$supplier <- NA_real_

# -------------------------------------------- #
df_all <- read.csv(file = paste(datadir, "df_all.csv", sep = ""), header = TRUE, sep = ';')


matches <- match(munic_elec_stands, df_all$stand_number)
df_all[matches[!is.na(matches)], "supplier"] <- "munic"

matches <- match(eskom_elec_stands, df_all$stand_number)
df_all[matches[!is.na(matches)], "supplier"] <- "eskom"


write.csv(df_all, file = paste(datadir, "df_all.csv", sep = ""))





df_2008 <- read.xlsx(xlsxFile = paste(datadir, "Kwaza_electrification_data_sales.xlsx", sep = ""), sheet = "2008", colNames = FALSE)




