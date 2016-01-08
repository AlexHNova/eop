# Date created: 16 September 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# Script to read and prepare the Eskom iButton data for pre-lim analysis
# ---------------------------------------- #
# start clean
rm(list=ls())
# ---------------------------------------- #
# source the preamble
source(file = paste(getwd(), "/Scripts/preamble.R", sep = ""))
# ---------------------------------------- #
# load necessary libraries
# ---------------------------------------- #
# source addtional functions
source(file = paste(scriptdir, "maak.vuur.R", sep = ""))
# ---------------------------------------- #
# declare script constants
TIME_INTERVAL <- "20 min"
# ---------------------------------------- #
# define auxiliary functions
time_average <- function(df_ibutton_data)
{
  # time to time average... (the wall reading lags 16 minutes, so we need to fix that as well)
  # split the df into to smaller dfs - one for the W readings and one for the C readings
  df_ibutton_data$full_name <- as.character(df_ibutton_data$full_name)
  df_ibutton_data$button_place <- NA
  df_ibutton_data$button_place <- substr(x = df_ibutton_data$full_name, start = nchar(df_ibutton_data$full_name), stop = nchar(df_ibutton_data$full_name))
  df_ibutton_data$stand_id <- paste(df_ibutton_data$full_name, df_ibutton_data$type, sep = "_")
  df_ibutton_data <- df_ibutton_data[, -match(x = c("full_name", "type"), table = names(df_ibutton_data))]
  
  df_ibutton_data_C <- df_ibutton_data[which(df_ibutton_data$button_place == "c"),]
  df_ibutton_data_C <- df_ibutton_data_C[, -match("button_place", names(df_ibutton_data_C))]
  df_ibutton_data_C_list <- split(x = df_ibutton_data_C, f = df_ibutton_data_C$stand_id)
  
  df_ibutton_data_C_list <- lapply(df_ibutton_data_C_list, FUN = function(x) {
    x <- x[, c("date", "temperature")]
    start_date <- paste(substr(min(x$date), start = 1, stop = 10), "00:00:00", sep = " ")
    x <- timeAverage(mydata = x, avg.time = TIME_INTERVAL, statistic = "mean", start.date = start_date)
  })
  
  df_ibutton_data_C_list <- lapply(X = 1:length(df_ibutton_data_C_list), FUN = function(i) {
    x <- df_ibutton_data_C_list[[i]]
    characteristics <- as.array((strsplit(names(df_ibutton_data_C_list)[[i]], split = "_"))[[1]])
    x$stand_id <- paste(characteristics[1], characteristics[5], characteristics[6], sep = "_")
    x$C_id <- paste(characteristics[2], characteristics[4], characteristics[3], sep = "_")
    x
  })
  
  df_ibutton_data_C <- do.call("rbind", df_ibutton_data_C_list)
  # save(df_ibutton_data_C_list, file = paste(rdadir, "df_ibutton_data_C_list.Rda", sep = ""))
  rm(df_ibutton_data_C_list)
  
  df_ibutton_data_W <- df_ibutton_data[which(df_ibutton_data$button_place == "w"),]
  df_ibutton_data_W <- df_ibutton_data_W[, -match("button_place", names(df_ibutton_data_W))]
  df_ibutton_data_W_list <- split(x = df_ibutton_data_W, f = df_ibutton_data_W$stand_id)
  
  df_ibutton_data_W_list <- lapply(df_ibutton_data_W_list, FUN = function(x) {
    x <- x[, c("date", "temperature")]
    start_date <- paste(substr(min(x$date), start = 1, stop = 10), "00:00:00", sep = " ")
    x <- timeAverage(mydata = x, avg.time = TIME_INTERVAL, statistic = "mean", start.date = start_date)
  })
  
  df_ibutton_data_W_list <- lapply(X = 1:length(df_ibutton_data_W_list), FUN = function(i) {
    x <- df_ibutton_data_W_list[[i]]
    characteristics <- as.array((strsplit(names(df_ibutton_data_W_list)[[i]], split = "_"))[[1]])
    x$stand_id <- paste(characteristics[1], characteristics[5], characteristics[6], sep = "_")
    x$W_id <- paste(characteristics[2], characteristics[4], characteristics[3], sep = "_")
    x
  })
  
  df_ibutton_data_W <- do.call("rbind", df_ibutton_data_W_list)
  # save(df_ibutton_data_W_list, file = paste(rdadir, "df_ibutton_data_W_list.Rda", sep = ""))
  rm(df_ibutton_data_W_list)
  
  names(df_ibutton_data_C)[match("temperature", table = names(df_ibutton_data_C))] <- "C"
  names(df_ibutton_data_W)[match("temperature", table = names(df_ibutton_data_W))] <- "W"
  df_ibutton_data_synced <- full_join(x = df_ibutton_data_C, y = df_ibutton_data_W, by = c("date", "stand_id"))
  df_ibutton_data_synced <- df_ibutton_data_synced[, c("date", "stand_id", "W", "W_id", "C", "C_id")]
  df_ibutton_data <- df_ibutton_data_synced
  rm(df_ibutton_data_C, df_ibutton_data_W, df_ibutton_data_synced)
  
  df_ibutton_data$stand_id <- as.factor(df_ibutton_data$stand_id)
  df_ibutton_data$C_id <- as.factor(df_ibutton_data$C_id)
  df_ibutton_data$W_id <- as.factor(df_ibutton_data$W_id)
  
  df_ibutton_data_postTA <- df_ibutton_data
  save(df_ibutton_data_postTA, file = paste(rdadir, "df_ibutton_data_postTA.Rda", sep = ""))
  rm(df_ibutton_data_postTA)
  
  return(df_ibutton_data)
}

# -------------------------------------- #
# -------------------------------------- #
# read the data
df_ibutton_data <- read.csv(file = paste(datadir, "iButtons/all_button_data_ExcelPrepped.csv", sep = ""), header = TRUE, sep = ";")

print("Progress point 1...", quote = FALSE)
# -------------------------------------- #
# format the column names and column types

names(df_ibutton_data) <- fixname(names(df_ibutton_data))

for(i in 1:ncol(df_ibutton_data))
{
  col <- df_ibutton_data[, i]
  col_temp <- try(tolower(col))
  if (length(col_temp) == length(col)) {df_ibutton_data[, i] <- col_temp}
}

names(df_ibutton_data)[match(x = "date_and_time", table = names(df_ibutton_data))] <- "date"
df_ibutton_data <- df_ibutton_data[, - match(x = c("retrofit_type", "unit"), table = names(df_ibutton_data))]

df_ibutton_data$date <- as.POSIXct(substr(df_ibutton_data$date, start = 1, stop = 16), tz = TIME_ZONE)
df_ibutton_data$temperature <- as.numeric(df_ibutton_data$temperature)

print("Progress point 2...", quote = FALSE)
# -------------------------------------- #
# clean the full_name and file_name fields up a bit

df_ibutton_data$full_name <- gsub(pattern = "^eop_", replacement = "", x = df_ibutton_data$full_name)
df_ibutton_data$full_name <- gsub(pattern = ".csv", replacement = "", x = df_ibutton_data$full_name, fixed = TRUE)
df_ibutton_data$full_name <- gsub(pattern = "[[:blank:]]", replacement = "", x = df_ibutton_data$full_name)
df_ibutton_data$full_name <- gsub(pattern = "[[:punct:]]", replacement = "_", x = df_ibutton_data$full_name)

df_ibutton_data$file_name <- gsub(pattern = "^eop_", replacement = "", x = df_ibutton_data$file_name)
df_ibutton_data$file_name <- gsub(pattern = "_all_button_data.csv", replacement = "", x = df_ibutton_data$file_name, fixed = TRUE)
df_ibutton_data$file_name <- gsub(pattern = "[[:blank:]]", replacement = "", x = df_ibutton_data$file_name)
df_ibutton_data$file_name <- gsub(pattern = "[[:punct:]]", replacement = "_", x = df_ibutton_data$file_name)

df_ibutton_data[which(df_ibutton_data$file_name == "control"),"file_name"] <- "control_none"
names(df_ibutton_data)[match("file_name", table = names(df_ibutton_data))] <- "type"
df_ibutton_data$full_name <- as.factor(df_ibutton_data$full_name)
df_ibutton_data$type <- as.factor(df_ibutton_data$type)

df_ibutton_data_preTA <- df_ibutton_data
archive(fileName = "df_ibutton_data_preTA.Rda", currentDir = rdadir, verbose = TRUE)
save(df_ibutton_data_preTA, file = paste(rdadir, "df_ibutton_data_preTA.Rda", sep = ""))
rm(df_ibutton_data_preTA)

print("Progress point 3...", quote = FALSE)
# -------------------------------------- #
# time average

df_ibutton_data <- time_average(df_ibutton_data)

print("Progress point 4...", quote = FALSE)

print("Progress point 5...", quote = FALSE)
# -------------------------------------- #
# split the stand_id field into core fields and remove
df_ibutton_data$stand_id <- as.character(df_ibutton_data$stand_id)
df_ibutton_data$stand_number <- word(df_ibutton_data$stand_id, start = 1, sep = "_")
df_ibutton_data$stand_number <- factor(x = df_ibutton_data$stand_number)

df_ibutton_data$energy_type <- NA
df_ibutton_data$energy_type <- word(as.character(df_ibutton_data$stand_id), start = 2, sep = "_")
df_ibutton_data$energy_type <- factor(df_ibutton_data$energy_type, levels = c("control", "electricity", "lpg", "coal"), ordered = TRUE)

df_ibutton_data$insulation_type <- NA
df_ibutton_data$insulation_type <- word(as.character(df_ibutton_data$stand_id), start = 3, sep = "_")
df_ibutton_data$insulation_type <- factor(df_ibutton_data$insulation_type, levels = c("none", "basic", "full"), ordered = TRUE)

df_ibutton_data <- df_ibutton_data[, - match("stand_id", names(df_ibutton_data))]

print("Progress point 6...", quote = FALSE)
# -------------------------------------- #
# split the W_id field into core fields

df_ibutton_data$W_id <- as.character(df_ibutton_data$W_id)

df_ibutton_data$read_date_W <- NA

for (i in 1:length(df_ibutton_data$W_id)) {
  if (is.na(df_ibutton_data[i, "W_id"])) {next}
  
  read_date <- word(df_ibutton_data[i,"W_id"], start = 3, sep = "_")
  if (read_date == "200150731") {read_date <- "20150731"} # this error occurs in the original data
  if (read_date == "20050731") {read_date <- "20150731"} # this error occurs in the original data
  if (read_date == "20150831") {read_date <- "20150731"} # this error occurs in the original data
  read_date_yr <- substr(x = read_date, start = 1, stop = 4)
  read_date_mo <- substr(x = read_date, start = 5, stop = 6)
  read_date_day <- substr(x = read_date, start = 7, stop = 8)
  read_date <- paste(read_date_day, read_date_mo, read_date_yr, sep = "-")
  # read_date <- as.Date(read_date)
  
  df_ibutton_data[i, "read_date_W"] <- read_date
}

df_ibutton_data$button_shortname_W <- word(df_ibutton_data$W_id, start = 2, sep = "_")
df_ibutton_data$button_shortname_W <- as.factor(df_ibutton_data$button_shortname_W)

df_ibutton_data$W_id <- word(df_ibutton_data$W_id, start = 1, sep = "_")
df_ibutton_data$W_id <- as.factor(df_ibutton_data$W_id)

print("Progress point 7...", quote = FALSE)
# -------------------------------------- #
# split the C_id field into core fields

df_ibutton_data$C_id <- as.character(df_ibutton_data$C_id)

df_ibutton_data$read_date_C <- NA

for (i in 1:length(df_ibutton_data$C_id))
{
  if (is.na(df_ibutton_data[i, "C_id"])) {next}
  
  read_date <- word(df_ibutton_data[i,"C_id"], start = 3, sep = "_")
  read_date_yr <- substr(x = read_date, start = 1, stop = 4)
  read_date_mo <- substr(x = read_date, start = 5, stop = 6)
  read_date_day <- substr(x = read_date, start = 7, stop = 8)
  read_date <- paste(read_date_yr, read_date_mo, read_date_day, sep = "-")
  # read_date <- as.Date(read_date)
  
  df_ibutton_data[i, "read_date_C"] <- read_date
}

df_ibutton_data$button_shortname_C <- word(df_ibutton_data$C_id, start = 2, sep = "_")
df_ibutton_data$button_shortname_C <- as.factor(df_ibutton_data$button_shortname_C)

df_ibutton_data$C_id <- word(df_ibutton_data$C_id, start = 1, sep = "_")
df_ibutton_data$C_id <- as.factor(df_ibutton_data$C_id)

print("Progress point 8...", quote = FALSE)
# -------------------------------------- #

# add metadata
df_ibutton_data$C <- as.numeric(df_ibutton_data$C)
attributes(df_ibutton_data$C) <- list(unit = "degrees Celsius")

df_ibutton_data$W <- as.numeric(df_ibutton_data$W)
attributes(df_ibutton_data$W) <- list(unit = "degrees Celsius")

print("Progress point 9...", quote = FALSE)
# -------------------------------------- #
# put the columns in a better order
df_ibutton_data <- df_ibutton_data[, c("date", "stand_number", "energy_type", "insulation_type", "W", "W_id", "button_shortname_W", "read_date_W", "C", "C_id", "button_shortname_C", "read_date_C")]

print("Progress point 10...", quote = FALSE)
# -------------------------------------- #

# fix the energy type for stand 3327
idxx <- grep(pattern = "3327", x = as.character(df_ibutton_data$stand_number))
if (length(idxx) > 0) {
  df_ibutton_data[idxx, "energy_type"] <- "lpg"
}

# save the prepared data
df_ibutton_data_prepared <- df_ibutton_data
archive(fileName = "df_ibutton_data_prepared.Rda", currentDir = rdadir, verbose = TRUE)
save(df_ibutton_data_prepared, file = paste(rdadir, "df_ibutton_data_prepared.Rda", sep = ""))
rm(df_ibutton_data_prepared, df_ibutton_data)

print("Progress point 11...DONE.", quote = FALSE)
# -------------------------------------- #
# -------------------------------------- #





# -------------------------------------- #
# -------------------------------------- #
# NOTES


# -------------------------------------- #
# -------------------------------------- #
# DOODLE CODE...

# df_ibutton_data_prepared$type <- paste(as.character(df_ibutton_data_prepared$energy_type), as.character(df_ibutton_data_prepared$insulation_type), sep = "_")
# 
# cb <- df_ibutton_data_prepared[which(df_ibutton_data_prepared$type == "coal_basic"), c("stand_number", "type")]
# cb <- as.array(unique(cb$stand_number))
# 
# cf <- df_ibutton_data_prepared[which(df_ibutton_data_prepared$type == "coal_full"), c("stand_number", "type")]
# cf <- as.array(unique(cf$stand_number))
# 
# eb <- df_ibutton_data_prepared[which(df_ibutton_data_prepared$type == "electricity_basic"), c("stand_number", "type")]
# eb <- as.array(unique(eb$stand_number))
# 
# ef <- df_ibutton_data_prepared[which(df_ibutton_data_prepared$type == "electricity_full"), c("stand_number", "type")]
# ef <- as.array(unique(ef$stand_number))
# 
# lb <- df_ibutton_data_prepared[which(df_ibutton_data_prepared$type == "lpg_basic"), c("stand_number", "type")]
# lb <- as.array(unique(lb$stand_number))
# 
# lf <- df_ibutton_data_prepared[which(df_ibutton_data_prepared$type == "lpg_full"), c("stand_number", "type")]
# lf <- as.array(unique(lf$stand_number))
# 
# cntrl <- df_ibutton_data_prepared[which(df_ibutton_data_prepared$type == "control_none"), c("stand_number", "type")]
# cntrl <- as.array(unique(cntrl$stand_number))

