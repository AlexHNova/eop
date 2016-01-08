# Date created: 01 Nov 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# script to explore the coal log data
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

# load
load(paste(rdadir, "df_coal_logs.Rda", sep = ""))

# ---------------------------------------- #
# add totals - the total coal and wood (in kg) used for each individual fire
df_coal_logs$total_wood <- df_coal_logs$no_of_wood_batches * df_coal_logs$weight_of_wood_batch_kg
df_coal_logs$total_coal <- df_coal_logs$no_of_coal_containers * df_coal_logs$weight_of_a_filled_container_kg

# ---------------------------------------- #
# add fields to indicate whether the main source for the fire was coal or wood
df_coal_logs$wood_fire <- 1
df_coal_logs$wood_fire <- ifelse(test = df_coal_logs$total_wood < df_coal_logs$total_coal, 0, 1)
df_coal_logs$coal_fire <- 1
df_coal_logs$coal_fire <- ifelse(test = df_coal_logs$total_coal < df_coal_logs$total_wood, 0, 1)

# ---------------------------------------- #
# add a time of day field is it a morning, midday, afternoon, evening or night fire?
df_coal_logs$time_of_day <- NA_character_

idxx <- which(between(x = hour(df_coal_logs$date), left = 0, right = 4)) 
if (length(idxx) > 0) {df_coal_logs[idxx, "time_of_day"] <- "night"}

idxx <- which(between(x = hour(df_coal_logs$date), left = 5, right = 10))
if (length(idxx) > 0) {df_coal_logs[idxx, "time_of_day"] <- "morning"}

idxx <- which(between(x = hour(df_coal_logs$date), left = 11, right = 13))
if (length(idxx) > 0) {df_coal_logs[idxx, "time_of_day"] <- "midday"}

idxx <- which(between(x = hour(df_coal_logs$date), left = 14, right = 16))
if (length(idxx) > 0) {df_coal_logs[idxx, "time_of_day"] <- "afternoon"}

idxx <- which(between(x = hour(df_coal_logs$date), left = 17, right = 22))
if (length(idxx) > 0) {df_coal_logs[idxx, "time_of_day"] <- "evening"}

idxx <- which(hour(df_coal_logs$date) == 23)
if (length(idxx) > 0) {df_coal_logs[idxx, "time_of_day"] <- "night"}

rm(idxx)

# ---------------------------------------- #
# match stand types to each of the stands
df_coal_logs$type <- NA_character_
df_coal_logs$stand_number <- as.character(df_coal_logs$stand_number)

load(paste(rdadir, "stands_and_types.Rda", sep = ""))

matches <- match(x = df_coal_logs$stand_number, table = stands_and_types$stand_number)
table(is.na(matches))
if (any(!is.na(matches))) {
  df_coal_logs[which(!is.na(matches)), "type"] <- stands_and_types[matches[!is.na(matches)], "type"]
}

rm(matches, stands_and_types)

# ---------------------------------------- #
# reorder the columns
firstCols <- c("date", "stand_number", "extension", "type")
otherCols <- names(df_coal_logs)[!(names(df_coal_logs) %in% firstCols)]
df_coal_logs <- df_coal_logs[, c(firstCols, otherCols)]
rm(firstCols, otherCols)

# ---------------------------------------- #
# save
df_coal_logs_exploration <- df_coal_logs
rm(df_coal_logs)
archive(fileName = "exploration_coal_logs.Rda", currentDir = rdadir, verbose = TRUE)
save(df_coal_logs_exploration, file = paste(rdadir, "exploration_coal_logs.Rda", sep = ""))

# ---------------------------------------- #
# table(df_coal_logs_exploration$new_or_refill, df_coal_logs_exploration$time_of_day, exclude = NULL)
# round(x = 100 * (prop.table(table(df_coal_logs_exploration$new_or_refill, df_coal_logs_exploration$time_of_day, exclude = NULL))), digits = 1)
# 
# table(df_coal_logs_exploration$time_of_day, df_coal_logs_exploration$to_cook, exclude = NULL)
# 
# table(df_coal_logs_exploration$time_of_day, df_coal_logs_exploration$to_heat_house, exclude = NULL)
# 
# table(df_coal_logs_exploration$time_of_day, df_coal_logs_exploration$to_heat_water, exclude = NULL)


# ---------------------------------------- #
# ---------------------------------------- #
# NOTES




# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...

# how much coal per household of each type per winter season (take summed readings for June and July and times it by two to estimate the avg usage over a typical 4-month South African winter)
{
load(paste(rdadir, "exploration_coal_logs.Rda", sep = ""))

CARRIER <- "coal"
SEASON <- "annual" 
# "winter" will calculate the avg total usage over a 4 month winter; 
# "summer" will calculate the avg total usage over an 8 month summer;
# "annual" will calculate both and return their sum, amounting to a 12 month year.

table(month(df_coal_logs_exploration$date))
if (SEASON == "winter") {
  df_coal_logs_exploration <- df_coal_logs_exploration[which(month(df_coal_logs_exploration$date) %in% c(6:7)),]
}
if (SEASON == "summer") {
  df_coal_logs_exploration <- df_coal_logs_exploration[which(month(df_coal_logs_exploration$date) %in% c(9:10)),]
}
if (SEASON == "annual") {
  df_coal_logs_exploration <- df_coal_logs_exploration[which(month(df_coal_logs_exploration$date) %in% c(6:10)),]
}

typesplits <- split(x = df_coal_logs_exploration, f = df_coal_logs_exploration$type)

typeAvgs <- sapply(X = typesplits, FUN = function(tpdf) {
  
  standsplits <- split(x = tpdf, f = tpdf$stand_number) 
  
  standAvgs <- sapply(X = standsplits, FUN = function(sdf) {
    
    var <- ifelse(test = CARRIER == "coal", "total_coal", "total_wood")

    if (SEASON %in% c("winter", "annual")) {
      totalWinter <- sum(x = sdf$total_coal, na.rm = TRUE) * 2
    }
    
    if (SEASON %in% c("summer", "annual")) {
      totalMild <- sum(x = sdf[which(month(sdf$date) == 9), var], na.rm = TRUE) * 2
      totalHot <- sum(x = sdf[which(month(sdf$date) == 10), var], na.rm = TRUE) * 6
      totalSummer <- totalMild + totalHot
    }
    
    if (SEASON == "winter") {return(totalWinter)}
    if (SEASON == "summer") {return(totalSummer)}
    if (SEASON == "annual") {return(totalWinter + totalSummer)}

  })
  
  tpavg <- mean(x = standAvgs, na.rm = TRUE)
  return(tpavg)
})

names(typeAvgs) <- names(typesplits)
typeAvgs
}

# ---------------------------------------- #
# ---------------------------------------- #




# ---------------------------------------- #
# ---------------------------------------- #
# visual exploration
