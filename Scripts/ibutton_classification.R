# Date created: 01 November 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# script to classify the Eskom ibutton data
# ---------------------------------------- #
# start clean
rm(list = ls())
# ---------------------------------------- #
# source preamble
dropdir <- "C:/Users/Alex H/Dropbox (Nova SA)/"
projdir <- paste(dropdir, "Eskom Offset Study 2014 - Team/R/eop/", sep = "")
source(paste(projdir, "/Scripts/preamble.R", sep = ""))
# ---------------------------------------- #
# load locally required libraries
library(e1071)
library(caret)
# ---------------------------------------- #
# source additional functions
source(paste(novafunctdir, "wees.vure.R", sep = ""))
source(paste(novafunctdir, "heg_vure.R", sep = ""))
# ---------------------------------------- #
# declare script constants
VERBOSE <- TRUE
DEBUG <- TRUE
# ---------------------------------------- #
# ---------------------------------------- #

# load the data
load(paste(rdadir, "df_ibutton_data.Rda", sep = ""))

# ---------------------------------------- #
# load the machine
load(paste(novafunctdir,"svm_fire_classifier.Rda", sep = ""))

# ---------------------------------------- #
# add the fields required by the svm monster
df_ibutton_data$sdif <- df_ibutton_data$c - df_ibutton_data$w
df_ibutton_data$lag_c <- lag(df_ibutton_data$c)
df_ibutton_data$lag_c2 <- lag(df_ibutton_data$c, 2)
df_ibutton_data$lag_c3 <- lag(df_ibutton_data$c, 3)
df_ibutton_data$lag_w <- lag(df_ibutton_data$w)
df_ibutton_data$lag_w2 <- lag(df_ibutton_data$w, 2)
df_ibutton_data$lag_w3 <- lag(df_ibutton_data$w, 3)
df_ibutton_data$lag_sdif <- lag(df_ibutton_data$sdif)
df_ibutton_data$lag_sdif2 <- lag(df_ibutton_data$sdif, 2)
df_ibutton_data$lag_sdif3 <- lag(df_ibutton_data$sdif, 3)

# ---------------------------------------- #
# classify, household by household
standsplits <- split(x = df_ibutton_data, f = df_ibutton_data$stand_number)

standsplits <- lapply(X = standsplits, FUN = function(sdf) {
  
  sdf$fire <- NA_character_
  predicted_fire <- predict(model_fire, sdf[, c("c", "w", "sdif", 
                                                 "lag_c", "lag_c2", "lag_c3", 
                                                 "lag_w", "lag_w2", 
                                                 "lag_sdif", "lag_sdif2", "lag_sdif3")])
  if (length(predicted_fire) > 0) {
    idxx <- which(complete.cases(sdf[, c("c", "w", "sdif", 
                                                 "lag_c", "lag_c2", "lag_c3", 
                                                 "lag_w", "lag_w2", 
                                                 "lag_sdif", "lag_sdif2", "lag_sdif3")]))
    if (length(idxx) > 0) {
      sdf[idxx, "fire"] <- predicted_fire
    }
  }
  
  print(table(sdf$fire))
  
  return(sdf)
})

# ---------------------------------------- #
# do some fine-tuning
standsplits <- lapply(X = standsplits, FUN = function(s) {
  s <- heg_vure(df = s, fireField = "fire", fireLabel = 2, noFireLabel = 1, minGapLength = 3)
  s <- wees_vure(df = s, fireField = "fire", fireLabel = 2, noFireLable = 1, minFireLength = 5)
  
  # add an ignition field
  s$ignition <- NA
  isPrevFire <- FALSE
  for (r in 1:nrow(s)) {
    if (is.na(s[r, "fire"])) {
      isPrevFire <- FALSE
      next
    }
    if (s[r, "fire"] == "1") {
      s[r, "ignition"] <- "no"
      isPrevFire <- FALSE
      next
    } #1 = "no", 2 = "yes"
    if (s[r, "fire"] == "2" & (isPrevFire == FALSE)) { 
      s[r, "ignition"] <- "yes"
      isPrevFire <- TRUE
      next
    }
    if (s[r, "fire"] == "yes" & (isPrevFire == TRUE)) { 
      s[r, "ignition"] <- "no"
      isPrevFire <- TRUE
      next
    }
  }
  print(table(s$ignition, exclude = NULL))
  
  return(s)
})

df_ibutton_data <- do.call("rbind", standsplits)
rm(standsplits)

# ---------------------------------------- #
# humanise it
df_ibutton_data$fire <- gsub(pattern = "1", replacement = "no", x = df_ibutton_data$fire, fixed = TRUE)
df_ibutton_data$fire <- gsub(pattern = "2", replacement = "yes", x = df_ibutton_data$fire, fixed = TRUE)

# ---------------------------------------- #
# save
df_ibutton_data_classified <- df_ibutton_data
rm(df_ibutton_data)
archive(fileName = "df_ibutton_data_classified.Rda", currentDir = rdadir, verbose = TRUE)
save(df_ibutton_data_classified, file = paste(rdadir, "df_ibutton_data_classified.Rda", sep = ""))

# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...

# rlengths <- rle(df_ibutton_data$fire)
# rlengths <- data.frame(rlengths$lengths, rlengths$values, stringsAsFactors = FALSE)
# rlenghts <- rlengths[which(rlengths$rlengths.values == "no"),]
# table(rlengths$rlengths.lengths)