# Date created: 11 November 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# Script to prepare a house hold list structure that organises the entire EOP database according to household
# ---------------------------------------- #
# start clean
rm(list = ls())
# ---------------------------------------- #
# source preamble
dropdir <- "C:/Users/Alex H/Dropbox (Nova SA)/"
projdir <- paste(dropdir, "Eskom Offset Study 2014 - Team/R/eop/", sep = "")
source(paste(projdir, "/Scripts/preamble.R", sep = ""))
# ---------------------------------------- #
# source additional functions/object definitions
source(paste(scriptdir, "EOPhhObj.R", sep = ""))
# ---------------------------------------- #
# load locally required libraries
library(doBy)
# ---------------------------------------- #
# declare script constants
VERBOSE <- TRUE
DEBUG <- TRUE
# ---------------------------------------- #
# ---------------------------------------- #

# add data from df_stands_info
load(paste(rdadir, "df_stands_info.Rda", sep = ""))

for (c in 1:ncol(df_stands_info)) {
  if (is.factor(df_stands_info[[c]])) {
    df_stands_info[[c]] <- as.character(df_stands_info[[c]])
  }
}

houseHoldList <- vector(mode = "list", length = nrow(df_stands_info))

for (r in 1:nrow(df_stands_info)) {
  temphh <- new.EOPhouseHold()
  temphh@address@standNumber <- df_stands_info[r, "stand_number"]
  temphh@address@extension <- df_stands_info[r, "extension"]
  temphh@hhMainMember <- df_stands_info[r, "respondent_name"]
  temphh@energyType <- df_stands_info[r, "energy_type"]
  temphh@insulationType <- df_stands_info[r, "insulation_type"]
  temphh@stoveNumber <- as.integer(df_stands_info[r, "stove_number"])
  temphh@elecMeterNumber <- df_stands_info[r, "meter_number"]
  temphh@address@mainPlace <- "KwaZamokuhle"
  temphh@contactNumber <- df_stands_info[r, "contact_number"]
  houseHoldList[[r]] <- temphh
  rm(temphh)
}

rm(df_stands_info)

# ---------------------------------------- #
# add data from df_ibutton_data_classified
load(paste(rdadir, "df_ibutton_data_classified.Rda", sep = ""))

for (c in 1:ncol(df_ibutton_data_classified)) {
  if (is.factor(df_ibutton_data_classified[[c]])) {
    df_ibutton_data_classified[[c]] <- as.character(df_ibutton_data_classified[[c]])
  }
}

standsplits <- split(x = df_ibutton_data_classified, f = df_ibutton_data_classified$stand_number)

houseHoldList <- lapply(X = houseHoldList, FUN = function(hh) {
  stnd <- hh@address@standNumber
  idx <- match(x = stnd, table = names(standsplits))
  if (is.na(idx)) {return(hh)}
  
  df <- standsplits[[idx]]
  
  # extract and save the metadata
  metaCols <- c("stand_number", "energy_type", "insulation_type")
  df_meta <- data.frame(matrix(nrow = 1, ncol = length(metaCols)), stringsAsFactors = FALSE)
  names(df_meta) <- metaCols
  
  for (c in 1:length(metaCols)) {
    colnm <- metaCols[c]
    uniq <- unique(df[[colnm]])
    uniq <- uniq[!is.na(uniq)]
    if (length(uniq) < 1) {
      uniq <- NA
    }
    if (length(uniq) > 1) {
      warning("More than one unique variable found for stand ", stnd, " in meta-field ", colnm, ": ", uniq,". Only the first will be used.")
      uniq <- uniq[[1]]
    }
    df_meta[1, colnm] <- uniq
  }
  
  hh@ibuttonData$meta_info <- df_meta
  rm(df_meta)
  
  # save the rest of the data
  df <- remove.cols(df = df, colNames = metaCols)
  hh@ibuttonData$data_df <- df
  rm(df)
  
  return(hh)
})

rm(standsplits, df_ibutton_data_classified)

# ---------------------------------------- #
load(paste(rdadir, "df_elec_data_all.Rda", sep = ""))

for (c in 1:ncol(df_elec_data_all)) {
  if (is.factor(df_elec_data_all[[c]])) {
    df_elec_data_all[[c]] <- as.character(df_elec_data_all[[c]])
  }
}

standsplits <- split(x = df_elec_data_all, f = df_elec_data_all$stand_number)

houseHoldList <- lapply(X = houseHoldList, FUN = function(hh) {
  stnd <- hh@address@standNumber
  idx <- match(x = stnd, table = names(standsplits))
  if (is.na(idx)) {return(hh)}
  
  df <- standsplits[[idx]]
  
  # extract and save the metadata
  metaCols <- c("stand_number", "extension", "user", "type", "supplier")
  df_meta <- data.frame(matrix(nrow = 1, ncol = length(metaCols)), stringsAsFactors = FALSE)
  names(df_meta) <- metaCols
  
  for (c in 1:length(metaCols)) {
    colnm <- metaCols[c]
    uniq <- unique(df[[colnm]])
    uniq <- uniq[!is.na(uniq)]
    if (length(uniq) < 1) {
      uniq <- NA
    }
    if (length(uniq) > 1) {
      warning("More than one unique variable found for stand ", stnd, " in field ", colnm, ": ", uniq, ". Only the first will be used.")
      uniq <- uniq[[1]]
    }
    
    df_meta[1, colnm] <- uniq
  }
  
  hh@elecData$meta_info <- df_meta
  rm(df_meta)
  
  # save the rest of the data
  df <- remove.cols(df = df, colNames = metaCols)
  hh@elecData$data_df <- df
  rm(df)
  
  return(hh)
})

rm(standsplits, df_elec_data_all)

# ---------------------------------------- #
load(paste(rdadir, "df_coal_logs.Rda", sep = ""))

for (c in 1:ncol(df_coal_logs)) {
  if (is.factor(df_coal_logs[[c]])) {
    df_coal_logs[[c]] <- as.character(df_coal_logs[[c]])
  } 
}

standsplits <- split(x = df_coal_logs, f = df_coal_logs$stand_number)

houseHoldList <- lapply(X = houseHoldList, FUN = function(hh) {
  stnd <- hh@address@standNumber
  idx <- match(x = stnd, table = names(standsplits))
  if (is.na(idx)) {return(hh)}
  
  df <- standsplits[[idx]]
  
  # extract and save meta data
  metaCols <- c("stand_number", "extension", "weight_of_wood_batch_kg", "weight_of_a_filled_container_kg")
  df_meta <- data.frame(matrix(nrow = 1, ncol = length(metaCols)), stringsAsFactors = FALSE)
  names(df_meta) <- metaCols
  
  for (c in 1:length(metaCols)) {
    colnm <- metaCols[c]
    uniq <- unique(df[[colnm]])
    uniq <- uniq[!is.na(uniq)]
    if (length(uniq) < 1) {
      uniq <- NA
    }
    if (length(uniq) > 1) {
      warning("More than one unique variable found for stand ", stnd, " in field ", colnm, ": ", uniq, ". Only the first will be used.")
      uniq <- uniq[[1]]
    }
    
    df_meta[1, colnm] <- uniq
  }
  
  hh@coalLog$meta_info <- df_meta
  rm(df_meta)
  
  # save the rest of the data
  df <- remove.cols(df = df, colNames = metaCols)
  hh@coalLog$data_df <- df
  rm(df)
  
  return(hh)
})

rm(standsplits, df_coal_logs)

# ---------------------------------------- #
load(paste(rdadir, "df_coal_data.Rda", sep = ""))
df_coal_data <- remove.col(df = df_coal_data, colName = "submission_id")

for (c in 1:ncol(df_coal_data)) {
  if (is.factor(df_coal_data[[c]])) {
    df_coal_data[[c]] <- as.character(df_coal_data[[c]])
  }
}

standsplits <- split(x = df_coal_data, f = df_coal_data$stand_number)

houseHoldList <- lapply(X = houseHoldList, FUN = function(hh) {
  stnd <- hh@address@standNumber
  idx <- match(x = stnd, table = names(standsplits))
  if (is.na(idx)) {return(hh)}

  df <- standsplits[[idx]]
  
  # extract and save metadata
  hh@address@latitude <- mean(df$latitude, na.rm = TRUE)
  hh@address@longitude <- mean(df$longitude, na.rm = TRUE)
  
  metaCols <- c("survey_version", "stand_number", "coal_buying_format", "other_format", "purchase_frequency")
  df_meta <- data.frame(matrix(nrow = 1, ncol = length(metaCols)), stringsAsFactors = FALSE)
  names(df_meta) <- metaCols
  
  for (c in 1:length(metaCols)) {
    colnm <- metaCols[c]
    uniq <- unique(df[[colnm]])
    uniq <- uniq[!is.na(uniq)]
    if (length(uniq) < 1) {
      uniq <- NA
    }
    if (length(uniq) > 1) {
      warning("More than one unique variable found for stand ", stnd, " in field ", colnm, ": ", uniq, ". Only the first will be used.")
      uniq <- uniq[[1]]
    }
    
    df_meta[1, colnm] <- uniq
  }
  
  hh@coalWeighingData$meta_info <- df_meta
  rm(df_meta)
  
  # save the rest of the data
  df <- remove.cols(df = df, colNames = c(metaCols, "latitude", "longitude"))
  hh@coalWeighingData$data_df <- df
  rm(df)
  
  return(hh)
})

rm(standsplits, df_coal_data)

# ---------------------------------------- #
# ---------------------------------------- #
# Now for the DES operations
# create more space in the DES field of each hh object

houseHoldList <- lapply(X = houseHoldList, FUN = function(hh) {
  hh@DESdata$meta_info <- vector(mode = "list", length = 2)
  hh@DESdata$data_df <- vector(mode = "list", length = 2)
  
  return(hh)
})

# ---------------------------------------- #
load(paste(rdadir, "des_data_pre.Rda", sep = ""))
load(paste(rdadir, "des_data_post.Rda", sep = ""))

desDfList <- list(df_des_data_pre, df_des_data_post)

for (desNumber in 1:2) {
  desDf <- desDfList[[desNumber]]

  desDf <- remove.col(df = desDf, colName = "submission_id")
  
  for (c in 1:ncol(desDf)) {
    if (is.factor(desDf[[c]])) {
      desDf[[c]] <- as.character(desDf[[c]])
    }
  }

  metaCols <- c("stand_number", "household_structure_number", "latitude", "longitude", "place", "date", "respondent_name", "respondent_household_surname", "fieldworker_name", "fieldworker_id", "respondent_phone", "respondent_stand_households_number", "resident_9months", "respondent_age", "survey_version")
  metaCols <- metaCols[which(metaCols %in% names(desDf))]
  
  standsplits <- split(x = desDf, f = desDf$stand_number)
  
  houseHoldList <- lapply(X = houseHoldList, FUN = function(hh) {
    stnd <- hh@address@standNumber
    
    idx <- match(x = stnd, table = names(standsplits))
    if (is.na(idx)) {return(hh)}
  
    df <- standsplits[[idx]]
    
    # extract and save metadata
    df_meta <- data.frame(matrix(nrow = 1, ncol = length(metaCols)), stringsAsFactors = FALSE)
    names(df_meta) <- metaCols
    
    for (c in 1:length(metaCols)) {
      colnm <- metaCols[c]
      uniq <- unique(df[[colnm]])
      uniq <- uniq[!is.na(uniq)]
      if (length(uniq) < 1) {
        uniq <- NA
      }
      if (length(uniq) > 1) {
        warning("More than one unique variable found for stand ", stnd, " in field ", colnm, ": ", uniq, ". Only the first will be used.")
        uniq <- uniq[[1]]
      }
      
      df_meta[1, colnm] <- uniq
    }
    
    hh@DESdata$meta_info[[desNumber]] <- df_meta
    rm(df_meta)
    
    # save the rest of the data
    df <- remove.cols(df = df, colNames = metaCols)
    hh@DESdata$data_df[[desNumber]] <- df
    rm(df)
    
    return(hh)
  })
  
  rm(desDf, metaCols, standsplits)
}

# ---------------------------------------- #
# ---------------------------------------- #

archive(fileName = "houseHoldList.Rda", currentDir = rdadir, verbose = TRUE)
save(houseHoldList, file = paste(rdadir, "houseHoldList.Rda", sep = ""))







# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...
## ------------------- ##
