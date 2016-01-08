# Date created: 19 November 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# Script to prepare the stats from the DES that Nuveshen requested per e-mail on 18 Nov 2015.
# ---------------------------------------- #
# start clean
rm(list = ls())
# ---------------------------------------- #
# source preamble
dropdir <- "C:/Users/Alex H/Dropbox (Nova SA)/"
projdir <- projdir <- paste(dropdir, "Eskom Offset Study 2014 - Team/R/eop/", sep = "")
source(paste(projdir, "Scripts/preamble.R", sep = ""))
# ---------------------------------------- #
# declare script constants
VERBOSE <- TRUE
DEBUG <- TRUE
WOOD_UNIT_WEIGHT <- 35 #kg
# ---------------------------------------- #
# ---------------------------------------- #

# load the data
load(paste(rdadir, "houseHoldList.Rda", sep = ""))

# split the data by intervention type
coalBasic <- vector(mode = "list")
coalFull <- vector(mode = "list")
elecBasic <- vector(mode = "list")
elecFull <- vector(mode = "list")
lpgBasic <- vector(mode = "list")
lpgFull <- vector(mode = "list")
controlNone <- vector(mode = "list")

for (i in 1:length(houseHoldList)) {
  hh <- houseHoldList[[i]]
  if (hh@energyType == "coal" & hh@insulationType == "basic") {
    coalBasic <- c(coalBasic, hh)
    next
  }
  if (hh@energyType == "coal" & hh@insulationType == "full") {
    coalFull <- c(coalFull, hh)
    next
  }
  if (hh@energyType == "elec" & hh@insulationType == "basic") {
    elecBasic <- c(elecBasic, hh)
    next
  }
  if (hh@energyType == "elec" & hh@insulationType == "full") {
    elecFull <- c(elecFull, hh)
    next
  }
  if (hh@energyType == "lpg" & hh@insulationType == "basic") {
    lpgBasic <- c(lpgBasic, hh)
    next
  }
  if (hh@energyType == "lpg" & hh@insulationType == "full") {
    lpgFull <- c(lpgFull, hh)
    next
  }
  if (hh@energyType == "control" & hh@insulationType == "none") {
    controlNone <- c(controlNone, hh)
    next
  }
  
  warning("Undefined type encountered at stand ", hh@address@standNumber, ".")
}

typeList <- list(coalBasic, coalFull, elecBasic, elecFull, lpgBasic, lpgFull, controlNone)
names(typeList) <- c("coalBasic", "coalFull", "elecBasic", "elecFull", "lpgBasic", "lpgFull", "controlNone")

hhStatStruct <- data.frame(matrix(nrow = 2, ncol = 13), stringsAsFactors = FALSE)
names(hhStatStruct) <- c("stand_number", "hh_struct_num", "season", "coal_pre", "coal_post", "wood_pre", "wood_post", "lpg_pre", "lpg_post", "elec_pre", "elec_post", "paraffin_pre", "paraffin_post")
hhStatStruct[["season"]] <- c("winter", "summer")

typeStats <- lapply(X = typeList, FUN = function(tp) {
  
  hhStats <- lapply(X = tp, FUN = function(hh) {
    statFrame <- hhStatStruct
    
    # extract the stand number
    statFrame[["stand_number"]] <- hh@address@standNumber
    
    # extract the structure number
    structNum <- sapply(X = hh@DESdata$meta_info, FUN = function(mdf) {
      if (is.null(mdf)) {return(NA)}
      return(mdf$household_structure_number)
    })
    structNum <- unique(structNum)
    structNum <- structNum[!is.na(structNum)]
    if (length(structNum) < 1) {
      warning("No unique structNum found for stand ", hh@address@standNumber, ".")
      structNum <- NA
    }
    if (length(structNum) > 1) {
      warning("More than one unique structNum found for stand ", hh@address@standNumber, ": ", structNum, ". Only the first will be used.")
      structNum <- structNum[[1]]
    }
    
    statFrame[["hh_struct_num"]] <- structNum
    
    # extract data from DES_pre - preference given to pre_2, but if not available, use pre_1
    desDfPre <- NULL
    if (!is.null(hh@DESdata$data_df[[2]])) {
      desDfPre <- hh@DESdata$data_df[[2]]
    } else {
      if (!is.null(hh@DESdata$data_df[[1]])) {
      desDfPre <- hh@DESdata$data_df[[1]]  
      }
    }
    
    if (is.null(desDfPre)) {
      statFrame[, c("coal_pre", "wood_pre", "lpg_pre", "elec_pre", "paraffin_pre")] <- NA
    } else {
      
      coalWinter <- unique(format.numeric(desDfPre$energy_coal_consumption_wintercurrentunits))
      coalWinter <- coalWinter[!is.na(coalWinter)]
      if (length(coalWinter) < 1) {coalWinter <- NA}
      if (length(coalWinter) > 1) {
        warning("More than one unique coalWinter (pre) found for stand ", 
                hh@address@standNumber, ": ", coalWinter, ". Only the first will be used.")
        coalWinter <- coalWinter[[1]]
      }
      statFrame[which(statFrame$season == "winter"), "coal_pre"] <- coalWinter*4
      
      coalSummer <- unique(format.numeric(desDfPre$energy_coal_consumption_summercurrentunits))
      coalSummer <- coalSummer[!is.na(coalSummer)]
      if (length(coalSummer) < 1) {coalSummer <- NA}
      if (length(coalSummer) > 1) {
        warning("More than one unique coalSummer (pre) found for stand ", 
                hh@address@standNumber, ": ", coalSummer, ". Only the first will be used.")
        coalSummer <- coalSummer[[1]]
      }
      statFrame[which(statFrame$season == "summer"), "coal_pre"] <- coalSummer*8
      
      if (is.na(desDfPre$energy_coal_format)) {statFrame$coal_pre <- statFrame$coal_pre*50} else {
      if (desDfPre$energy_coal_format == "small_bag_about_25kg") {statFrame$coal_pre <- statFrame$coal_pre*25} else {
      if (desDfPre$energy_coal_format == "big_bag_about_50kg_or_more") {statFrame$coal_pre <- statFrame$coal_pre*50} else {
      if (desDfPre$energy_coal_format == "tin") {statFrame$coal_pre <- statFrame$coal_pre*17.5}}}}
      
      woodWinter <- unique(format.numeric(desDfPre$energy_wood_consumption_wintercurrentunits))
      woodWinter <- woodWinter[!is.na(woodWinter)]
      if (length(woodWinter) < 1) {woodWinter <- NA}
      if (length(woodWinter) > 1) {
        warning("More than one unique woodWinter (pre) found for stand ", 
                hh@address@standNumber, ": ", woodWinter, ". Only the first will be used.")
        woodWinter <- woodWinter[[1]]
      }
      statFrame[which(statFrame$season == "winter"), "wood_pre"] <- woodWinter*4*WOOD_UNIT_WEIGHT
      
      woodSummer <- unique(format.numeric(desDfPre$energy_wood_consumption_summercurrentunits))
      woodSummer <- woodSummer[!is.na(woodSummer)]
      if (length(woodSummer) < 1) {woodSummer <- NA}
      if (length(woodSummer) > 1) {
        warning("More than one unique woodSummer (pre) found for stand ", 
                hh@address@standNumber, ": ", woodSummer, ". Only the first will be used.")
        woodSummer <- woodSummer[[1]]
      }
      statFrame[which(statFrame$season == "summer"), "wood_pre"] <- woodSummer*8*WOOD_UNIT_WEIGHT
      
      lpgWinter <- unique(format.numeric(desDfPre$energy_lpg_comsumption_kgwintermonth))
      lpgWinter <- lpgWinter[!is.na(lpgWinter)] 
      if (length(lpgWinter) < 1) {lpgWinter <- NA}
      if (length(lpgWinter) > 1) {
        warning("More than one unique lpgWinter (pre) found for stand ", 
                hh@address@standNumber, ": ", lpgWinter, ". Only the first will be used.")
        lpgWinter <- lpgWinter[[1]]
      }
      statFrame[which(statFrame$season == "winter"), "lpg_pre"] <- lpgWinter*4

      lpgSummer <- unique(format.numeric(desDfPre$energy_lpg_comsumption_kgsummermonth))
      lpgSummer <- lpgSummer[!is.na(lpgSummer)]
      if (length(lpgSummer) < 1) {lpgSummer <- NA}
      if (length(lpgSummer) > 1) {
        warning("More than one unique lpgSummer (pre) found for stand ", 
                hh@address@standNumber, ": ", lpgSummer, ". Only the first will be used.")
        lpgSummer <- lpgSummer[[1]]
      }
      statFrame[which(statFrame$season == "summer"), "lpg_pre"] <- lpgSummer*8
      
      elecWinter <- NA
      statFrame[which(statFrame$season == "winter"), "elec_pre"] <- elecWinter
      elecSummer <- NA
      statFrame[which(statFrame$season == "summer"), "elec_pre"] <- elecSummer
      
      paraffinWinter <- unique(format.numeric(desDfPre$energy_paraffin_comsumption_litreswinterweek))
      paraffinWinter <- paraffinWinter[!is.na(paraffinWinter)]
      if (length(paraffinWinter) < 1) {paraffinWinter <- NA}
      if (length(paraffinWinter) > 1) {
        warning("More than one unique paraffinWinter (pre) found for stand ", 
                hh@address@standNumber, ": ", paraffinWinter, ". Only the first will be used.")
        paraffinWinter <- paraffinWinter[[1]]
      }
      statFrame[which(statFrame$season == "winter"), "paraffin_pre"] <- paraffinWinter*17
      
      paraffinSummer <- unique(format.numeric(desDfPre$energy_paraffin_comsumption_litressummerweek))
      paraffinSummer <- paraffinSummer[!is.na(paraffinSummer)]
      if (length(paraffinSummer) < 1) {paraffinSummer <- NA}
      if (length(paraffinSummer) > 1) {
        warning("More than one unique paraffinSummer (pre) found for stand ", 
                hh@address@standNumber, ": ", paraffinSummer, ". Only the first will be used.")
        paraffinSummer <- paraffinSummer[[1]]
      }
      statFrame[which(statFrame$season == "summer"), "paraffin_pre"] <- paraffinSummer*35
      
    }
      
    # extract data from DES_post
    if (is.null(hh@DESdata$data_df[[3]])) {
      statFrame[, c("coal_post", "wood_post", "lpg_post", "elec_post", "paraffin_post")] <- NA
      
    } else {
      dfDesPost <- hh@DESdata$data_df[[3]]
      
      coalWinter <- unique(format.numeric(dfDesPost$energy_coal_consumption_wintercurrentunits))
      coalWinter <- coalWinter[!is.na(coalWinter)]
      if (length(coalWinter) < 1) {coalWinter <- NA}
      if (length(coalWinter) > 1) {
        warning("More than one unique coalWinter (post) found for stand ", 
                hh@address@standNumber, ": ", coalWinter, ". Only the first will be used.")
        coalWinter <- coalWinter[[1]]
      }
      statFrame[which(statFrame$season == "winter"), "coal_post"] <- coalWinter*4
      
      coalSummer <- unique(format.numeric(dfDesPost$energy_coal_consumption_summercurrentunits))
      coalSummer <- coalSummer[!is.na(coalSummer)]
      if (length(coalSummer) < 1) {coalSummer <- NA}
      if (length(coalSummer) > 1) {
        warning("More than one unique coalSummer (post) found for stand ", 
                hh@address@standNumber, ": ", coalSummer, ". Only the first will be used.")
        coalSummer <- coalSummer[[1]]
      }
      statFrame[which(statFrame$season == "summer"), "coal_post"] <- coalSummer*8
      
      if (is.na(dfDesPost$energy_coal_format)) {statFrame$coal_post <- statFrame$coal_post*50} else {
      if (dfDesPost$energy_coal_format == "small_bag_about_25kg") {statFrame$coal_post <- statFrame$coal_post*25} else {
      if (dfDesPost$energy_coal_format == "big_bag_about_50kg_or_more") {statFrame$coal_post <- statFrame$coal_post*50} else {
      if (dfDesPost$energy_coal_format == "tin") {statFrame$coal_post <- statFrame$coal_post*17.5}}}}
      
      woodWinter <- unique(format.numeric(dfDesPost$energy_wood_consumption_wintercurrentunits))
      woodWinter <- woodWinter[!is.na(woodWinter)]
      if (length(woodWinter) < 1) {woodWinter <- NA}
      if (length(woodWinter) > 1) {
        warning("More than one unique woodWinter (post) found for stand ", 
                hh@address@standNumber, ": ", woodWinter, ". Only the first will be used.")
        woodWinter <- woodWinter[[1]]
      }
      statFrame[which(statFrame$season == "winter"), "wood_post"] <- woodWinter*4*WOOD_UNIT_WEIGHT
      
      woodSummer <- unique(format.numeric(dfDesPost$energy_wood_consumption_summercurrentunits))
      woodSummer <- woodSummer[!is.na(woodSummer)]
      if (length(woodSummer) < 1) {woodSummer <- NA}
      if (length(woodSummer) > 1) {
        warning("More than one unique woodSummer (post) found for stand ", 
                hh@address@standNumber, ": ", woodSummer, ". Only the first will be used.")
        woodSummer <- woodSummer[[1]]
      }
      statFrame[which(statFrame$season == "summer"), "wood_post"] <- woodSummer*8*WOOD_UNIT_WEIGHT
      
      lpgWinter <- unique(format.numeric(dfDesPost$energy_lpg_comsumption_kgwintermonth))
      lpgWinter <- lpgWinter[!is.na(lpgWinter)] 
      if (length(lpgWinter) < 1) {lpgWinter <- NA}
      if (length(lpgWinter) > 1) {
        warning("More than one unique lpgWinter (post) found for stand ", 
                hh@address@standNumber, ": ", lpgWinter, ". Only the first will be used.")
        lpgWinter <- lpgWinter[[1]]
      }
      statFrame[which(statFrame$season == "winter"), "lpg_post"] <- lpgWinter*4

      lpgSummer <- unique(format.numeric(dfDesPost$energy_lpg_comsumption_kgsummermonth))
      lpgSummer <- lpgSummer[!is.na(lpgSummer)]
      if (length(lpgSummer) < 1) {lpgSummer <- NA}
      if (length(lpgSummer) > 1) {
        warning("More than one unique lpgSummer (post) found for stand ", 
                hh@address@standNumber, ": ", lpgSummer, ". Only the first will be used.")
        lpgSummer <- lpgSummer[[1]]
      }
      statFrame[which(statFrame$season == "summer"), "lpg_post"] <- lpgSummer*8
      
      elecWinter <- unique(format.char(dfDesPost$energy_cost_winter_electricity))
      elecWinter <- elecWinter[!is.na(elecWinter)]
      if (length(elecWinter) < 1) {elecWinter <- NA}
      if (length(elecWinter) > 1) {
        warning("More than one unique elecWinter (post) found for stand ", 
                hh@address@standNumber, ": ", elecWinter, ". Only the first will be used.")
        elecWinter <- elecWinter[[1]]
      }
      statFrame[which(statFrame$season == "winter"), "elec_post"] <- elecWinter
      
      elecSummer <- unique(format.char(dfDesPost$energy_cost_summer_electricity))
      elecSummer <- elecSummer[!is.na(elecSummer)]
      if (length(elecSummer) < 1) {elecSummer <- NA}
      if (length(elecSummer) > 1) {
        warning("More than one unique elecSummer (post) found for stand ", 
                hh@address@standNumber, ": ", elecSummer, ". Only the first will be used.")
        elecSummer <- elecSummer[[1]]
      }
      statFrame[which(statFrame$season == "summer"), "elec_post"] <- elecSummer
      
      paraffinWinter <- unique(format.numeric(dfDesPost$energy_paraffin_comsumption_litreswinterweek))
      paraffinWinter <- paraffinWinter[!is.na(paraffinWinter)]
      if (length(paraffinWinter) < 1) {paraffinWinter <- NA}
      if (length(paraffinWinter) > 1) {
        warning("More than one unique paraffinWinter (post) found for stand ", 
                hh@address@standNumber, ": ", paraffinWinter, ". Only the first will be used.")
        paraffinWinter <- paraffinWinter[[1]]
      }
      statFrame[which(statFrame$season == "winter"), "paraffin_post"] <- paraffinWinter*17
      
      paraffinSummer <- unique(format.numeric(dfDesPost$energy_paraffin_comsumption_litressummerweek))
      paraffinSummer <- paraffinSummer[!is.na(paraffinSummer)]
      if (length(paraffinSummer) < 1) {paraffinSummer <- NA}
      if (length(paraffinSummer) > 1) {
        warning("More than one unique paraffinSummer (post) found for stand ", 
                hh@address@standNumber, ": ", paraffinSummer, ". Only the first will be used.")
        paraffinSummer <- paraffinSummer[[1]]
      }
      statFrame[which(statFrame$season == "summer"), "paraffin_post"] <- paraffinSummer*35
      
    }
    
    return(statFrame)
  })
  
  hhStats <- do.call("rbind", hhStats)
  
  return(hhStats)
})

# fix the elec_post field
typeStats <- lapply(X = typeStats, FUN = function(tpdf) {
  temp <- tpdf$elec_post
  if (all(is.na(temp))) {return(tpdf)}
  
  temp <- strsplit(x = temp, split = "_", fixed = TRUE)
  
  tempMin <- sapply(X = temp, FUN = function(e) {e[[1]]})
  tempMax <- sapply(X = temp, FUN = function(e) {if (length(e) > 1) {return(e[[2]])} else {return(NA)}})
  
  tempMin <- format.int(tempMin)
  tempMax <- format.int(tempMax)
  
  temp <- apply(X = as.array(1:length(tempMin)), MARGIN = 1, FUN = function(idx) {
    mn <- mean(c(tempMin[idx], tempMax[idx]), na.rm = TRUE)
    mn <- ifelse(is.nan(mn), NA, mn)
    return(mn)
  }) 
  
  tpdf$elec_post <- temp
  rm(temp)
  
  tpdf[which(tpdf$season == "winter"), "elec_post"] <- (tpdf[which(tpdf$season == "winter"), "elec_post"]) *4
  tpdf[which(tpdf$season == "summer"), "elec_post"] <- (tpdf[which(tpdf$season == "summer"), "elec_post"]) *8
  
  return(tpdf)
})

# determine the number of non-missing values in each field as well as the sum and avg of that particular field
typeStats_summed <- lapply(X = typeStats, FUN = function(tpdf) {
  sums <- data.frame(matrix(nrow = 1, ncol = ncol(tpdf)), stringsAsFactors = FALSE)
  sums[,1] <- c("avg_annual")
  names(sums) <- names(tpdf)
  
  for (c in 4:ncol(tpdf)) {
    n_summer <- length(which(!is.na(tpdf[which(tpdf$season == "summer"),c])))
    tot_summer <- sum(tpdf[which(tpdf$season == "summer"),c], na.rm = TRUE)
    avg_summer <- tot_summer / n_summer
    n_winter <- length(which(!is.na(tpdf[which(tpdf$season == "winter"),c])))
    tot_winter <- sum(tpdf[which(tpdf$season == "winter"),c], na.rm = TRUE)
    avg_winter <- tot_winter / n_winter
    avg_annual <- avg_summer + avg_winter
    sums[, c] <- avg_annual
  }
  
  sums <- remove.cols(df = sums, colNames = c("hh_struct_num", "season"))
  names(sums)[which(names(sums) == "stand_number")] <- "variable"
  return(sums)
})

typeStats_summed <- apply(X = as.array(names(typeStats_summed)), MARGIN = 1, FUN = function(tnm) {
  tpdf <- typeStats_summed[[tnm]]
  tpdf$type <- tnm
  tpdf <- move.col(df = tpdf, colName = "type", colIdx = 1)
  return(tpdf)
})

typeStats_summed <- do.call("rbind", typeStats_summed)



typeStats_summed <- do.call("rbind.fill", typeStats_summed)
units <- data.frame(matrix(nrow = 1, data = c(rep.int(NA, 3), rep.int(x = "kg/annum", times = 6), "R/annum", rep.int(x = "l/annum", times = 2))), stringsAsFactors = FALSE)
names(units) <- names(typeStats_summed)
typeStats_summed <- rbind.fill(units, typeStats_summed)

typeStats_perHh_summed <- typeStats_summed
for (r in 2:nrow(typeStats_perHh_summed)) {
  n <- as.numeric(typeStats_perHh_summed[r, "n"])
  for (c in 4:ncol(typeStats_perHh_summed)) {
    if (!is.na(typeStats_perHh_summed[r,c])) {
      typeStats_perHh_summed[r,c] <- as.numeric(typeStats_perHh_summed[r,c]) / n
    }
  }
}

  
# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...
## ------------------- ##

# sapply(X = houseHoldList, FUN = function(hh) {
#  if (is.null(hh@DESdata$data_df[[1]]) & is.null(hh@DESdata$data_df[[2]])) {
#    return("bad")
#   } else {return("good")}
# })
