# Date created: 01 December 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# Script to prepare the coal and wood usage data from the logs and ibuttons, required by Michael from eScience
# ---------------------------------------- #
# start clean
rm(list = ls())
# ---------------------------------------- #
# source preamble
dropdir <- "C:/Users/Alex H/Dropbox (Nova SA)/"
#dropdir <- "C:/Users/nova/Dropbox/"
projdir <- paste(dropdir, "Eskom Offset Study 2014 - Team/R/eop/", sep = "")
source(paste(projdir, "Scripts/preamble.R", sep = ""))
# ---------------------------------------- #
# declare script constants
VERBOSE <- TRUE
DEBUG <- TRUE
COAL_WEIGHTS  <- c(small_bag_about_25kg = 43,
                   big_bag_about_50kg_or_more = 52,
                   tin = 17,
                   other = 47)
WOOD_WEIGHT <- 35
# ---------------------------------------- #
# source locally required functions and definitions
source(paste(scriptdir, "EOPhhObj.R", sep = ""))
# ---------------------------------------- #
# define auxiliary functions

summariseLog <- function(hh = NULL, var = c("coal", "wood", "ignitions"), summariseBy = c("pfire", "pday", "pweek", "pmonth")) {
  if (is.null(hh)) {
    warning("NULL argument received by parameter 'hh'. Returning NULL")
    return(NULL)
  }
  
  if (length(var) < 1) {
    warning("No argument received by parameter 'var'. Defaulting to 'coal'.")
    var <- "coal"
  }
  if (length(var) > 1) {
    warning("More than one argument received by parameter 'var'. Can only specify one. Defaulting to 'coal'")
    var <- "coal"
  }
  if (!(var %in% c("coal", "wood", "ignitions")))  {
    stop("Argument for 'var' can only be 'coal', 'wood' or 'ignitions'.")
  }
  
  if (length(summariseBy) < 1) {
    warning("No argument received by parameter 'summariseBy'. Defaulting to 'pfire'.")
    summariseBy <- "pfire"
  }
  if (length(summariseBy) > 1) {
    warning("More than one argument received by parameter 'summariseBy'. Can only specify one. Defaulting to 'pfire'")
    summariseBy <- "pfire"
  }
  if (!(summariseBy %in% c("pfire", "pday", "pweek", "pmonth")))  {
    stop("Argument for 'summariseBy' can only be 'pfire', 'pday', 'pweek' or 'pmonth'.")
  }
  
  ## ---------- ##
  dfCoalLog <- hh@coalLog$data_df
  if (is.null(dfCoalLog)) {
    if (hh@energyType == "lpg") {return(0)}
    warning("No log data found for hh. Returning NULL.")
    return(NULL)
  }
  
  ## ---------- ##
  # add a fire_number field to the log data
  dfCoalLog$fire_number <- NA
  fireCounter <- 0L
  for (r in 1:nrow(dfCoalLog)) {
    if (is.na(dfCoalLog[r, "new_or_refill"])) {
      warning("NA encountered in field 'new_or_refill'.")
      next
    }
    if (dfCoalLog[r, "new_or_refill"] == "new") {
      fireCounter <- fireCounter + 1
    }
    dfCoalLog[r, "fire_number"] <- fireCounter
  }
  
  ## ---------- ##
  # add a yrday field as 
  dfCoalLog$yrday <- yday(dfCoalLog$date)
  
  ## ---------- ##
  # add other required fields, if necessary
  if (var == "coal") {
    dfCoalLog$total_coal <- dfCoalLog$no_of_coal_containers * hh@coalLog$meta_info$weight_of_a_filled_container_kg
  }
  if (var == "wood") {
    dfCoalLog$total_wood <- dfCoalLog$no_of_wood_batches * hh@coalLog$meta_info$weight_of_wood_batch_kg
  }

  ## ---------- ##
  ## calculate and return
  if (summariseBy == "pfire") {
    x <- switch(var, 
                "coal" = (dplyr::summarise(group_by(dfCoalLog, fire_number), mn = mean(total_coal)))[["mn"]],
                "wood" = (dplyr::summarise(group_by(dfCoalLog, fire_number), mn = mean(total_wood)))[["mn"]],
                "ignitions" = group_size(group_by(dfCoalLog, fire_number)))
    return(mean(x, na.rm = TRUE))
  }
  
  dfCoalLog <- group_by(dfCoalLog, yrday)
  x <- switch(var, 
                "coal" = (dplyr::summarise(dfCoalLog, mn = mean(total_coal, na.rm = TRUE)))[["mn"]],
                "wood" = (dplyr::summarise(dfCoalLog, mn = mean(total_wood, na.rm = TRUE)))[["mn"]],
                "ignitions" = group_size(dfCoalLog))
  x <- mean(x, na.rm = TRUE)
  x <- switch(summariseBy, 
              "pday" = x,
              "pweek" = x*7,
              "pmonth" = x*30.4375)
  return(x)
}

summariseIbuttons <- function(hh = NULL, 
                              var = c("ignitions"), 
                              summariseBy = c("pfire", "pday", "pweek", "pmonth"),
                              season = c("winter", "summer")) {
  if (is.null(hh)) {
    warning("NULL argument received by parameter 'hh'. Returning NA")
    return(NA)
  }
  
  if (length(var) < 1) {
    warning("No argument received by parameter 'var'. Defaulting to 'ignitions'.")
    var <- "ignitions"
  }
  if (length(var) > 1) {
    warning("More than one argument received by parameter 'var'. Can only specify one. Defaulting to 'ignitions'")
    var <- "ignitions"
  }
  if (!(var %in% c("ignitions")))  {
    stop("Argument for 'var' can only be 'ignitions'.")
  }
  
  if (hh@energyType == "lpg") {return(0)}
  
  if (length(summariseBy) < 1) {
    warning("No argument received by parameter 'summariseBy'. Defaulting to 'pfire'.")
    summariseBy <- "pfire"
  }
  if (length(summariseBy) > 1) {
    warning("More than one argument received by parameter 'summariseBy'. Can only specify one. Defaulting to 'pfire'")
    summariseBy <- "pfire"
  }
  if (!(summariseBy %in% c("pfire", "pday", "pweek", "pmonth")))  {
    stop("Argument for 'summariseBy' can only be 'pfire', 'pday', 'pweek' or 'pmonth'.")
  }
  
  ## ---------- ##
  dfIbutton <- hh@ibuttonData$data_df
  if (is.null(dfIbutton)) {
    if (hh@energyType == "lpg") {return(0)}
    warning("No ibutton data found for hh. Returning NA.")
    return(NA)
  }
  
  if ("season" %in% names(dfIbutton)) {
    if (length(season) == 1) {
      if (season %in% c("winter", "summer")) {
        dfIbutton <- dfIbutton[which(dfIbutton$season == season),]
        if (nrow(dfIbutton) < 1) {
          warning("No ibutton data left after seasonal subsetting. Returning NA.")
          return(NA_real_)
        }
      }
    }
  }
  
  ## ---------- ##
  # add an 'ignition' field to the data to mark the moments of ignition
  isPrevFire <- FALSE
  dfIbutton$ignition <- NA
  for (r in 1:nrow(dfIbutton)) {
    if (is.na(dfIbutton[r, "fire"])) {
      isPrevFire <- FALSE
      next
    }
    if (dfIbutton[r, "fire"] == "no") {
      dfIbutton[r, "ignition"] <- 0
      isPrevFire <- FALSE
      next
    }
    if (dfIbutton[r, "fire"] == "yes") {
      dfIbutton[r, "ignition"] <- ifelse(isPrevFire, 0, 1)
      isPrevFire <- TRUE
      next
    }
    message("Uh-oh.")
  }
  
  ## ---------- ##
  # calculate and return
  if (summariseBy == "pfire") {
    return(1) # for now at least, because we know that our machine cannot classify refills, 
    # so only one ignition per fire is possible at this stage
  }
  
  dfIbutton$yrday <- yday(dfIbutton$date)
  dfIbutton <- group_by(dfIbutton, yrday)
  answer <- (dplyr::summarise(dfIbutton, sm = sum(ignition)))[["sm"]]
  
  answer <- switch(summariseBy,
                   "pday" = mean(answer),
                   "pweek" = mean(answer) * 7,
                   "pmonth" = mean(answer) * 30.4375)
  return(answer)
}

doIt <- function(VAR = "coal", 
                 PER = "pmonth", 
                 viewRaw = FALSE, 
                 doAnova = TRUE, 
                 season = "winter") {
  
  nrows <- ifelse(VAR == "ignitions", 
                  length(houseHoldList) * 2, 
                  length(houseHoldList) * 4)
  infoStruct <- data.frame(matrix(nrow = nrows, ncol = 4), 
                           stringsAsFactors = FALSE)
  colnames(infoStruct) <- c("value", "energy_type", 
                            "insulation_type", 
                            "source")
  
  r <- 0
  
  ctch <- lapply(X = houseHoldList, FUN = function(hh) {
    
    print(hh@address@standNumber)
    
    # gather info from the logs
    r <<- r + 1
    x <- summariseLog(hh = hh, var = VAR, summariseBy = PER)
    infoStruct[r, "energy_type"] <<- hh@energyType
    infoStruct[r, "insulation_type"] <<- hh@insulationType
    infoStruct[r, "value"] <<- ifelse(is.null(x), NA, x)
    infoStruct[r, "source"] <<- "logs"
  
    # gather info from the ibuttons
    r <<- r + 1
    x <- summariseIbuttons(hh = hh, 
                           var = "ignitions", 
                           summariseBy = PER, 
                           season = season)
    infoStruct[r, "energy_type"] <<- hh@energyType
    infoStruct[r, "insulation_type"] <<- hh@insulationType
    if (VAR %in% c("coal", "wood")) {
      avgPerFire <- summariseLog(hh = hh, var = VAR, summariseBy = "pfire")
      if (is.null(avgPerFire)) {avgPerFire <- NA}
    } else {
      avgPerFire <- 1
    }
    infoStruct[r, "value"] <<- (ifelse(is.null(x), NA, x)) * avgPerFire
    infoStruct[r, "source"] <<- "ibuttons"
    
    if (VAR == "ignitions") {rm(x, hh); return(0)}
    
    # gather info from DES[[1]]
    r <<- r + 1
    infoStruct[r, "energy_type"] <<- hh@energyType
    infoStruct[r, "insulation_type"] <<- hh@insulationType
    infoStruct[r, "source"] <<- "des1"
    dfDes <- hh@DESdata$data_df[[1]]
    if (is.null(dfDes)) {x <- NA} else {
      
      unitWeight <- switch(VAR, 
                           "coal" = {f <- unnada(dfDes$energy_coal_format)
                                     ifelse(is.na(f), 0, COAL_WEIGHTS[f])}, 
                           "wood" = WOOD_WEIGHT)
      div <- switch(PER,
                    "pfire" = {igns <- summariseIbuttons(hh = hh, 
                                                         var = "ignitions", 
                                                         summariseBy = "pmonth", 
                                                         season = season)
                               ifelse(is.null(igns), NA, igns)},
                    "pday" = 30.5,
                    "pweek" = 4.4,
                    "pmonth" = 1)
      
      desVar <- switch(VAR, 
                       "coal" = "energy_coal_consumption_wintercurrentunits",
                       "wood" = "energy_wood_consumption_wintercurrentunits")
      
      
      temp <- as.numeric(unnada(dfDes[[desVar]]))
      temp <- ifelse(length(temp) > 0, temp, 0)
      temp <- ifelse(is.na(temp), 0, temp)
      consumptVar <- temp

      x <- ifelse((consumptVar == 0 & unitWeight == 0), 
                  0, (consumptVar * unitWeight / div))
      
    }
    infoStruct[r, "value"] <<- x
    
    # gather info from DES[[2]]
    r <<- r + 1
    infoStruct[r, "energy_type"] <<- hh@energyType
    infoStruct[r, "insulation_type"] <<- hh@insulationType
    infoStruct[r, "source"] <<- "des2"
    dfDes <- hh@DESdata$data_df[[2]]
    if (is.null(dfDes)) {x <- NA} else {
      
      unitWeight <- switch(VAR, 
                           "coal" = {f <- unnada(dfDes$energy_coal_format)
                                     ifelse(is.na(f), 0, COAL_WEIGHTS[f])}, 
                           "wood" = WOOD_WEIGHT)
      div <- switch(PER,
                    "pfire" = {igns <- summariseIbuttons(hh = hh, 
                                                         var = "ignitions", 
                                                         summariseBy = "pmonth",
                                                         season = season)
                               ifelse(is.null(igns), NA, igns)},
                    "pday" = 30.5,
                    "pweek" = 4.4,
                    "pmonth" = 1)
      
      desVar <- switch(VAR, 
                       "coal" = "energy_coal_consumption_wintercurrentunits",
                       "wood" = "energy_wood_consumption_wintercurrentunits")
      
      
      temp <- as.numeric(unnada(dfDes[[desVar]]))
      temp <- ifelse(length(temp) > 0, temp, 0)
      temp <- ifelse(is.na(temp), 0, temp)
      consumptVar <- temp; rm(temp)

      x <- ifelse((consumptVar == 0 & unitWeight == 0), 
                  0, (consumptVar * unitWeight / div))

    }
    infoStruct[r, "value"] <<- x
  
    rm(x, dfDes, hh)
  })
  rm(ctch) 
  
  if (doAnova) {
    for (k in 1:2) {print(" ", quote = FALSE)}
    print("for source = DES_1:")
    print(anova(aov(value ~ energy_type + insulation_type, 
                    data = infoStruct[which(infoStruct$source == "des1"),])))
    for (k in 1:2) {print(" ", quote = FALSE)}
    print("for source = DES_2:")
    print(anova(aov(value ~ energy_type + insulation_type, 
                    data = infoStruct[which(infoStruct$source == "des2"),])))
    for (k in 1:2) {print(" ", quote = FALSE)}
    print("for source = logs:")
    print(anova(aov(value ~ energy_type + insulation_type, 
                    data = infoStruct[which(infoStruct$source == "logs"),])))
    for (k in 1:2) {print(" ", quote = FALSE)}
    print("for source = ibuttons:")
    print(anova(aov(value ~ energy_type + insulation_type, 
                    data = infoStruct[which(infoStruct$source == "ibuttons"),])))
    for (k in 1:2) {print(" ", quote = FALSE)}
    mod <- lm(value ~ energy_type + insulation_type + source, 
              data = infoStruct[infoStruct$energy_type != "lpg", ])
    print(summary(mod))
    assign(x = "lm_model", value = mod, envir = .GlobalEnv)
  }
  
  if (viewRaw) {View(infoStruct)}
  
  return(infoStruct)
}

# ---------------------------------------- #
# ---------------------------------------- #

# load the data
load(paste(rdadir, "houseHoldList.Rda", sep = ""))

# divide them ibutton data into seasons.
houseHoldList <- lapply(X = houseHoldList, FUN = function(hh) {
  
  dfIbutton <- hh@ibuttonData$data_df
  if (is.null(dfIbutton)) {return(hh)}
  if (nrow(dfIbutton) < 1) {return(hh)}
  
  dfIbutton$season <- ifelse(month(dfIbutton$date) %in% c(6,7,8), "winter", "summer")
  print(table(dfIbutton$season))
  hh@ibuttonData$data_df <- dfIbutton ; rm(dfIbutton)
  
  return(hh)
})

x <- doIt(VAR = "coal", PER = "pmonth", doAnova = FALSE, season = "summer")

x$type <- paste(x$energy_type, x$insulation_type, sep = "_")
x <- remove.cols(df = x, colNames = c("energy_type", "insulation_type"))

xc <- dcast(data = x, 
            formula = type ~ source, 
            fun.aggregate = mean, 
            na.rm = TRUE, 
            value.var = "value")

allCombinations <- expand.grid(c("coal", "elec", "control"), 
                               c("basic", "full", "none"), 
                               c("logs", "ibuttons", "des1", "des2")[2])
names(allCombinations) <- c("energy_type", "insulation_type", "source")

res <- predict.lm(lm_model, newdata = allCombinations)
allCombinations$coal_use <- res
# ---------------------------------------- #
# ---------------------------------------- #
# NOTES

# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...
## ------------------- ##




# divide them ibutton data into seasons.
houseHoldList <- lapply(X = houseHoldList, FUN = function(hh) {
  
  dfIbutton <- hh@ibuttonData$data_df
  if (is.null(dfIbutton)) {return(hh)}
  if (nrow(dfIbutton) < 1) {return(hh)}
  
  dfIbutton$season <- ifelse(month(dfIbutton$date) %in% c(6,7,8), "winter", "summer")
  print(table(dfIbutton$season))
  hh@ibuttonData$data_df <- dfIbutton ; rm(dfIbutton)
  
  return(hh)
})


ignitionsdff <- lapply(X = houseHoldList, FUN = function(hh) {
  num <- summariseIbuttons(hh = hh, 
                           var = "ignitions", 
                           summariseBy = "pmonth", 
                           season = "summer")
  df <- data.frame(num, 
                   energyType = hh@energyType, 
                   insulationType = hh@insulationType, 
                   stringsAsFactors = FALSE)
  return(df)
})

ignitionsdff <- do.call("rbind", ignitionsdff)
ignitionsdff <- ignitionsdff[which(!is.na(ignitionsdff$num)),]
# ignitionsdff <- ignitionsdff[which(!(ignitionsdff$energyType == "lpg")),]
ignitionsdff[which(ignitionsdff$energyType == "lpg" & ignitionsdff$num > 0), "energyType"] <- "elec"

ggplot(data = ignitionsdff, mapping = aes(x = num)) +
  geom_dotplot() +
  facet_grid(energyType ~ insulationType)

igns_grouped <- group_by(.data = ignitionsdff, energyType, insulationType)
means <- summarise(igns_grouped, "mean_fire_ig_freq" = mean(num))

means$type <- paste(means$energyType, means$insulationType, sep = "_")

ggplot(data = means, mapping = aes(x = type, y = mean_fire_ig_freq)) +
  geom_bar(stat = "identity", fill = "red") +
  guides(fill = FALSE) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  geom_text(aes(label = round(mean_fire_ig_freq, digits = 1), vjust = 1)) +
  ggtitle("Avg number of ignitions per spring month \n given by intervention type") +
  xlab("Intervention type \n (Source: ibuttons & logs)") +
  ylab("Mean monthly fire ignitions")





# controlList <- sapply(X = houseHoldList, FUN = function(hh) {return(hh@energyType == "control")})
# controlList <- houseHoldList[controlList]
# houseHoldList <- controlList
# rm(controlList)

# usage <- x
# extensions <- sapply(X = houseHoldList, FUN = function(hh) {return(hh@address@extension)})
# dfTemp <- cbind(usage, extensions)
# 
# anova(aov(value ~ extensions, data = dfTemp))
# mod <- lm(value ~ extensions + source, data = dfTemp)
# print(summary(mod))


