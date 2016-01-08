# Date created: 10 November 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# script to explore the KwaZamokuhle electricity data (all three sets combined)
# ---------------------------------------- #
# start clean
rm(list = ls())
# ---------------------------------------- #
# source preamble
dropdir <- "C:/Users/Alex H/Dropbox (Nova SA)/"
projdir <- paste(dropdir, "Eskom Offset Study 2014 - Team/R/eop/", sep = "")
perlPath <- "C:/Perl/bin/perl.exe"

source(paste(projdir, "/Scripts/preamble.R", sep = ""))
# ---------------------------------------- #
# declare script constants
VERBOSE <- TRUE
DEBUG <- TRUE
# ---------------------------------------- #
# ---------------------------------------- #

load(paste(rdadir, "df_elec_data_all.Rda", sep = ""))
df_elec_data_all$yr <- as.factor(year(df_elec_data_all$date))
df_elec_data_all$energy_type <- NA_character_
df_elec_data_all$insulation_type <- NA_character_

for (r in 1:nrow(df_elec_data_all)) {
  df_elec_data_all[r, "energy_type"] <- ((strsplit(x = df_elec_data_all[r, "type"], split = "_", fixed = TRUE))[[1]])[[1]]
  df_elec_data_all[r, "insulation_type"] <- ((strsplit(x = df_elec_data_all[r, "type"], split = "_", fixed = TRUE))[[1]])[[2]]
}
rm(r)


lpg <- df_elec_data_all[which(df_elec_data_all$energy_type == "lpg"),]
df_elec_data_all$yrday <- yday(df_elec_data_all$date)
df_elec_data_all$month_day <- mday(df_elec_data_all$date)



ggplot(data = df_elec_data_all[which(month(df_elec_data_all$date) == 7),], mapping = aes(x = month_day, y = kwh_units, colour = yr)) + 
  geom_line() + 
  facet_grid(energy_type ~ insulation_type) + geom_smooth() + ylim(0,100)



ggplot(data = df_elec_data_all[which(month(df_elec_data_all$date) == 7 & ((is.na(df_elec_data_all$transaction_type)) | (df_elec_data_all$transaction_type != "ebsst issue"))),], mapping = aes(x = yrday, y = kwh_units, colour = yr)) + 
  geom_smooth(alpha = I(1/3)) + 
  geom_point() +
  facet_grid(energy_type ~ insulation_type) + ylim(0,100)





ggplot(data = df_elec_data_all, mapping = aes(x = date, y = kwh_units, colour = yr)) + 
  geom_line() + 
  geom_smooth() +
  ylim(0,100) +
  facet_grid(energy_type ~ insulation_type) + geom_smooth()


ggplot(data = df_elec_data_all, mapping = aes(x = date, y = kwh_units, colour = yr)) + 
  geom_line() + 
  facet_grid(type ~ supplier) + geom_smooth()

ggplot(data = df_elec_data_all, mapping = aes(x = date, y = kwh_units, colour = type)) + 
  geom_smooth() + 
  facet_grid(. ~ yr, scales = "free") 

ggplot(data = df_elec_data_all, mapping = aes(x = yr, y = kwh_units)) + 
  geom_boxplot() + 
  facet_grid(energy_type ~ insulation_type) + ylim(0,200)

ggplot(data = df_elec_data_all, mapping = aes(x = kwh_units, group = yr, fill = yr)) + 
  geom_density(alpha = I(1/3)) + 
  facet_grid(energy_type ~ insulation_type)

ggplot(data = df_elec_data_all, mapping = aes(x = kwh_units, fill = supplier)) + 
  geom_density(alpha = I(1/3)) + 
  facet_grid(. ~ yr)


elec_m <- melt(data = df_elec_data_all, id.vars = c("date", "stand_number", "extension", "user"), na.rm = TRUE)


# avg annual expenditure per hh on electricity/
## take two general winter months from 2015 and times by two
## take two general summer months from 2015 and times by four

load(paste(rdadir, "df_elec_data_all.Rda", sep = ""))

# subset to use only data from 2015
df_elec_data_all <- df_elec_data_all[which(year(df_elec_data_all$date) == 2015),]

SEASON <- "annual"
# "winter" will calculate the avg total usage over a 4 month winter; 
# "summer" will calculate the avg total usage over an 8 month summer;
# "annual" will calculate both and return their sum, amounting to a 12 month year.

table(month(df_elec_data_all$date))
if (SEASON == "winter") {
  df_elec_data_all <- df_elec_data_all[which(month(df_elec_data_all$date) %in% c(6:7)),]
}
if (SEASON == "summer") {
  df_elec_data_all <- df_elec_data_all[which(month(df_elec_data_all$date) %in% c(1,9)),]
}
if (SEASON == "annual") { # remove the records for October, because we didn't cover the entire month.
  df_elec_data_all <- df_elec_data_all[which(month(df_elec_data_all$date) %in% c(1:9)),]
}

typesplits <- split(x = df_elec_data_all, f = df_elec_data_all$type)

typeAvgs <- lapply(X = typesplits, FUN = function(tpdf) {
  
  standsplits <- split(x = tpdf, f = tpdf$stand_number) 
  
  standAvgs <- sapply(X = standsplits, FUN = function(sdf) {
    
    if (SEASON %in% c("summer", "annual")) {
      mild <- sum(sdf[which(month(sdf$date) %in% c(4,9)), "kwh_units"], na.rm = TRUE)
      fairlyWarm <- sum(sdf[which(month(sdf$date) == 3), "kwh_units"], na.rm = TRUE) * 2
      warm <- sum(sdf[which(month(sdf$date) == 2), "kwh_units"], na.rm = TRUE) * 2
      hot <- sum(sdf[which(month(sdf$date) == 1), "kwh_units"], na.rm = TRUE) * 2
      totalSummer <- mild + fairlyWarm + warm + hot
    }
    
    if (SEASON %in% c("winter", "annual")) {
      totalWinter <- sum(sdf[which(month(sdf$date) %in% c(5:8)), "kwh_units"], na.rm = TRUE)
    }
    
    if (SEASON == "summer") {return(totalSummer)}
    if (SEASON == "winter") {return(totalWinter)}
    if (SEASON == "annual") {return(totalSummer + totalWinter)}
    
  })
  
  tpn <- length(standAvgs[!is.na(standAvgs)])
  print(tpn)
  tpavg <- mean(x = standAvgs, na.rm = TRUE)
  return(list(tpavg, tpn))
})

names(typeAvgs) <- names(typesplits)
typeAvgs



# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...
## ------------------- ##

# elec_m <- melt(data = df_elec_data_all, id.vars = c("date", "stand_number", "extension", "user"), na.rm = TRUE)

