# Date created: 14 December 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# Script to prepare the DES and ibutton stats needed for the EOP December 2015 report
# ---------------------------------------- #
# start clean
# rm(list = ls())
# # ---------------------------------------- #
# # source preamble
# dropdir <- "C:/Users/Alex H/Dropbox (Nova SA)/"
# projdir <- paste(dropdir, "Eskom Offset Study 2014 - Team/R/eop/", sep = "")
# source(paste(projdir, "Scripts/preamble.R", sep = ""))
# # ---------------------------------------- #
# source(paste(novafunctdir, "pears.R", sep = ""))
# # ---------------------------------------- #
# # declare script constants
# VERBOSE <- TRUE
# DEBUG <- TRUE
# ---------------------------------------- #
# ---------------------------------------- #

# load 
load(paste(rdadir, "houseHoldList.Rda", sep = ""))

# ---------------------------------------- #
# construct a des from the houseHoldList 
desdff <- lapply(X = houseHoldList, function(hh) {
  desPre <- hh@DESdata$data_df[[1]]
  if (!is.null(desPre)) {
    desPre$prepost <- "pre"
  }
  desPost <- hh@DESdata$data_df[[2]]
  if (!is.null(desPost)) {
    desPost$prepost <- "post"
  }
  
  des <- rbind.fill(desPre, desPost) ; rm(desPre, desPost)
  if (!is.null(des)) {
    des$energy_type <- hh@energyType
    des$insulation_type <- hh@insulationType
  }
  
  return(des)
})

des <- do.call("rbind.fill", desdff) ; rm(desdff)
des$type <- paste(des$energy_type, des$insulation_type, des$prepost, sep = "_")





# ---------------------------------------- #
# ---------------------------------------- #

load(paste(rdadir, "df_ibutton_data_classified.Rda", sep = ""))

# fix an error
df_ibutton_data_classified[which(df_ibutton_data_classified$stand_number == "2859"), "energy_type"] <- "elec"

df_ibutton_data_classified$ignition <- gsub(pattern = "TRUE", 
                                            replacement = "yes", 
                                            x = df_ibutton_data_classified$ignition, 
                                            fixed = TRUE)
# df_ibutton_data_classified[which(is.na(df_ibutton_data_classified$ignition)), "ignition"] <- "no"
# df_ibutton_data_classified[which(is.na(df_ibutton_data_classified$fire)), "ignition"] <- NA

ignitionsdf <- df_ibutton_data_classified[which(df_ibutton_data_classified$ignition == "yes"),]
rm(df_ibutton_data_classified)
ignitionsdf$type <- paste(ignitionsdf$energy_type, ignitionsdf$insulation_type, sep = "_")
ignitionsdf$hod <- hour(ignitionsdf$date)
ignitionsdf$season <- ifelse(month(ignitionsdf$date) %in% c(6,7,8,9), "winter", "summer")

# remove unnecessary fields and records
ignitionsdf <- ignitionsdf[,c("type", "ignition", "hod", "season")]
ignitionsdf <- ignitionsdf[which(!(ignitionsdf$type %in% c("lpg_basic", "lpg_full"))),]

# split by season
ignitionsdf_summer <- ignitionsdf[which(ignitionsdf$season == "summer"),]
ignitionsdf_winter <- ignitionsdf[which(ignitionsdf$season == "winter"),]




# SELECT WHICH SEASON TO WORK WITH
ignitionsdf <- ignitionsdf_summer

# calculate the props for each type
hoddf <- data.frame(matrix(nrow = 24, ncol = 1), stringsAsFactors = FALSE) 
names(hoddf) <- "hod"
hoddf$hod <- c(0:23)
typesplits <- split(x = ignitionsdf, f = ignitionsdf$type)

typeprops <- lapply(X = typesplits, FUN = function(tpdf) {
  
  tab <- table(tpdf$hod, tpdf$ignition)
  hod <- as.integer(rownames(tab))
  tab <- data.frame(rbind(prop.table(tab)), stringsAsFactors = FALSE)
  tab$hod <- hod; rm(hod)

  hoursShort <- hoddf[which(!(hoddf$hod %in% tab$hod)), "hod"]
  if (length(hoursShort) > 0) {
    filldf <- data.frame(matrix(nrow = length(hoursShort), ncol = 2), stringsAsFactors = FALSE)
    names(filldf) <- c("hod", "yes")
    filldf$hod <- hoursShort ; rm(hoursShort)
    filldf[, "yes"] <- 0
    
    tab <- rbind(tab, filldf) ; rm(filldf)
  }
  
  #tab_m <- melt(data = tab, id.vars = "hod")
  tpe <- unique(tpdf$type)
  tab$type <- tpe
  
  tab <- tab[order(tab$hod, decreasing = FALSE),]
  rownames(tab) <- c(1:nrow(tab))
  
  return(tab)
})

# put them all together
typeprops <- do.call("rbind", typeprops)
names(typeprops) <- gsub(pattern = "yes", replacement = "prop", x = names(typeprops), fixed = TRUE)

# split the type field into an energy type and insulation type
typeprops$energy_type <- as.character(typeprops$type)
typeprops$insulation_type <- as.character(typeprops$type)

for (r in 1:nrow(typeprops)) {
  typeprops[r, "energy_type"] <- (strsplit(x = typeprops[r, "energy_type"], 
                                           split = "_", 
                                           fixed = TRUE))[[1]][1]
  typeprops[r, "insulation_type"] <- (strsplit(x = typeprops[r, "insulation_type"], 
                                               split = "_", 
                                               fixed = TRUE))[[1]][2]
}

typeprops$energy_type <- factor(x = typeprops$energy_type, 
                                levels = c("control", "coal", "elec", "lpg"), 
                                ordered = TRUE)
typeprops$insulation_type <- factor(x = typeprops$insulation_type, 
                                    levels = c("none", "basic", "full"),
                                    ordered = TRUE)


x <- c(5,6,7,16,17,18,19)
ctrl <- typeprops[which(typeprops$type == "control_none"), ]
y <- ctrl[match(x = x, table = ctrl$hod), "prop"]
ctrl <- remove.cols(df = ctrl, colNames = c("type", "energy_type", "insulation_type"))
refdots <- data.frame(x,y, stringsAsFactors = FALSE)

ggplot(data = typeprops, mapping = aes(x = hod, y = prop, fill = "red")) +
  geom_bar(stat = "identity") +
  guides(fill = FALSE) +
  facet_grid(facets = energy_type ~ insulation_type) +
  xlab("Hour of day (00 - 23) \n (Source: ibuttons & logs)") +
  ylab("Proportion of ignitions") +
  ggtitle("Proportion of ignitions per hour of winter day \n given by intervention type") +
  #geom_point(data = refdots, mapping = aes(x = x, y = y)) +
  geom_line(data = ctrl, 
           mapping = aes(x = hod, y = prop))

# ggplot(data = typeprops, mapping = aes(x = hod, y = prop, group = type, colour = type)) +
#   geom_smooth(alpha = I(1/5)) +
#   guides(fill = FALSE) 

byhour <- dcast(data = typeprops[, c("type", "prop", "hod")], 
                formula = type ~ hod, value.var = "prop")

earlyMornings <- byhour[, c("type", "5", "6", "7")]
earlyMornings$sums <- rowSums(x = earlyMornings[, c("5","6","7")], na.rm = FALSE)
earlyMornings <- earlyMornings[order(earlyMornings$sums, decreasing = FALSE),]
rownames(earlyMornings) <- c(1:nrow(earlyMornings))
earlyMornings$sums <- earlyMornings$sums * 100

evenings <- byhour[, c("type", "16", "17", "18", "19")]
evenings$sums <- rowSums(x = evenings[, c("16", "17", "18", "19")], na.rm = FALSE)
evenings <- evenings[order(evenings$sums, decreasing = FALSE),]
rownames(evenings) <- c(1:nrow(evenings))
evenings$sums <- evenings$sums * 100

# spaceHeating <- byhour[, c("type", "0", "1", "2", 
#                            "3", "4", "5", "6", "7", 
#                            "8", "21", "22", "23")]
# spaceHeating$sums <- rowSums(x = spaceHeating[, c("0", "1", "2", 
#                                                   "3", "4", "5", 
#                                                   "6", "7", "8", 
#                                                   "21", "22", "23")], na.rm = FALSE)


# ---------------------------------------- #
# ---------------------------------------- #


load(paste(rdadir, "df_ibutton_data_classified.Rda", sep = ""))

# fix an error
df_ibutton_data_classified[which(df_ibutton_data_classified$stand_number == "2859"), "energy_type"] <- "elec"

df_ibutton_data_classified$hod <- hour(df_ibutton_data_classified$date)
df_ibutton_data_classified$type <- paste(df_ibutton_data_classified$energy_type, 
                                         df_ibutton_data_classified$insulation_type, 
                                         sep = "_")
df_ibutton_data_classified$season <- ifelse(month(df_ibutton_data_classified$date) %in% c(6,7,8,9), "winter", "summer")

dfIbuttons_winter <- df_ibutton_data_classified[which(df_ibutton_data_classified$season == "winter"),]
dfIbuttons_summer <- df_ibutton_data_classified[which(df_ibutton_data_classified$season == "summer"),]

ggplot(data = dfIbuttons_summer, 
       mapping = aes(x = hod, 
                     y = w, 
                     group = type, 
                     colour = type)) + 
  geom_smooth() +
  ggtitle("Avg indoor temperature per hour of spring day \n across the different intervention groups") +
  xlab("Hour of day (00 - 23) \n (Source: ibuttons)") +
  ylab("Avg indoor temp \n (Post-intervention)")


fiveAm_w <- dfIbuttons_winter[, c("hod", "w", "type")]
fiveAm_w <- fiveAm_w[which(fiveAm_w$hod == 5), ]
fiveAm_w <- dcast(data = fiveAm_w, 
                  formula = type ~ hod, 
                  fun.aggregate = mean, 
                  na.rm = TRUE, 
                  value.var = "w")

fiveAm_w <- fiveAm_w[order(fiveAm_w$`5`, decreasing = TRUE),]
rownames(fiveAm_w) <- c(1:nrow(fiveAm_w))
names(fiveAm_w) <- c("type", "temp at 5am")

fiveAm_s <- dfIbuttons_summer[, c("hod", "w", "type")]
fiveAm_s <- fiveAm_s[which(fiveAm_s$hod == 5), ]
fiveAm_s <- dcast(data = fiveAm_s, 
                  formula = type ~ hod, 
                  fun.aggregate = mean, 
                  na.rm = TRUE, 
                  value.var = "w")

fiveAm_s <- fiveAm_s[order(fiveAm_s$`5`, decreasing = TRUE),]
rownames(fiveAm_s) <- c(1:nrow(fiveAm_s))
names(fiveAm_s) <- c("type", "temp at 5am")

  
# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...
## ------------------- ##
