# Date created: 
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# script to explore the coal data by matching stand types to the data etc. and doing some visual exploration
# ---------------------------------------- #
# start clean
rm(list = ls())
# ---------------------------------------- #
# source preamble
source(paste(getwd(), "/Scripts/preamble.R", sep = ""))
# ---------------------------------------- #
# declare script constants
VERBOSE <- TRUE
DEBUG <- TRUE
# ---------------------------------------- #
# ---------------------------------------- #
# load the coal data
#dropdir <- "~/Dropbox (Nova SA)/"
#projdir <- "~/Dropbox (Nova SA)/Eskom Offset Study 2014 - Team/R/eop/"

load(paste(rdadir, "df_coal_data_with_calc_cols.Rda", sep = ""))

# ---------------------------------------- #
# add a 'type' column to the coal data
## load the stands and types data
load(paste(rdadir, "stands_and_types.Rda", sep = ""))

## matching
df_coal_data_with_calc_cols$stand_number <- as.character(df_coal_data_with_calc_cols$stand_number)
stands_and_types$stand_number <- as.character(stands_and_types$stand_number)
matches <- match(df_coal_data_with_calc_cols$stand_number, table = stands_and_types$stand_number)

if (any(is.na(matches))) {
  warning("Coal weighing stands detected without records in the stands_and_types list.")
}

df_coal_data_with_calc_cols$type <- NA_character_
df_coal_data_with_calc_cols[which(!is.na(matches)), "type"] <- stands_and_types[matches[!is.na(matches)],"type"]

rm(stands_and_types)
# ---------------------------------------- #
# for the sake of exploration, teleport the records for all the stands back to a common temporal starting point (in terms of the day of year)
df_coal_data_with_calc_cols$yrday <- yday(df_coal_data_with_calc_cols$date)
startday <- min(df_coal_data_with_calc_cols$yrday)
if (VERBOSE) {message("The common starting yrday will be: ", startday)}

standsplits <- split(x = df_coal_data_with_calc_cols, f = df_coal_data_with_calc_cols$stand_number)
standsplits <- lapply(X = standsplits, FUN = function(s) {
  minday <- min(s$yrday)
  if (VERBOSE) {message("minday for stand ", unique(s$stand_number), " was ", minday)}
  adjustby <- minday - startday
  if (adjustby > 0) {
    s$date <- (s$date - adjustby*24*60*60)
    s$yrday <- yday(s$date)
  }
  if (VERBOSE) {message("minday for stand ", unique(s$stand_number), " now is ", yday(min(s$date)))}
  return(s)
})

df_coal_data_with_calc_cols <- do.call("rbind", standsplits)

df_coal_data_with_calc_cols$energy_type <- NA_character_
df_coal_data_with_calc_cols$insulation_type <- NA_character_

for (r in 1:nrow(df_coal_data_with_calc_cols)) {
  if (is.na(df_coal_data_with_calc_cols[r, "type"])) {next}
  
  splits <- (strsplit(as.character(df_coal_data_with_calc_cols[r, "type"]), "_", fixed = TRUE))[[1]]
  if (length(splits) > 1) {
    df_coal_data_with_calc_cols[r, "energy_type"] <- splits[[1]]
    df_coal_data_with_calc_cols[r, "insulation_type"] <- splits[[2]]
  }
}

#rm(standsplits)

# --------------------------------------------- #
# --------------------------------------------- #
# visual inspection
p1 <- ggplot(data = df_coal_data_with_calc_cols, mapping = aes(x = date, y = cumulative_usage_kg, group = stand_number)) + facet_wrap(facets = ~ type) + geom_line()  + geom_smooth(method = "lm", aes(group = type))
p1 + theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(data = df_coal_data_with_calc_cols, mapping = aes(x = date, y = cumulative_usage_kg, group = stand_number, colour = stand_number)) + 
  facet_wrap(facets = ~ type) + 
  geom_line(alpha = I(1/3)) + geom_smooth(method = "lm", aes(group = type)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position='none')
p2

p3 <- ggplot(data = df_coal_data_with_calc_cols, mapping = aes(x = daily_avg)) + 
  geom_density(alpha = I(1/4), aes(fill = type)) + 
  facet_wrap(facets = insulation_type ~energy_type, ncol = 2)  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position='none')
p3

p4 <- ggplot(data = df_coal_data_with_calc_cols, mapping = aes(x = daily_avg)) + 
  geom_density(alpha = I(1/4), aes(fill = type)) + 
  facet_wrap(facets = ~insulation_type , ncol = 2)  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position='bottom')
p4

p5 <- ggplot(data = df_coal_data_with_calc_cols, mapping = aes(x = daily_avg)) + 
  geom_density(alpha = I(1/4), aes(fill = type)) + 
  facet_wrap(facets = ~energy_type, ncol = 2)  + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position='bottom')
p5

p6 <- ggplot(data = df_coal_data_with_calc_cols, mapping = aes(x = type, y = daily_avg)) + 
  geom_boxplot(alpha = I(1/1), aes(fill = type)) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position='right')
p6

# ---------------------------------------- #
# extract coefficients from linear models 
typesplits <- split(x = df_coal_data_with_calc_cols, f = df_coal_data_with_calc_cols$type)

coefs <- lapply(typesplits, function(x) lm(cumulative_usage_kg ~ yrday, data = x)$coef[[2]])
CI <- lapply(typesplits, function(x) confint(lm(cumulative_usage_kg ~ yrday, data = x))[2,])

df <- data.frame(
  lower = sapply(typesplits, function(x) {(confint(lm(cumulative_usage_kg ~ yrday, data = x)))[2,1]}), 
  point = sapply(typesplits, function(x) {lm(cumulative_usage_kg ~ yrday, data = x)$coef[[2]]}), 
  upper = sapply(typesplits, function(x) {confint(lm(cumulative_usage_kg ~ yrday, data = x))[2,2]})
  )

save(df, df_coal_data_with_calc_cols, p1, p2, p3, p4, p5, p6, file = paste(rdadir, "exploration_coal.Rda", sep=""))

# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...