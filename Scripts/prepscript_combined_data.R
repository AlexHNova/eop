# this script combines the data from the classified ibutton data set with the data in the coal data set and electrification data set
# ---------------------------------------- #

# start clean
rm(list=ls())

# define directories
dropdir <- "C:/Users/Alex H/Dropbox (Nova SA)/"
datadir <- paste(getwd(), "/Data/", sep = "")
scriptdir <- paste(getwd(), "/ScriptsAndRdas/", sep = "")
rdadir <- paste(getwd(), "/ScriptsAndRdas/", sep = "")
rfunctdir <- paste(dropdir, "Rfunctions/", sep = "")

# load necessary libraries
library(dplyr)

# declare constants

# source additional functions

# define auxiliary functions

# ---------------------------------------- #
# load the data sets to combine
load(paste(rdadir, "df_ibutton_data_classified.Rda", sep = ""))
load(paste(rdadir, "df_coal_data_with_calc_cols.Rda", sep = ""))
# load the electricity data as well

# ---------------------------------------- #
# subset all data sets to include only the data from those stand numbers that they have in common
common <- intersect(as.character(levels(df_coal_data_with_calc_cols$stand_number)), as.character(levels(df_ibutton_data_classified$stand_number)))

df_ibutton_data_classified$stand_number <- as.character(df_ibutton_data_classified$stand_number)
df_ibutton_data_classified <- df_ibutton_data_classified[which(df_ibutton_data_classified$stand_number %in% common),]
df_ibutton_data_classified$stand_number <- factor(df_ibutton_data_classified$stand_number, levels = common, ordered = FALSE)

df_coal_data_with_calc_cols$stand_number <- as.character(df_coal_data_with_calc_cols$stand_number)
df_coal_data_with_calc_cols <- df_coal_data_with_calc_cols[which(df_coal_data_with_calc_cols$stand_number %in% common),]
df_coal_data_with_calc_cols$stand_number <- factor(x = df_coal_data_with_calc_cols$stand_number, levels = common, ordered = FALSE)

# ---------------------------------------- #
# now subset all sub-datasets for each stand to the time period that they all have in common
# coal_list <- split(x = df_coal_data_with_calc_cols, f = df_coal_data_with_calc_cols$stand_number)
# ibutton_list <- split(x = df_ibutton_data_classified, f = df_ibutton_data_classified$stand_number)
# 
# for (s in 1:length(common)) {
#   stand <- common[[s]]
#   coal_df <- coal_list[[stand]] 
#   ibutton_df <- ibutton_list[[stand]]
#   
#   lower_bound <- max(min(coal_df$date, na.rm = TRUE), min(ibutton_df$date, na.rm = TRUE))
#   upper_bound <- min(max(coal_df$date, na.rm = TRUE), max(ibutton_df$date, na.rm = TRUE))
#   
#   if (upper_bound < lower_bound) {
#     warning(paste("No overlapping time period found for stand ", stand, ". Returning NULL.", sep = ""))
#     coal_list[[stand]] <- NULL
#     ibutton_list[[stand]] <- NULL
#     next
#   }
#   
#   coal_df <- coal_df[which(between(x = coal_df$date, left = lower_bound, right = upper_bound)),]
#   coal_list[[stand]] <- coal_df
#   ibutton_df <- ibutton_df[which(between(x = ibutton_df$date, left = lower_bound, right = upper_bound)),]
#   ibutton_list[[stand]] <- ibutton_df
#   
#   print(paste(lower_bound, upper_bound, sep = " - "))
# }

# ---------------------------------------- #
# now put the different sub-data-sets for each stand together

coal_list <- split(x = df_coal_data_with_calc_cols, f = df_coal_data_with_calc_cols$stand_number)
ibutton_list <- split(x = df_ibutton_data_classified, f = df_ibutton_data_classified$stand_number)

coal_list <- lapply(X = coal_list, FUN = function(stand) {
  # make sure the data frame is sorted according to date (ascending)
  sort_order <- stand$date
  sort_order <- sort(x = sort_order)
  matches <- match(stand$date, table = sort_order)
  stand <- stand[matches,]
  
  # now attach the 'weighing_period' column
  stand$weighing_period <- 1:nrow(stand)
  return(stand)
})
  
common <- intersect(names(coal_list), names(ibutton_list))

combined_list <- lapply(X = common, FUN = function(stand) {
  
  coal_df <- coal_list[[stand]]
  ibutton_df <- ibutton_list[[stand]]
  
  idxx <- match(c("date", "stand_number"), table = names(coal_df))
  cols_to_add <- names(coal_df)[-na.omit(idxx)]
  
  ibutton_df[, cols_to_add] <- NA
  
  ibutton_df[which(ibutton_df$date <= coal_df[1, "date"]), cols_to_add] <- coal_df[1,cols_to_add]
  
  for (r in 2:nrow(coal_df)) {
    ibutton_df[which((ibutton_df$date <= coal_df[r, "date"]) & (ibutton_df$date > coal_df[r-1, "date"])), cols_to_add] <- coal_df[r,cols_to_add]
  }
  
  return(ibutton_df)
})

df_all_combined <- do.call(what = "rbind", combined_list)
save(df_all_combined, file = paste(rdadir, "df_all_combined.Rda", sep = ""))
