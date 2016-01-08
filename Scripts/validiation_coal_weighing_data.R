# ---------------------------------------- #
# start clean
rm(list = ls())
# ---------------------------------------- #
# Date created: 13 October 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# script to validate the coal weighing data in light of the classified ibutton data and the coal-use log data
# ---------------------------------------- #
# source preamble
source(paste(getwd(), "/Scripts/preamble.R", sep = ""))
# ---------------------------------------- #
# load libraries
#
# ---------------------------------------- #
# source additional functions
#
# ---------------------------------------- #
# declare script constants
DEBUG <- TRUE
VERBOSE <- TRUE
# ---------------------------------------- #
# define auxiliary functions
#
# ---------------------------------------- #
# ---------------------------------------- #

# load the data
load(paste(rdadir, "df_coal_data_with_calc_cols.Rda", sep = ""))
load(paste(rdadir, "df_ibutton_data_classified.Rda", sep = ""))

# ---------------------------------------- #
# subset both sets to contain only data from the stands that occur in both sets
# ------------------- #
## get the stand numbers that occur in both sets
stands <- c(unique(as.character(df_ibutton_data_classified$stand_number)), unique(as.character(df_coal_data_with_calc_cols$stand_number)))
dups <- duplicated(stands)
if (any(dups)) {
  stands <- stands[!dups]
}

keep <- stands %in% unique(as.character(df_ibutton_data_classified$stand_number))
if (any(keep)) {
  stands <- stands[keep]
}

keep <- stands %in% unique(as.character(df_coal_data_with_calc_cols$stand_number))
if (any(keep)) {
  stands <- stands[keep]
}

# ------------------- #
## subset the ibutton data
keep <- as.character(df_ibutton_data_classified$stand_number) %in% stands
if (any(keep)) {
  df_ibutton_data_classified <- df_ibutton_data_classified[keep,]
}

# ------------------- #
## subset the coal weighing data
keep <- as.character(df_coal_data_with_calc_cols$stand_number) %in% stands
if (any(keep)) {
  df_coal_data_with_calc_cols <- df_coal_data_with_calc_cols[keep,]
}

# ---------------------------------------- #
# make sure that both data sets have a 'yrday' variable
df_coal_data_with_calc_cols$yrday <- yday(x = df_coal_data_with_calc_cols$date)
df_ibutton_data_classified$yrday <- yday(x = df_ibutton_data_classified$date)

# ---------------------------------------- #
# split the ibutton and coal data frames into lists according to stand numbers
coal_list <- split(x = df_coal_data_with_calc_cols, f = as.character(df_coal_data_with_calc_cols$stand_number))

ibutton_list <- split(x = df_ibutton_data_classified, f = as.character(df_ibutton_data_classified$stand_number))

# ---------------------------------------- #
# now count the number of fires for each coal weighing period by taking the coal data and just adding a 'num_of_fires' column to it

coal_list_with_fires <- lapply(X = stands, FUN = function(stnd) {
  
  if (DEBUG) {print(stnd)}
  
  # get the coal and ibutton data for the current stand
  df_stand_ibutton_data <- ibutton_list[[stnd]]
  df_stand_coal_data <- coal_list[[stnd]]
  
  # make sure both sets are sorted according to date
  dte_sorted <- sort(df_stand_coal_data$date)
  sorted_idxx <- match(df_stand_coal_data$date, table = dte_sorted)
  df_stand_coal_data <- df_stand_coal_data[sorted_idxx,]
  
  dte_sorted <- sort(df_stand_ibutton_data$date)
  sorted_idxx <- match(df_stand_ibutton_data$date, table = dte_sorted)
  df_stand_ibutton_data <- df_stand_ibutton_data[sorted_idxx,]
  
  # add the "num_fires" column to the coal data and initialise it
  df_stand_coal_data$num_fires <- NA_integer_ 
  df_stand_coal_data$coal_per_fire_kg <- NA_real_
  
  # now loop through the coal data records and match their number of fires
  for (r in 2:nrow(df_stand_coal_data)) {
    mindate <- df_stand_coal_data[r-1, "date"]
    maxdate <- df_stand_coal_data[r, "date"]
    
    ibutton_subset <- df_stand_ibutton_data[which((df_stand_ibutton_data$date > mindate) & (df_stand_ibutton_data$date < maxdate)),]
    if (nrow(ibutton_subset) < 1) {
      if (VERBOSE) { message("No ibutton data found for the current coal weighing period.") }
      next
    }
    
    if (DEBUG) {print(min(ibutton_subset$date, na.rm = TRUE)); print(max(ibutton_subset$date, na.rm = TRUE))}
    episodes <- table((rle(ibutton_subset$vuur))$values)
    if ("vuur" %in% names(episodes)) {
      df_stand_coal_data[r, "num_fires"] <- episodes[["vuur"]]
      df_stand_coal_data[r, "coal_per_fire_kg"] <- df_stand_coal_data[r, "usage_net_kg"] / (episodes[["vuur"]])
    } 
    else {
      df_stand_coal_data[r, "num_fires"] <- 0
    }
  }
  
  return(df_stand_coal_data)
})


