# --------------------------------------------- #
# start clean
rm(list = ls())
# --------------------------------------------- #
# Date created: 15 September 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# --------------------------------------------- #
# source preamble
source(paste(getwd(), "/Scripts/preamble.R", sep = ""))
# --------------------------------------------- #
# declare script constants
FORMAT_WEIGHTS <- c("Big Bag" = 52, "Small Bag" = 52, "Tin" = 17, "Unknown" = 52)
MAX_DAILY_AVG <- 28
DEBUG <- FALSE
VERBOSE <- TRUE
# --------------------------------------------- #
# define additional script functions

# function 'calculate(...)' adds five calculated columns to the argument received by its coal_df parameter; these are:
# kg_bought, cumulative_coal_kg, cumulative_usage_kg, usage_net_kg and daily_avg
calculate <- function(coal_df, format_weights = FORMAT_WEIGHTS) {
  
  coal_df$daily_avg <- NA_real_
  coal_df$kg_bought <- coal_df$units_bought_since_last_weighing * format_weights[[unique(as.character(coal_df$coal_buying_format))]]
  
  for (k in 2:nrow(coal_df)) {
    m <- (k - 1) 
    
    coal_df[k, "cumulative_coal_kg"] <- ifelse(is.na(coal_df[m, "cumulative_coal_kg"]), 
                                               0, 
                                               coal_df[m, "cumulative_coal_kg"]) + ifelse(is.na(coal_df[k, "kg_bought"]), 
                                                                                          0, 
                                                                                          coal_df[k, "kg_bought"])

    coal_df[k, "cumulative_usage_kg"] <-  ifelse(is.na(coal_df[k, "cumulative_coal_kg"]), 
                                                 0, 
                                                 coal_df[k, "cumulative_coal_kg"]) - ifelse(is.na(coal_df[k, "current_coal_weight"]), 
                                                                                            0, 
                                                                                            coal_df[k, "current_coal_weight"])
    
    coal_df[k, "usage_net_kg"] <- ifelse(is.na(coal_df[k, "cumulative_usage_kg"]), 
                                         0, 
                                         coal_df[k, "cumulative_usage_kg"]) - ifelse(is.na(coal_df[m, "cumulative_usage_kg"]), 
                                                                                     0, 
                                                                                     coal_df[m, "cumulative_usage_kg"])
    
    coal_df[k, "daily_avg"] <- coal_df[k, "usage_net_kg"] / (coal_df[k, "yrday"] - coal_df[m, "yrday"])
  
  }
  
  return(coal_df)
}

# function 'check_coal(...)' looks for unreasonably high or low values in its received argument df and tries to fix them
# ---------------------- #
check_coal <- function(df) {
  
  OKAY <- FALSE
  
  # initialise the weights of the different coal buying formats; these might be adjusted by the function if it sees that the current weights do not work with the data for the stand in question
  format_weights <- FORMAT_WEIGHTS
  
  while (!OKAY) {
    if (VERBOSE) {message("Stand number: ", unique(as.character(df[, "stand_number"])))}
    df <- calculate(df, format_weights = format_weights)
    
    for (r in 1:nrow(df)) {
 
      # if the net usage is 0 or positive for the current record and the daily_avg is reasonable, no further changes have to be made for the current record, so move on
      if (df[r, "usage_net_kg"] >= 0) { 
        if (is.na(df[r, "daily_avg"]) | (df[r, "daily_avg"] <= MAX_DAILY_AVG)) {
          if (r == nrow(df)) {OKAY <- TRUE; if (DEBUG) {message("done")}} 
          next 
        }
      }
      
      # if daily average is wayyy too high, inspection is needed
      if (df[r, "daily_avg"] > MAX_DAILY_AVG)
      {
        if (VERBOSE) {message("Daily average too high: ", df[r, "daily_avg"])}
        
        if (df[r, "units_bought_since_last_weighing"] < 1) {
          if (VERBOSE) {message("Daily avg too high, but no units bought, so cannot correct the record. Moving on.")}
          if (r == nrow(df)) {OKAY <- TRUE; if (DEBUG) {message("done")}} 
          next
        }
        
        # reaching this point means that the daily_avg is too high, but fortunately some units were bought, so we can correct it to some extent by estimating the minimum amount of coal still in store (but accidentally not reflected in the current_coal_weight)
        {
          kg_in_stock <- 0.0
          kg_in_stock <- df[r, "kg_bought"] + df[r-1, "current_coal_weight"] - df[r, "current_coal_weight"] - (MAX_DAILY_AVG * (df[r, "yrday"] - df[r-1, "yrday"]))
          if (kg_in_stock < 0) {
            warning("Something went wrong...these people seem to be storing dark matter! (negative value for kg of coal in stock)")
          }
          if (VERBOSE) {message("Estimating that some ", kg_in_stock, " kg of coal in still in separate stock.")}
          df[r, "current_coal_weight"] <- df[r, "current_coal_weight"] + kg_in_stock
          break
        }
      }
      
      # reaching this point means the net usage is negative, so inspect
      if (VERBOSE) {message("usage_net_kg negative: ", df[r, "usage_net_kg"])}

      buying_format <- df[r, "coal_buying_format"]
      
      # if units were bought and adjusting the weight of the buying format with not more than 3 kg will solve the problem, adjust the weight of the buying format
      ## so first determine the amount with which the format weight will have to be adjusted to solve the problem
      adjust_by <- (ceiling((((-1) * (df[r, "usage_net_kg"])) / (df[r, "units_bought_since_last_weighing"])) + 0.5))
      
      ## now check if the adjustment is smaller than (or equal to) 3; if it is, adjust
      if (adjust_by <= 3)
      {
        if (VERBOSE) {message("Adjusting ", buying_format, " by ", adjust_by, " kg...")}
        format_weights[[buying_format]] <- format_weights[[buying_format]] + adjust_by
        if (VERBOSE) {message("format_weights now are: ", format_weights)}
        break
      }
      
      # if no units were bought and adding a unit bought will raise the daily average too high, simply decrease the current coal weight with the amount necessary to make the net_usage = 0.00
      if (((df[r, "usage_net_kg"] + format_weights[[buying_format]]) / (df[r, "yrday"] - df[r-1, "yrday"])) > MAX_DAILY_AVG) {
        df[r, "current_coal_weight"] <- df[r-1, "current_coal_weight"]
        break
      }
      
      # if neither of the above two situations are applicable, add unit(s) bought to the current record 
      if (DEBUG) {message("df[r, 'current_coal_weight']: ", df[r, "current_coal_weight"])}
      if (DEBUG) {message("df[r-1, 'current_coal_weight']: ", df[r-1, "current_coal_weight"])}
      if (DEBUG) {message("df[r, 'kg_bought']: ", df[r, "kg_bought"])}
      units_to_add <- ceiling((df[r, "current_coal_weight"] - (df[r-1, "current_coal_weight"]) - (df[r, "kg_bought"])) / (format_weights[[buying_format]]))
      if (VERBOSE) {message("Adding ", units_to_add, " units to the num of units bought...")}
      df[r, "units_bought_since_last_weighing"] <- df[r, "units_bought_since_last_weighing"] + units_to_add
      break
    }
  }
  
  return(df)
}
# --------------------------------------------- #
# --------------------------------------------- #
# load the data
load(paste(rdadir, "df_coal_data.Rda", sep = ""))
# --------------------------------------------- #
# prepare the data for the calculations that we want to do
# -------------------- #
# add a yrday (year day) column
df_coal_data$yrday <- yday(df_coal_data$date)

idx <- match(c("submission_id", "fieldworker_id", "latitude", "longitude", "survey_version"), table = names(df_coal_data))
if (any(!is.na(idx))) { df_coal_data <- df_coal_data[, - na.omit(idx)] }

idx <- match("other_format", colnames(df_coal_data))
if (!is.na(idx)) {
  if (all(is.na(df_coal_data$other_format))) {
    df_coal_data <- df_coal_data[, - idx]
  }
}
# -------------------- #
# split the data according to the different stands
stand_df_list <- split(x = df_coal_data, f = as.character(df_coal_data$stand_number))
# -------------------- #
# sort each stand's df according to date 
stand_df_list <- lapply(X = stand_df_list, FUN = function(s) {
  date_col <- s$date
  date_col_ordered <- sort(date_col)
  ordr <- match(date_col, date_col_ordered)
  s <- s[ordr,]
  
  return(s)
})

# --------------------------------------------- #
# do the actual calculations
stand_df_list <- lapply(X = stand_df_list, FUN = function(s) {

  if (VERBOSE) {message(unique(as.character(s$stand_number)))}
    
  # initialise
  s$kg_bought <- 0
  s$cumulative_coal_kg <- 0 # cumulative amount of coal that entered the system since the start of the monitoring period
  s$cumulative_usage_kg <- 0
  s$usage_net_kg <- 0

  # check for discrepancies in the 'current_coal_weight' column...
  if (is.na(s[1,"current_coal_weight"])) {
    warning("NA found as first recording of 'current_coal_weight'. Changing to 0.")
    s[1,"current_coal_weight"] <- 0
  }
  
  # ...and then initialise the 'cumulative_coal_kg' accordingly
  s[1, "cumulative_coal_kg"] <- s[1,"current_coal_weight"]
  
  # check for discrepancies in the 'units_bought_since_last_weighing' column and then initialise it if necessary...
  if (!is.na(s[1,"units_bought_since_last_weighing"])) {
    warning("Unexpected value detected for variable 'units_bought_since_laste_weighing' in the first record.")
  } 
  else { s[1,"units_bought_since_last_weighing"] <- 0 }
  
  # initialisation done; populate the rest of the data frame
  s <- calculate(s)
  
  # now check for errors, such as negative coal usage and try to fix it
  s <- check_coal(df = s)
  
  return(s)
})

stand_df_list <- lapply(X = stand_df_list, FUN = function(s) {
  s$stand_number <- as.character(s$stand_number)
  return(s)
})

# --------------------------------------------- #
# rejoin
df_coal_data_with_calc_cols <- do.call("rbind", stand_df_list)
df_coal_data_with_calc_cols$stand_number <- as.factor(df_coal_data_with_calc_cols$stand_number)

# --------------------------------------------- #
# save
archive(fileName = "df_coal_data_with_calc_cols.Rda", currentDir = rdadir, verbose = TRUE)
save(df_coal_data_with_calc_cols, file = paste(rdadir, "df_coal_data_with_calc_cols.Rda", sep = ""))
rm(stand_df_list, df_coal_data)

# --------------------------------------------- #
# --------------------------------------------- #
