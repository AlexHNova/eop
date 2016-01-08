# Date created: 15 September 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# --------------------------------------------- #
# start clean
rm(list = ls())

# define directories
rdadir <- paste(getwd(), "/Rdas/", sep = "")

# load the data
load(paste(rdadir, "df_coal_data.Rda", sep = ""))

# declare script constants
FORMAT_WEIGHTS <- c("Big Bag" = 79, "Small Bag" = 40, "Tin" = 17)
DEBUG <- TRUE
# --------------------------------------------- #

# --------------------------------------------- #
# split the data according to the different stands
stand_df_list <- split(x = df_coal_data, f = df_coal_data$stand_number)

# now subset the data until we have only good records to work with

# --------------------- #
## w.r.t. the 'revisit' column
# t <- table(df_coal_data$revisit, df_coal_data$stand_number)
# # t_more_than_one_first_visit <- t[, which(t["no",] > 1)]
# t_exactly_one_first_visit <- t[, which(t["no",] == 1)]
# t_no_first_visit <- t[, which(t["no",] < 1)]
# 
# stands_with_usable_revisit <- colnames(t_exactly_one_first_visit)

# --------------------- #
## w.r.t. the "coal_buying_format" column
buying_formats <- sapply(X = stand_df_list, FUN = function(s) { unique(na.omit(as.character(s$coal_buying_format))) })
buying_formats_known <- sapply(X = buying_formats, FUN = function(s) {length(s) > 0})
notknown <- names(buying_formats_known[!buying_formats_known])

# set the coal_buying_format as default "Small Bag" for those stands of which the buying format is unknown
for (i in 1:length(notknown)) {
  stnd <- notknown[[i]]
  print(stnd)
  stand_df_list[[stnd]]$coal_buying_format <- "Small Bag"
}

# stands_with_usable_coal_buying_format <- names(buying_formats[buying_formats_known])

# --------------------- #
## w.r.t. the "units_bought_since_last_weighing" column - drop die houses for which we have no such info at all
# has_empty_units_bought_column <- sapply(X = stand_df_list, FUN = function(s) { all(is.na(s$units_bought_since_last_weighing)) })
# stands_with_usable_units_bought_columns <- names(stand_df_list)[!has_empty_units_bought_column]

# --------------------- #
# ## so, the good stands are...
# good_stands <- intersect(stands_with_usable_revisit, stands_with_usable_coal_buying_format)
# good_stands <- intersect(good_stands, stands_with_usable_units_bought_columns)
# 
# # therefore...
# stand_df_list <- stand_df_list[good_stands]

# --------------------- #

# now the calculations can begin
# --------------------------------------------- #

# sort each stand's df according to date 
stand_df_list <- lapply(X = stand_df_list, FUN = function(s) {
  date_col <- s$date
  date_col_ordered <- sort(date_col)
  ordr <- match(date_col, date_col_ordered)
  s <- s[ordr,]
  
  return(s)
})

# remove unnecessary columns and populate the other important columns, if possible
stand_df_list <- lapply(X = stand_df_list, FUN = function(s) {
  # remove
  idx <- match(c("submission_id", "fieldworker_id", "latitude", "longitude", "survey_version"), table = names(s))
  if (any(!is.na(idx))) { s <- s[, - na.omit(idx)] }
  
  idx <- match("other_format", colnames(s))
  if (!is.na(idx)) {
    if (all(is.na(s$other_format))) {
      s <- s[, - idx]
    }
  }
  
  # populate
  idx <- match(x = "no", table = s$revisit)
  
  if (!is.na(idx)) {
    if (!is.na(s[idx, "coal_buying_format"])) {
      s$coal_buying_format <- as.character(s[idx,"coal_buying_format"])
    } else {warning("Detected stand with no certain coal buying format.")}
   
    if (!is.na(s[idx,"purchase_frequency"])) {
      s$purchase_frequency <- as.character(unique(s[which(s$revisit == "no"),"purchase_frequency"]))
    } else {warning("Detected stand with no certain purchase frequency.")}
  }
  return(s)
})

# do the actual coal calculations
idx <- 0
stand_df_list <- lapply(X = stand_df_list, FUN = function(s) {
  
  idx <<- idx + 1
  print(idx)
  
  # initialise
  s$kg_bought <- 0
  s$cumulative_coal_kg <- 0 # cumulative amount of coal that entered the system since the start of the monitoring period
  s$cumulative_usage_kg <- 0
  s$usage_net_kg <- 0
  
  # check for discrepancies in the 'revisit' column
  idx <- match(x = "no", table = s$revisit)
  if (!is.na(idx)){
    if (!match("no", table = s$revisit) == 1) {
      warning("Stand detected where date of first visit does not match date of first record for stand.")
    }
  }
  
  # check for discrepancies in the 'current_coal_weight' column...
  if (is.na(s[1,"current_coal_weight"])) {
    warning("NA found as first recording of 'current_coal_weight'. Changing to 0.")
    s[1,"current_coal_weight"] <- 0
  }
  
  # ...and then initialise the 'cumulative_coal_kg' accordingly
  s[1, "cumulative_coal_kg"] <- s[1,"current_coal_weight"]
  
  
  # check for discrepancies in the 'units_bought_since_last_weighing' column...
  if (!(is.na(s[1,"units_bought_since_last_weighing"]) | s[1,"units_bought_since_last_weighing"] > 0)) {
    warning("Positive value detected for variable 'units_bought_since_laste_weighing' in the first record.")
  } else { 
    if (is.na(s[1,"units_bought_since_last_weighing"])) {
      s[1,"units_bought_since_last_weighing"] <- 0}
  }
  
  # ...and then initialise the 'kg_bought' column accordingly
  if (DEBUG) {print(unique(s$coal_buying_format))}
  s$kg_bought <- s$units_bought_since_last_weighing * FORMAT_WEIGHTS[[unique(as.character(s$coal_buying_format))]]
  
  # initialisation done; populate the rest of the data frame
  for (k in 2:nrow(s)) {
    m <- (k - 1) 
    
    s[k, "cumulative_coal_kg"] <- ifelse(is.na(s[m, "cumulative_coal_kg"]), 0, s[m, "cumulative_coal_kg"]) + 
                                  ifelse(is.na(s[k, "kg_bought"]), 0, s[k, "kg_bought"])
    s[k, "cumulative_usage_kg"] <-  ifelse(is.na(s[k, "cumulative_coal_kg"]), 0, s[k, "cumulative_coal_kg"]) - 
                                    ifelse(is.na(s[k, "current_coal_weight"]), 0, s[k, "current_coal_weight"])
    s[k, "usage_net_kg"] <- ifelse(is.na(s[k, "cumulative_usage_kg"]), 0, s[k, "cumulative_usage_kg"]) - 
                            ifelse(is.na(s[m, "cumulative_usage_kg"]), 0, s[m, "cumulative_usage_kg"])
  }

  print(s)
  return(s)
})

stand_df_list <- lapply(X = stand_df_list, FUN = function(s) {
  s$stand_number <- as.character(s$stand_number)
  return(s)
})

df_coal_data_with_calc_cols <- do.call("rbind", stand_df_list)
df_coal_data_with_calc_cols$stand_number <- as.factor(df_coal_data_with_calc_cols$stand_number)

save(df_coal_data_with_calc_cols, file = paste(rdadir, "df_coal_data_with_calc_cols.Rda", sep = ""))
rm(stand_df_list, df_coal_data, df_coal_data_with_calc_cols)


