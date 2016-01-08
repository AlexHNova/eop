
library(lubridate)
source("C:/Users/Alex H/Dropbox (Nova SA)/Rfunctions/binconf.uur.R")

df_ibutton_data_classified$fire_start <- FALSE

splits <- split(x = df_ibutton_data_classified, f = df_ibutton_data_classified$stand_number)

temp <- lapply(X = splits, FUN = function(stnd) {
  if (!("vuur" %in% names(stnd))) {next}
  
  for (r in 2:nrow(stnd)) {
    if (is.na(stnd[r, "vuur"]) | is.na(stnd[r-1, "vuur"])) {next}
    
    if ((stnd[r, "vuur"] == "vuur") & (stnd[r-1, "vuur"] == "nie")) {
      stnd[r, "fire_start"] <- TRUE
    }
  }
  
  return(stnd)
})

temp <- do.call("rbind", temp)

temp$type <- paste(temp$energy_type, temp$insulation_type, sep = "_")
temp$hod <- hour(temp$date)
temp$mo <- month(temp$date)
temp$yr <- year(temp$date)
splits <- split(x = temp, f = temp$type)

catch <- lapply(X = splits, FUN = function(t) {
  #t$hod <- as.factor(t$hod)
#   t <- t[which(t$fire_start == "TRUE"),]
#   t_tbled <- table(t$hod, t$fire_start, exclude = NULL)
#   t_tbled$hod <- rownames(t_tbled)
#   
  #ggplot(data = t_tbled, mapping = aes(x = hod, y = "TRUE")) + geom_bar()
  tpe <- unique(t$type)
  nobs <- nrow(t)
  nstands <- length(unique(t$stand_number))
  ttle <- paste(tpe, "- Proportion of fires ignited\n by hour of day (with 95% CI; nobs = ", nobs, "; n_stands = ", nstands, ")", sep = "")
  ctch <- binconf.uur(x = t, uurvar = "hod", vuurvar = "fire_start", vuurnaam = "TRUE", maandvar = "mo", jaarvar = "yr", plot = "bar", stoor = FALSE, ttl = ttle, ylm = c(0, 0.1))
  return(ctch)
})

# ---------------------------------------------------------------------- #
# ---------------------------------------------------------------------- #
stand_df_list <- split(x = df_coal_data_with_calc_cols, f = df_coal_data_with_calc_cols$stand_number)

stand_df_list <- lapply(X = stand_df_list, FUN = function(x) {
  x$cumday <- x$yrday - min(x$yrday)
  x$days_between <- NA_integer_
  x$data_quality <- "theo"
  
  for (r in 2:nrow(x)) {
    x[r, "days_between"] <- x[r, "cumday"] - x[r-1, "cumday"]
    if (x[r, "units_bought_since_last_weighing"] < 1) {
      x[r, "data_quality"] <- "emp"
    }
  }
  x$mom_day_avg <- x$usage_net_kg / x$days_between
  
  return(x)
})

# ---------------------------------------------------------------------- #
sapply(X = stand_df_list, FUN = function(s) {
  print(max(s$mom_day_avg))
})

# ---------------------------------------------------------------------- #
fwsplits <- split(x = df_coal_data_wcc, f = df_coal_data_wcc$fieldworker_name)

lapply(X = fwsplits, FUN = function(x) {
  #print(summaryBy(data = x, formula = mom_day_avg ~ data_quality, FUN = max, na.rm = TRUE))
  summaryBy(data = x, formula = mom_day_avg ~ data_quality, FUN = function(x) {c(mean = mean(x, na.rm = TRUE), n = length(x))} )
})










  nms <- apply(X = as.array(subdirs), MARGIN = 1, FUN = function(nm) {
    nm <- tolower(nm)
    nm <- gsub(pattern = ".csv", replacement = "", x = nm, fixed = TRUE)
    nm <- gsub(pattern = c("basic/", "full/"), replacement = "", x = nm, fixed = TRUE)
    nm <- gsub(pattern = "eop_", replacement = "", x = nm, fixed = TRUE)
    str_elements <- (strsplit(x = nm, split = "_", fixed = TRUE))[[1]]
    stand_number <- str_elements[[1]]
    read_date <- str_elements[[3]]
    button_place <- str_elements[[4]]
    button_place <- substr(x = button_place, start = nchar(button_place), stop = nchar(button_place))
    
    return()
  })