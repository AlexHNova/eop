# for each df, determine the average amount of coal used per fire 
# ------------------------------------------ #

firevar <- "vuur"

data_list <- split(x = df_all_combined, f = df_all_combined$stand_number)

list_coal_per_fire <- lapply(X = data_list, FUN = function(df) {

  print(paste(unique(df$energy_type), unique(df$insulation_type), sep = "_"))
  avg_coal_per_fire_per_wperiod <- vector(mode = "numeric", length = length(names(table(df$weighing_period))))
  periods <- as.array(as.integer(names(table(df$weighing_period))))
  df$avg_coal_per_fire <- NA_real_
  
  avg_coal_per_fire_per_wperiod <- apply(X = periods, MARGIN = 1, FUN = function(p) {
    total_coal_used <- unique(df[which(df$weighing_period == p), "usage_net_kg"])
    if (firevar %in% names(table((rle(df[which(df$weighing_period == p), firevar]))$values))) {
      num_of_fires <- (table((rle(df[which(df$weighing_period == p), firevar]))$values))[[firevar]] 
    } else {num_of_fires <- 0}
    
    if (!(is.na(total_coal_used) | is.na(num_of_fires) | (num_of_fires == 0))) {
      df[which(df$weighing_period == p), "avg_coal_per_fire"] <<- round((total_coal_used / num_of_fires), digits = 3)
    } else {
        warning(paste("Could not calculate coal for the current period. Returning NA.", sep = ""))
        df[which(df$weighing_period == p), "avg_coal_per_fire"] <<- NA_real_
    }
  })
  
  print(mean(na.omit(avg_coal_per_fire_per_wperiod)))
  return(df)
})

df_all_combined_with_coal_per_fire <- do.call("rbind", list_coal_per_fire)
df_all_combined_with_coal_per_fire$vuur <- as.factor(df_all_combined_with_coal_per_fire$vuur)

save(df_all_combined_with_coal_per_fire, file = "df_all_combined_with_coal_per_fire.Rda")


# plots (visual exploration)

df_all_combined_with_coal_per_fire$type <- paste(df_all_combined_with_coal_per_fire$energy_type, df_all_combined_with_coal_per_fire$insulation_type, sep = "_")
df_all_combined_with_coal_per_fire$type <- as.factor(df_all_combined_with_coal_per_fire$type)

ggplot(data = df_all_with_coal_per_fire, mapping = aes(x = date, y = avg_coal_per_fire)) + facet_grid(facets = insulation_type ~ energy_type) + xlim(as.POSIXct("2015-06-01 00:00:00"), as.POSIXct("2015-07-31 23:59:59")) + geom_line() + geom_smooth()


ggplot(data = df_all_with_coal_per_fire, mapping = aes(x = date, y = avg_coal_per_fire)) + facet_grid(facets = . ~ insulation_type + energy_type) + xlim(as.POSIXct("2015-06-01 00:00:00"), as.POSIXct("2015-07-31 23:59:59")) + geom_boxplot()
