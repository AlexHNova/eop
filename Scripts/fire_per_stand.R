# script to determine the average amount of fires per day for a stand of each intervention type
# the output is a data frame giving, for each stand, its stand_number, intervention type and avg fires per day
# ----------------------------------------- #

# define directories
dropdir <- "C:/Users/Alex H/Dropbox (Nova SA)/"
datadir <- paste(getwd(), "/Data/", sep = "")
figdir <- paste(getwd(), "/Figs/", sep = "")
reportdir <- paste(getwd(), "/Reports/", sep = "")
scriptdir <- paste(getwd(), "/ScriptsAndRdas/", sep = "")
rdadir <- paste(getwd(), "/ScriptsAndRdas/", sep = "")
rfunctdir <- paste(dropdir, "Rfunctions/", sep = "")

# load the necessary libraries 
library(plyr)

# load the input data
load(paste(rdadir, "df_ibutton_data_classified.Rda", sep = ""))

# declare constants
df_input <- df_ibutton_data_classified
rm(df_ibutton_data_classified)

# source additional functions
source(file = paste(rfunctdir, "count_fires.R", sep = ""))

# ----------------------------------------- #
# now determine the avg amount of fires per day per stand for each type

df_input$vuur <- as.factor(df_input$vuur)
df_input$type <- paste(df_input$energy_type, df_input$insulation_type, sep = "_")

df_output <- data.frame(matrix(nrow = nlevels(df_input$stand_number), ncol = 3), stringsAsFactors = FALSE)
colnames(df_output) <- c("stand_number", "type", "avg_fires_p_day")
df_output$stand_number <- levels(df_input$stand_number)
stand_list <- split(x = df_input, f = df_input$stand_number)

catch <- lapply(X = stand_list, FUN = function(s) {
  snr <- unique(as.character(s$stand_number))
  print(snr) 
  if (snr == "3185") {return(1)} # faulty data
  
  # LPG houses did not have a stove to make fire in, so just skip them
  df_output[which(df_output$stand_number == snr), "type"] <<- unique(as.character(s$type)) 
  if (unique(as.character(s$energy_type)) == "LPG") {
    return(0)
  }
  
  fpd <- count.fires.per.day(df = s, datevar = "date", firevar = "vuur", vuurnaam = "vuur")
  fpd <- fpd[["res_sum"]]
  avg <- mean(fpd$mean_fires_per_day, na.rm = TRUE)
  
  df_output[which(df_output$stand_number == snr), "avg_fires_p_day"] <<- avg
})

df_output <- df_output[complete.cases(df_output),]
n_type <- table(df_output$type)

xlabel <- vector(mode = "character")
for(i in 1:length(n_type)) {
  if (length(xlabel) > 0) {xlabel <- paste(xlabel, ", ", sep = "")}
  xlabel <- paste(xlabel, names(n_type)[[i]], " (n = ", n_type[[i]], ")", sep = "")
}

ggplot(data = df_output, mapping = aes(x = type, y = avg_fires_p_day)) + xlab(xlabel) + geom_boxplot()
