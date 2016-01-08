# script to classify fires

# ---------------------------------------- #
# start clean
rm(list = ls())

# define directories
dropdir <- "C:/Users/Alex H/Dropbox (Nova SA)/"
projdir <- paste(dropdir, "Eskom Offset Study 2014 - Team/R/eop/", sep = "")
source(paste(projdir, "/Scripts/preamble.R", sep = ""))

# source additional functions
source(paste(scriptdir, "maak.vuur.R", sep = ""))
source(paste(rfunctdir, "count_fires.R", sep = ""))

# load the data to be classified
load(file = paste(rdadir, "df_ibutton_data_prepared.Rda", sep = ""))

# ---------------------------------------- #
# split the data according to stand number
list_ibutton_data_by_stand <- split(x = df_ibutton_data_prepared, f = df_ibutton_data_prepared$stand_number)

# now apply the classification function over each stand's data
list_ibutton_data_by_stand <- lapply(X = 1:length(list_ibutton_data_by_stand), FUN = function(s) {
  df <- list_ibutton_data_by_stand[[s]]
  stand = names(list_ibutton_data_by_stand)[[s]]
  df <- maak.vuur(df = df, datum.naam = "date", stoofnaam = "C", verbose = TRUE, debug =TRUE, stoordir = paste(scriptdir, "Vuurklassifikasie_codes/", sep=""), naam.var = "stand_number")
  
  filename <- paste("stand_", stand, "_classified.Rda", sep = "")
  save(df, file = paste(rdadir, filename, sep = ""))
  return(df)
})

df_ibutton_data_classified <- do.call("rbind", list_ibutton_data_by_stand)
rm(list_ibutton_data_by_stand)
save(df_ibutton_data_classified, file = paste(rdadir, "df_ibutton_data_classified.Rda", sep = ""))

# ---------------------------------------- #
# now count the number of fires made by each stand across certain periods
df_list <- split(df_ibutton_data_classified, f = df_ibutton_data_classified$stand_number)

fpd <- lapply(X = df_list, FUN = function(s) {
  results <- count.fires.per.day(df = s, datevar = "date", firevar = "vuur", vuurnaam = "vuur", raw = FALSE)
  return(results)
})

catch <- lapply(X = fpd, FUN = function(r) {
  if (!is.null(r[[1]])) {
    print(r[[1]])
  }
  
  if (!is.null(r[[2]])) {
    print(r[[2]])
  }
})

