# Date created: 16 September 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# prepscript for the EOP coal weighing data
# ---------------------------------------- #
# start clean
rm(list = ls())
# ---------------------------------------- #
# source preamble
dropdir <- "C:/Users/Alex H/Dropbox (Nova SA)/"
projdir <- paste(dropdir, "Eskom Offset Study 2014 - Team/R/eop/", sep = "")
source(paste(projdir, "/Scripts/preamble.R", sep = ""))
# ---------------------------------------- #
# load libraries
#
# ---------------------------------------- #
# source additional functions
#
# ---------------------------------------- #
# declare script constants
#
# ---------------------------------------- #
# define auxiliary functions
#
# ---------------------------------------- #
# ---------------------------------------- #

# read the data
df_coal_data <- read.csv(file = paste(datadir, "Coal weighing/", "EOP_coal_weighing_20150907-130444.csv", sep = ""), header = TRUE, stringsAsFactors = FALSE)

# ---------------------------------------- #
# standardise the column names
names(df_coal_data) <- fixname(names(df_coal_data))

idx <- match("ï_submission_id", table = names(df_coal_data), nomatch = -1)
if (idx > 0) {names(df_coal_data)[idx] <- "submission_id"}

# ---------------------------------------- #
# convert all text columns to lower case and remove unnecessary spaces
for (v in 1:ncol(df_coal_data)) {
  if (is.character(df_coal_data[[v]])) {
    temp <- try(tolower(df_coal_data[[v]]))
    if (length(temp) == length(df_coal_data[[v]])) { df_coal_data[[v]] <- temp
    df_coal_data[[v]] <- gsub(pattern = "[[:space:]]{2,}", replacement = " ", x = df_coal_data[[v]])
    }
  }  
}

# ---------------------------------------- #
# remove unnecessary columns
idxx <- match(x = c("received", "end", "duration_seconds", "language", "device", "modified_by", "modified_on"), table = names(df_coal_data))
if (any(!is.na(idxx))) {df_coal_data <- df_coal_data[, -na.omit(idxx)]} 

# ---------------------------------------- #
# remove duplicate/problematic records
## duplicates from dan mahlangu's submissions
drops <- c( "c666a690-5477-4eaa-b0ea-0f952a9e045a",
            "a779f6c2-e9a6-495f-a6e0-282b5cb292a9",
            "2b377a7c-26b8-41b1-bc68-cd5e2c5c80b2",
            "66b0953e-10d5-446c-9fb9-61f89eb208c2",
            "d92520cf-6456-4082-9bf1-c31b8d2973f3",
            "5ef6cd74-0f1f-4958-977d-7772147170bb",
            "948ca8ea-cc3e-42ac-b479-9b5a9950642e",
            "86d84c2b-31e8-4e17-8e45-112716e9eec9",
            "fc9d6145-739a-4318-a370-47fd06554592",
            "9d4b8e42-9969-4fb1-b68e-7e52a43841be",
            "63b7fcbe-ad08-41ac-b707-81a72c5d6ce5",
            "e911571a-f3fe-442b-be36-16525ad5c6b2",
            "33734616-42c5-4bb2-be63-247a744f0d64" )

dropidxx <- match(x = drops, table = df_coal_data$submission_id)
if (any(!is.na(dropidxx))) {
  df_coal_data <- df_coal_data[-dropidxx[!is.na(dropidxx)],]
}

# duplicates from nonhlanhla's submissions
drops <- c( "89a510b5-fad7-4370-bb3c-21f7480de227",
            "e8b28153-abf9-45f4-bb94-a45d4228c697" )

dropidxx <- match(x = drops, table = df_coal_data$submission_id)
if (any(!is.na(dropidxx))) {
  df_coal_data <- df_coal_data[-dropidxx[!is.na(dropidxx)],]
}

# ---------------------------------------- #
# fix the date column
idx <- match("start", table = names(df_coal_data))

if (!is.na(idx)) {
  df_coal_data$start <- gsub(pattern = "[[:space:]]{1,}", replacement = "-", x = df_coal_data$start)
  
  temp <- apply(as.array(df_coal_data$start), MARGIN = 1, FUN = function(d) {
    date_elements <- strsplit(x = d, split = "-")[[1]]
    mo <- date_elements[[2]]
    if (nchar(mo) < 2) { mo <- paste("0", mo, sep = "") }
    day <- date_elements[[1]]
    if (nchar(day) < 2) { day <- paste("0", day, sep = "") }
    yr <- date_elements[[3]]
    tme <- date_elements[[4]]
    date <- paste(day, mo, yr, sep = "-")
    date <- paste(date, tme, sep = " ")
    return(date)
  })

  temp <- as.POSIXct(temp, format = "%d-%m-%Y %T")
  df_coal_data$start <- temp
  rm(temp)
  names(df_coal_data)[match(x = "start", table = names(df_coal_data))] <- "date"
}

# ---------------------------------------- #
# fix the stand_number column
df_coal_data$stand_number <- as.character(x = df_coal_data$stand_number)

matches <- match(x = 0.1895, table = df_coal_data$stand_number)
if (!is.na(matches)) {
  df_coal_data[matches, "stand_number"] <- 1895
}

idx <- match(x = "12.5", table = df_coal_data$stand_number)
if (!is.na(idx)) {
  df_coal_data[idx, "stand_number"] <- "3805"
}

idx <- match(x = "4391", table = df_coal_data$stand_number)
if (!is.na(idx)) {
  df_coal_data[idx, "stand_number"] <- "4291"
}

idx <- match(x = "3959", table = df_coal_data$stand_number)
if (!is.na(idx)) {
  df_coal_data <- df_coal_data[which(df_coal_data$stand_number != "3959"),]
}

idx <- match(x = "3224", table = df_coal_data$stand_number)
if (!is.na(idx)) {
  df_coal_data <- df_coal_data[which(df_coal_data$stand_number != "3224"),]
}

# ---------------------------------------- #
# # remove the stands of which the weighing only started somewhere after the 6th of July 2015
# dte <- as.POSIXct("2015-07-06 00:00:00")
# standsplits <- split(x = df_coal_data, f = df_coal_data$stand_number)
# mindates <- sapply(X = standsplits, FUN = function(s) {min(s$date)})
# toolate <- (mindates > dte)
# standstokeep <- names(mindates[!toolate])
# df_coal_data <- df_coal_data[which(as.character(df_coal_data$stand_number) %in% standstokeep),]
# rm(standsplits)

# ---------------------------------------- #
# fix the errors in the 'current_coal_weight' column
idx <- which((as.character(df_coal_data$stand_number) == "3330") & (df_coal_data$current_coal_weight == 790))
if (!is.na(idx)) {
  df_coal_data[idx, "current_coal_weight"] <- 79
}

idx <- which((as.character(df_coal_data$stand_number) == "3330") & (df_coal_data$current_coal_weight == 210))
if (!is.na(idx)) {
  df_coal_data[idx, "current_coal_weight"] <- 21
}

idx <- match(x = 370, table = df_coal_data$current_coal_weight)
if (!is.na(idx)) {
  df_coal_data <- df_coal_data[which(df_coal_data$current_coal_weight != 370),]
}

idx <- which((as.character(df_coal_data$stand_number) == "3423") & (df_coal_data$current_coal_weight == 10.1))
if (!is.na(idx)) {
  df_coal_data[idx, "current_coal_weight"] <- 10.5
}

# ---------------------------------------- #
# fix the errors in the "units_bought_since_last_weighing" column
standsplits <- split(x = df_coal_data, f = as.character(df_coal_data$stand_number))
# -------------------- #
## - typos in data for stand 3466
idx <- match(x = "3466", table = names(standsplits))
if (!is.na(idx)) {
  s3466 <- standsplits[["3466"]]
  
  idx <- match(x = 5, table = s3466$units_bought_since_last_weighing)
  if (!is.na(idx)) {
    s3466[idx, "units_bought_since_last_weighing"] <- 2
  }
  
  idx <- match(x = 6, table = s3466$units_bought_since_last_weighing)
  if (!is.na(idx)) {
    s3466[idx, "units_bought_since_last_weighing"] <- 3
  }
  
  standsplits[["3466"]] <- s3466
  rm(s3466)
}
# -------------------- #
## error in one record for s172 (another fw weighed that one on behalf of nonhlanhla and slipped up a bit)
idx <- match(x = "172", table = names(standsplits))
if (!is.na(idx)) {
  s172 <- standsplits[["172"]]
  
  idx <- which(s172$fieldworker_name == "dan mahlangu")
  if (!is.na(idx)) {
    s172[idx, "units_bought_since_last_weighing"] <- 1
  }
  
  standsplits[["172"]] <- s172
  rm(s172)
}
# -------------------- #
## error in one record for stand s3152
idx <- match(x = "3152", table = names(standsplits))
if (!is.na(idx)) {
  s3152 <- standsplits[["3152"]]
  idx <- match(x = 4, table = s3152$units_bought_since_last_weighing)
  if (!is.na(idx)) {
    s3152[idx, "units_bought_since_last_weighing"] <- 1
  }
  standsplits[["3152"]] <- s3152
  rm(s3152)
}
# -------------------- #
df_coal_data <- do.call("rbind", standsplits)
rm(standsplits)

# ---------------------------------------- #
# factorise the factorisable columns
## 1. the 'revisit' column
idx <- match("revisit", table = names(df_coal_data))
if (!is.na(idx)) {
  df_coal_data[[idx]] <- as.factor(df_coal_data[[idx]])
  df_coal_data[[idx]] <- addNA(x = df_coal_data[[idx]], ifany = TRUE)
}
# -------------------- #
## 2. the "coal_buying_format" column
idx <- match(x = "coal_buying_format", table = names(df_coal_data))
if (!is.na(idx)) {
  df_coal_data[which(df_coal_data$coal_buying_format == 1), "coal_buying_format"] <- "Big Bag"
  df_coal_data[which(df_coal_data$coal_buying_format == 2), "coal_buying_format"] <- "Small Bag"
  df_coal_data[which(df_coal_data$coal_buying_format == 3), "coal_buying_format"] <- "Tin"
  df_coal_data[which(df_coal_data$coal_buying_format == 4), "coal_buying_format"] <- "Other"
  
  # currently each stand has at most 1 record containing the coal_buying_format, so identify the cbf for each stand and then populate all its cells in the coal_buying_format field with the identified cbf
  standsplits <- split(x = df_coal_data, f = as.character(df_coal_data$stand_number))
  standsplits <- lapply(X = standsplits, FUN = function(s) {
    cbf <- unique(s$coal_buying_format)  
    if (any(is.na(cbf))) {cbf <- cbf[!is.na(cbf)]}
    cbf <- ifelse(length(cbf) == 1, cbf, "Unknown")
    s$coal_buying_format <- cbf
    return(s)
  })
  
  df_coal_data <- do.call("rbind", standsplits)
  rm(standsplits)
  
  df_coal_data$coal_buying_format <- as.factor(x = df_coal_data$coal_buying_format)
  df_coal_data$coal_buying_format <- addNA(x = df_coal_data$coal_buying_format, ifany = TRUE)
}
# -------------------- #
## 3. the 'purchase frequency' column
idx <- match(x = "purchase_frequency", table = names(df_coal_data))
if (!is.na(idx)) {
  df_coal_data$purchase_frequency <- as.character(df_coal_data$purchase_frequency)
  df_coal_data[which(df_coal_data$purchase_frequency == 1),"purchase_frequency"] <- "As needed"
  df_coal_data[which(df_coal_data$purchase_frequency == 2),"purchase_frequency"] <- "Monthly"
  df_coal_data[which(df_coal_data$purchase_frequency == 3),"purchase_frequency"] <- "Every Fortnight"
  df_coal_data[which(df_coal_data$purchase_frequency == 4),"purchase_frequency"] <- "Weekly"
  df_coal_data[which(df_coal_data$purchase_frequency == 5),"purchase_frequency"] <- "Other"
  
  # currently each stand has at most 1 record containing info in the "purchase_frequency" column, so identify the pf for each stand and then populate all its cells in the purchase_frequency field with the identified pf.
  standsplits <- split(x = df_coal_data, f = as.character(df_coal_data$stand_number))
  standsplits <- lapply(X = standsplits, FUN = function(s) {
    pf <- unique(s$purchase_frequency)
    if (any(is.na(pf))) {pf <- pf[!is.na(pf)]}
    pf <- ifelse(test = length(pf) == 0, pf, "Unknown")
    s$purchase_frequency <- pf
    return(s)
  })
  
  df_coal_data <- do.call("rbind", standsplits)
  rm(standsplits)
  
  df_coal_data$purchase_frequency <- as.factor(x = df_coal_data$purchase_frequency)
  df_coal_data$purchase_frequency <- addNA(x = df_coal_data$purchase_frequency, ifany = TRUE)
}
# -------------------- #
## 4. the 'stand number' column - for anonimity
df_coal_data$stand_number <- as.character(x = df_coal_data$stand_number)
df_coal_data$stand_number <- as.factor(x = df_coal_data$stand_number)
df_coal_data$stand_number <- addNA(x = df_coal_data$stand_number, ifany = TRUE)

# ---------------------------------------- #
# fix the 'unit_price' field
df_coal_data$unit_price <- gsub(pattern = "^[[:blank:]]", replacement = "", x = df_coal_data$unit_price)
df_coal_data[which(nchar(df_coal_data$unit_price) == 0), "unit_price"] <- NA_character_

# actually, the field is useless, so just remove it
df_coal_data <- remove.col(df = df_coal_data, colName = "unit_price")

# ---------------------------------------- #
# save the prepared data set as a whole
archive(fileName = "df_coal_data.Rda", currentDir = rdadir, verbose = TRUE)
save(df_coal_data, file = paste(rdadir, "df_coal_data.Rda", sep = ""))
