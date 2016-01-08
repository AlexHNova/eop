# Date created: 18 November 2015
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# Script to prepare an energy matrix from the DES data for each household in the housholdList
# ---------------------------------------- #
# start clean
rm(list = ls())
# ---------------------------------------- #
# source preamble
dropdir <- "C:/Users/Alex H/Dropbox (Nova SA)/"
projdir <- paste(dropdir, "Eskom Offset Study 2014 - Team/R/eop/", sep = "")
source(paste(projdir, "Scripts/preamble.R", sep = ""))
# ---------------------------------------- #
# declare script constants
VERBOSE <- TRUE
DEBUG <- TRUE
# ---------------------------------------- #
# ---------------------------------------- #

# get the matrix template
matrixTemplate <- read.csv2(file = paste(outputdir, "EnergyProfileMatrix.csv", sep = ""), header = TRUE, sep = ';', stringsAsFactors = FALSE, na.strings = "")

# get the household list
load(paste(rdadir, "houseHoldList.Rda", sep = ""))

# ---------------------------------------- #
# do the magic

# wag, let's first write a function so that we can process een huisietjie op 'n slag

desMatrix <- function(hh = NULL) {
  if (is.null(hh)) {
    warning("No argument received by parameter hh. Returning.")
    return(hh)
  }
  
  if (!(class(hh) == "EOPhouseHold")) {
    stop("Argument for parameter hh must be of type 'EOPhouseHold'. Returing NULL.")
    return(NULL)
  }
  
  mtrx <- matrixTemplate
  
  for (cno in 3:ncol(mtrx)) {
    # determine whether we should source from the pre-intervention DES or the post-intervention DES
    desSource <- mtrx[1, cno]
    
    if (desSource == "df_des_data_pre") {
      # if we should source from the pre-intervention DES, determine in which one of the two we will find the data for the particular stand
      if (is.null(hh@DESdata$data_df[[1]]) & is.null(hh@DESdata$data_df[[2]])) {
        next
      }
      if (is.null(hh@DESdata$data_df[[2]])) {
        desDf <- hh@DESdata$data_df[[1]]
      } else {
        desDf <- hh@DESdata$data_df[[2]]
      }
    } else {
      if (desSource != "df_des_data_post") {
        warning("Uh-oh...unknown source name received...")
        next
      }
      if (is.null(hh@DESdata$data_df[[3]])) {next}
      desDf <- hh@DESdata$data_df[[3]]
    }
    
    for (rno in 2:nrow(mtrx)) {
      if (is.na(mtrx[rno, cno])) {next}
      
      varName <- mtrx[rno, cno]
      if (!(varName %in% names(desDf))) {
        mtrx[rno, cno] <- NA
        next
      }
      
      uniq <- unique(desDf[[varName]])
      uniq <- uniq[!is.na(uniq)]
      if (length(uniq) < 1) {
        uniq <- NA
      } 
      if (length(uniq) > 1) {
        warning("More than one uniq identifier found in ", varName, " for stand ", hh@address@standNumber, " in ", desSource, ": ", uniq,". Only the first will be used")
        uniq <- uniq[[1]]
      }
      mtrx[rno, cno] <- uniq
    }
  }
  
  # remove the first row of the mtrx that simply served internal purposes
  mtrx <- mtrx[2:nrow(mtrx),]
  
  names(mtrx) <- fixname(names(mtrx))
  names(mtrx)[1:2] <- ""
  mtrx[[1]] <- format.char(mtrx[[1]])
  mtrx[[2]] <- format.char(mtrx[[2]])
  
  # put the units row in
  
  
  return(mtrx)
}



# ---------------------------------------- #
# ---------------------------------------- #
# NOTES



# ---------------------------------------- #
# ---------------------------------------- #
# doodle code...
## ------------------- ##
