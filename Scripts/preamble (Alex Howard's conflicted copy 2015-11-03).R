# Date created: 
# Owner: Nova Institute (Reg# 1994/002614/08).
# ---------------------------------------- #
# preamble to all scripts of the EOP Rproj
# ---------------------------------------- #

# define directories
if (!exists("dropdir")) {dropdir <- "C:/Users/Alex H/Dropbox (Nova SA)/"}
if (!exists("projdir")) {projdir <- paste(dropdir, "Eskom Offset Study 2014 - Team/R/eop/", sep = "")}
datadir <- paste(projdir, "Data/", sep = "")
figdir <- paste(projdir, "Figs/", sep = "")
reportdir <- paste(projdir, "Reports/", sep = "")
scriptdir <- paste(projdir, "Scripts/", sep = "")
rdadir <- paste(projdir, "Rdas/", sep = "")
outputdir <- paste(projdir, "Output/", sep = "")
novafunctdir <- paste(dropdir, "Rfunctions/", sep = "") 
novapackdir <- paste(dropdir, "Rpackages/", sep = "")

# load necessary libraries
library(openxlsx)
library(plyr)
library(stringr)
library(lubridate)
library(openair)
library(dplyr)
library(reshape2)
library(ggplot2)

# source additional functions
source(paste(novafunctdir, "fixname.R", sep = ""))
source(paste(novafunctdir, "archive.R", sep = ""))

# declare GLOBAL constants
TIME_ZONE <- "Africa/Johannesburg"
TIME_STEP <- 20 # minutes

# define auxiliary functions

# ---------------------------------------- #