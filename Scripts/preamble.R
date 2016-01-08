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
library(gdata)

# source additional functions and definitions
source(paste(novafunctdir, "fixname.R", sep = ""))
source(paste(novafunctdir, "archive.R", sep = ""))
source(paste(novafunctdir, "move.col.R", sep = ""))
source(paste(novafunctdir, "formatFields.R", sep = ""))
source(paste(novafunctdir, "makeItEasy.R", sep = ""))
source(paste(scriptdir, "EOPhhObj.R", sep = ""))

# declare GLOBAL constants
TIME_ZONE <- "Africa/Johannesburg"
par(cex.main = 0.8)
par(cex.sub = 0.7)
par(cex.lab = 0.7)
par(cex = 1)
par(mar = c(5.1, 4.1, 2.5, 2.1))

# define auxiliary functions

# ---------------------------------------- #

