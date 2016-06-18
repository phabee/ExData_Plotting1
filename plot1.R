#############################################################################
# Script name:      plot1.R
# Author:           Fabian Leuthold
# E-Mail:           fabian.leuthold@gmail.com
# Date:             June, 17th 2016
# 
# Project:          Exploratory Data Analysis, Coursera
#
#############################################################################

# a) calculate the approx. memory needed to read the whole data
# ==============================================================
# sample row: 
# 16/12/2006;17:24:00;4.216;0.418;234.840;18.400;0.000;1.000;17.000^M
#
# memory calculation:
# library(pryr)
# rowlen <- 2 * object_size(date()) + 7* object_size(numeric())
# memRequested <- 2075259 * rowlen; memRequested
# Total Memory Requested: 1.08 GB
#
# -> let's extract a subset to reduce memory load and processing 
#   time and get some more practice with R...

# ==============================================================
# extractFileSubset: 
# reads the file in the current wd and asserts, that within the
# data a date from interval [startDate, endDate] is contained
# and writes them into a new file named 'stub-*'
# ==============================================================
extractFileSubset <- function(startDate, endDate, fileName) {
  expectedDateFormat <- '%d/%m/%Y'
  hasDateInRange <- function(line, startDate, endDate) {
    attrs <- strsplit(line, ";")
    as.Date(attrs[[1]], expectedDateFormat) >= startDate && as.Date(attrs[[1]], expectedDateFormat) <= endDate
  }
  
  rcon <- file(fileName, "r", blocking = FALSE)
  wcon <- file(paste0('stub-', fileName), "w", blocking = TRUE)
  
  lineBuffer <-readLines(rcon)
  # loop through all lines of input file
  for (i in 1:length(lineBuffer)){
    # if we're in the header or a valid date is found -> take line over
    if (i == 1 || hasDateInRange(lineBuffer[i], startDate, endDate)) {
      print(lineBuffer[i])
      writeLines(lineBuffer[i], wcon)
    }
  }
  close(wcon)
  close(rcon)
}

# ==============================================================
# main programm
# ==============================================================
setwd("/home/phabi/Desktop/coursera/exploratory data analysis/git/ExData_Plotting1")
# set start/end-date to filter the file
startDate <- as.Date('2007-02-01', '%Y-%m-%d')
endDate <- as.Date('2007-02-02', '%Y-%m-%d')
sourceFile <- 'household_power_consumption.txt'
extractFileSubset(startDate, endDate, sourceFile)


