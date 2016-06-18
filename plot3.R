#############################################################################
# Script name:      plot1.R
# Author:           Fabian Leuthold
# E-Mail:           fabian.leuthold@gmail.com
# Date:             June, 18th 2016
# 
# Project:          Exploratory Data Analysis, Coursera
#
# Our overall goal here is simply to examine how household energy usage varies 
# over a 2-day period in February, 2007. Your task is to reconstruct the fol-
# lowing plots below, all of which were constructed using the base plotting 
# system.
# 
# https://www.coursera.org/learn/exploratory-data-analysis/peer/ylVFo/course-
# project-1
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
# -> that's easy manageable with 8 GB RAM. let's read the whole stuff.

# ==============================================================
# function:
# returns a dataframe coming from a file containing a specified 
# set of datatypes and providing a specific date format and na-
# string
# ==============================================================
readTable <- function(fileName, fileDateFormat, dataTypes, naString) {
  read.table(file = sourceFile, header = TRUE, sep = ";", colClasses
             = dataTypes, na.strings = naString)
}

# ==============================================================
# function:
# clean label-names, convert to lowercase, remove blanks
# ==============================================================
washlabels <- function(labels) {
  lbl <- sapply(labels, tolower, USE.NAMES = FALSE)
  lbl <- sapply(lbl, gsub, pattern = "[?(-()#$!_ ]", replacement = "", USE.NAMES = FALSE)
  lbl <- sapply(lbl, gsub, pattern = "[,.]", replacement = "-", USE.NAMES = FALSE)
  lbl
}

# ==============================================================
# main programm
# ==============================================================
library(dplyr)
setwd("/home/phabi/Desktop/coursera/exploratory data analysis/git/ExData_Plotting1")
# set start/end-date to extract from the data
startDate <- as.POSIXct('2007-02-01 00:00:00', '%Y-%m-%d %H:%M:%S', tz = "", usetz = FALSE)
endDate <- as.POSIXct('2007-02-02 23:59:59', '%Y-%m-%d %H:%M:%S', tz = "", usetz = FALSE)
# define file parameters
sourceFile <- 'household_power_consumption.txt'
dataTypes <- c("character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")
fileDateFormat <- "%d/%m/%Y %H:%M:%S"
naString <- "?"

# read data
hhPower <- readTable(sourceFile, fileDateFormat, dataTypes, naString)
# wash the labels from the data set
names(hhPower) <- washlabels(names(hhPower))
# generate date-column from date/time character column
hhPower <- mutate(hhPower, dateTime = as.POSIXct(strptime(paste(date, time, " "),format = fileDateFormat)))
# now filter for desired data-range
hhPower <- filter(hhPower, dateTime >= startDate & dateTime <= endDate)
# remove obsolete date / time character rows
hhPower <- select(hhPower, -(date:time))

# create plot3
# set locale to us to have english weekdays-lables
Sys.setlocale("LC_TIME","en_US.UTF-8")
png(filename="plot3.png", width = 480, height = 480)
par(mar=c(3.1,4.1,2.1,2.1))
plot(hhPower$submetering1 ~ hhPower$dateTime, type = "l", ylab = "Energy sub metering", col = "black")
lines(hhPower$submetering2 ~ hhPower$dateTime, col = "red")
lines(hhPower$submetering3 ~ hhPower$dateTime, col = "blue")
legend("topright", legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
                   pch = c(NA, NA, NA), col = c("black", "red", "blue"), lty = c(1,1,1))
dev.off()

