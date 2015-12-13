# Program name:   load_power_data.R
# Author:         Michael Baraz
# Purpose:        Load dataset into R workspace

# Program name:   load_power_data.R
# Author:         Michael Baraz
# Purpose:        Load dataset into R workspace

#Set the date/time format
#date_time <- function(date, time) {
#  return (strptime(paste(date, time), "%d/%m/%Y %H:%M:%S"))
#}

#Create function that can be called by plot routines.
load_power_data <- function() {
  filename <- "household_power_consumption.txt"
  power <- read.table(filename,
                   header=TRUE,
                   sep=";",
                   colClasses=c("character", "character", rep("numeric",7)),
                   na="?")

  # convert date and time variables to Date/Time class
  power$Time <- strptime(paste(power$Date, power$Time), "%d/%m/%Y %H:%M:%S")
  power$Date <- as.Date(power$Date, "%d/%m/%Y")
  # filter dates 2007-02-01 and 2007-02-02
  dates <- as.Date(c("2007-02-01", "2007-02-02"), "%Y-%m-%d")
  # apply filter
  power <- subset(power, Date %in% dates)
  
  #return dataframe
  return(power)
  
} #end function load_power_data
