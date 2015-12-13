# Program name:   plot2.R
# Author:         Michael Baraz
# Purpose:        Create time series plot per assignment


#Read file into a dataframe and set date and time columns
power <- read.table("household_power_consumption.txt",
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


#create PNG file
png("plot2.png", height=480, width=480)

#plot the graph
plot(power$Time,
     power$Global_active_power,
     type="l",
     xlab="",
     ylab="Global Active Power (kilowatts)"
)


#close device
dev.off()

#end plot2
