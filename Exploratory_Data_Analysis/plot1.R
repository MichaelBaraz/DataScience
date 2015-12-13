# Program name:   plot1.R
# Author:         Michael Baraz
# Purpose:        Create histogram plot per assignment


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
png("plot1.png", height=450, width=450)

#plot the histogram graph
hist(power$Global_active_power,
     main="Global Active Power",
     xlab="Global Active Power (kilowatts)",
     ylab="Frequency",
     col="red"
  )


#close device
dev.off()
  
#end plot1
