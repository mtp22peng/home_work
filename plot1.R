data_ori <- read.table("household_power_consumption.txt",sep = ";", stringsAsFactors=FALSE, col.names = c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3" ) )



data_ori$Date <- as.Date(data_ori$Date ,format =  "%d/%m/%Y")


data <- subset(data_ori, Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))
x <- paste(as.character(data$Date), data$Time)

y <- strptime(x, "%Y-%m-%d %H:%M:%S")
data$time <- y

#plot1
par(mfcol = c(1, 1))
hist(as.numeric(data$Global_active_power),  col = "red", xlab = 'Global_active_power (kilowatts)', main = "Global Active Power")
dev.copy(png, file = "plot1.png") 
dev.off() 

