data_ori <- read.table("household_power_consumption.txt",sep = ";", stringsAsFactors=FALSE, col.names = c("Date","Time","Global_active_power","Global_reactive_power","Voltage","Global_intensity","Sub_metering_1","Sub_metering_2","Sub_metering_3" ) )



data_ori$Date <- as.Date(data_ori$Date ,format =  "%d/%m/%Y")


data <- subset(data_ori, Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))
x <- paste(as.character(data$Date), data$Time)

y <- strptime(x, "%Y-%m-%d %H:%M:%S")
data$time <- y

#plot 4

par(mfcol = c(2, 2))
with(data, {
  plot(time, Global_active_power, type= "l",  ylab='Global_active_power' , xlab = '')
  
  
  plot(time, Sub_metering_1, type ="n", ylim = c(0,40), ylab='Energy sub metering', xlab = '' )
  
  #   plot(time, Sub_metering_2, type ="n",ylim = c(0,40), ylab='Energy sub metering' )
  #  plot(time, Sub_metering_3, type ="n",ylim = c(0,40), ylab='Energy sub metering' )}
  
  
  
  lines(data$time, data$Sub_metering_1, type="l",col = "black")
  lines(data$time, data$Sub_metering_2, type="l",col = "red")
  lines(data$time, data$Sub_metering_3, type="l",col = "blue")
  legend("topright", lty = 1,  col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  
  
  plot(time, Voltage, type="l", ylab='Voltage', xlab = 'datetime' )
  
  
  plot(time, Global_reactive_power,type="l",  ylab='Global_reactive_power', xlab = 'datetime' )
  
  
})

dev.copy(png, file = "plot4.png") 
dev.off() 
