# This function does the following
# 1. Reads household_power_consumption.txt residing in data folder
# 2. Filters out all records that don't fall on Feb 1st and 2nd 2007
# 3. Adds a new column to the data frame by combining and converting 
#     Date and Time columns to POSIXct 
# 4. Removes the original Date and Time columns (This is done just to reduce memory)
# 5. Plots 4 graphs to plot4.png with 2 rows and 2 columns
#    a. Graph plotted in plot2.R
#    b. Graph plotted in plot3.R
#    c. Scatter plot of record date vs. voltage
#    d. Scatter plot of record date vs. global reactive power
# 6. Returns the data frame that contains the data used to plot the graph

plot4 <- function()
{
    # Read the entire file. Treat ? as NA, and do not convert strings to factors
    powerData <- read.table(".\\data\\household_power_consumption.txt", 
                            sep=";", header=T, na.strings="?", stringsAsFactors=F)
    
    # Subset the data read to select only the readings from the dates of interest
    powerData <- powerData[powerData$Date == "1/2/2007" | powerData$Date == "2/2/2007",]
    
    # Add a new column in POSIXct format that contains the Date and Time
    powerData <- cbind(powerData, 
                       recordDate = as.POSIXct(strptime(paste(powerData$Date, powerData$Time, sep = " "), "%d/%m/%Y %H:%M:%S")))
    
    # Remove the original Date and Time character columns. (Just to save memory)
    powerData <- powerData[, 3:10]
    
    # Open a png graphic device
    png(file="plot4.png")
    
    # Set plotting to 2-row, 2-column, fill column first.
    par(mfcol=c(2,2))
    
    #Plot the first graph (this is same as plot2)
    plot(powerData$recordDate, powerData$Global_active_power, xlab="",ylab="Global Active Power (Kilowatts)", main="", type="l")
    
    
    #Plot the second graph (this is same as plot3)
    # Compute the minumum and maximum values of the y-axis
    ymin <- with(powerData, min(Sub_metering_1, Sub_metering_2, Sub_metering_3))
    ymax <- with(powerData, max(Sub_metering_1, Sub_metering_2, Sub_metering_3))
    
    # Plot recordDate vs Sub_metering_1.
    plot(powerData$recordDate, powerData$Sub_metering_1, xlab="",
         ylab="Energy sub metering", main="", type="l",
         ylim=c(ymin, ymax))
    
    # Add line for Sub_metering_2
    lines(powerData$recordDate, powerData$Sub_metering_2, col="red")
    
    # Add line for Sub_metering_3
    lines(powerData$recordDate, powerData$Sub_metering_3, col="blue")
    
    # Set legend without a border
    legend("topright", pch=NA, col=c("black","red","blue"), 
           legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
           lty=1,lwd=1,bty="n")
    
    # Plot the third graph
    plot(powerData$recordDate, powerData$Voltage, xlab="datetime",ylab="Voltage", main="", type="l")
    
    # Plot the fourth graph
    plot(powerData$recordDate, powerData$Global_reactive_power, xlab="datetime", ylab="Global_reactive_power", main="", type="l")

    # Close the graphic device to save the file
    dev.off()
    
    # Return the data used to plot the graph
    powerData
}