# This function does the following
# 1. Reads household_power_consumption.txt residing in data folder
# 2. Filters out all records that don't fall on Feb 1st and 2nd 2007
# 3. Adds a new column to the data frame by combining and converting 
#     Date and Time columns to POSIXct 
# 4. Removes the original Date and Time columns (This is done just to reduce memory)
# 5. Plots a scatterplot of Recording date vs. Sub metering values to plot3.png file
# 6. Returns the data frame that contains the data used to plot the graph

plot3 <- function()
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
    png(file="plot3.png")
    
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
    
    # Set legend
    legend("topright", pch=NA, col=c("black","red","blue"), 
           legend=c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
           lty=1,lwd=1)
    
    # Close the graphic device to save the file
    dev.off()
    
    # Return the data used to plot the graph
    powerData
}