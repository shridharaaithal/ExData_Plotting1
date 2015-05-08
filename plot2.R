# This function does the following
# 1. Reads household_power_consumption.txt residing in data folder
# 2. Filters out all records that don't fall on Feb 1st and 2nd 2007
# 3. Adds a new column to the data frame by combining and converting 
#     Date and Time columns to POSIXct 
# 4. Removes the original Date and Time columns (This is done just to reduce memory)
# 5. Plots a scatterplot of Recording date vs. Global_Active_Power to plot2.png file
# 6. Returns the data frame that contains the data used to plot the graph

plot2 <- function()
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
    png(file="plot2.png")
    
    # Plot graph
    plot(powerData$recordDate, powerData$Global_active_power, xlab="",ylab="Global Active Power (Kilowatts)", main="", type="l")
    
    # Close the graphic device to save the file
    dev.off()
    
    # Return the data used to plot the graph
    powerData
}