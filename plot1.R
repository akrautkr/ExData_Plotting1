## Program : plot1.R
##
## Course Project 1 : Exploratory Data Analysis from John Hopkins
##
## Written : A. Krautkramer, 10/8/2016
##
## Purpose : Obtain the electric power consumption dataset and plot histogram of 
##           Frequency verse Global Active Power.  Create a PNG from the histogram
##           named plot1.png.

library(dplyr)

## Function : downloadSource
## 
## Description : Download the electric power consumption data.  
##               Returns a character vector containing the names of the unzipped files.
downloadSource <- function (sourceURL, destfile) {
        
        ## Create Data Directory
        if (!file.exists("./data")){dir.create("./data")}
        
        ## Download data into the Data Directory
        download.file(sourceURL, destfile = destfile)
        
        ## Obtain the date the data was downloaded on and write to a file
        dateDownloaded <- date()
        
        ## Write the date of the file download to a file
        fileConn <- file("./data/downloadData.txt")
        writeLines(dateDownloaded, fileConn)
        close(fileConn)
        
        ## Unzip zip file
        files <-unzip(zipfile = destfile, exdir="data")
        
        return(files)
        
}


#### Function : importData
##
## Description : Read the downloaded data into a data frame
##               Combine the date and time columns and change the class
##               from character to POSIXlt.
##               Add the column names
importData <- function (filename) {
        
        na.string <- "?"
        sep <- ";"
        header <- TRUE
        stopRead <- 94654
        
        powerData <- read.table (filename, na.string=na.string, sep=sep, 
                                 header=header, nrows=stopRead)
        
        return(powerData)
        
}

#### Fuction : formatData
##
## Description : Subsets the data for the specific days this assignment is interested in.
##               Concatenate the date and time columns and convert the result from a
##               character to a POSIXlt object.
##
formatData <- function(data) {
  
        #--- Subset data to include only the days for this assignment:
        #--- 2007-02-01 and 2007-02-02
        
        data <- subset(data, (Date == "1/2/2007" | Date == "2/2/2007"))
        
        #--- Add a new column containing the concatentated date and time
        data <- mutate(data, datetime = paste(Date, Time))
        
        #--- Convert the Date and Time columns to the POSIXlt class
        data$datetime <- strptime(data$datetime, "%d/%m/%Y %H:%M:%S")
        
        return(data)
        
}

#### Fuction : plotData
##
## Description : Plots the data and creates a png of a the results
##
plotData <- function(data) {
        
        png(filename="plot1.png")
        plot1 <- hist(data$Global_active_power, col="Red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
        dev.off()   
}

#######
## START : Hard coded Data variables
#######

##--- source data
sourceURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

##--local workstation destination
destfile <- "./data/household_power_consumption.zip"

##--- unziped filename
unzipfile <- "./data/household_power_consumption.txt"


#######
## END : Hard coded Data variables
#######

### Main program function calls

##-- download data and unzip the file into destfile
files <- downloadSource(sourceURL, destfile)

##-- read data from file into a data structure
powerData <- importData(unzipfile)

##-- reformat the data before plotting
powerData <- formatData(powerData)

##-- Plot Data
plotData(powerData)



