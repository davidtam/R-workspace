pollutantmean <- function(directory, pollutant, id = 1:332) {



	
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        
        meanVector <- numeric()

        for (currentDataFile in id){
        
		paddedCurrentDataFile <- formatC(currentDataFile, width = 3, format = "d", flag = "0") 
		
		currentData <- read.csv(paste(paste(directory , paddedCurrentDataFile , sep = "/"), ".csv", sep = ""))
		
		currentData
		
		filteredNA <- currentData[!is.na(currentData[pollutant]), TRUE, drop=FALSE]
		
		thisMean <- mean(unlist(filteredNA[pollutant], use.names = FALSE))
		meanVector <- c(meanVector, thisMean)
	}
	meanVector
}