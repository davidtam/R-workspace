corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0

        ## Return a numeric vector of correlations

        countData <- complete(directory)

	countDataFilteredThres <- countData[countData["nobs"]>threshold, TRUE, drop=FALSE]

	filteredIDs <- countDataFilteredThres$ids

	returnVal <- numeric()
	for (currentDataFile in filteredIDs){
        
		paddedCurrentDataFile <- formatC(currentDataFile, width = 3, format = "d", flag = "0") 
		
		fileName <- paste(paste(directory , paddedCurrentDataFile , sep = "/"), ".csv", sep = "")

		currentData <- read.csv(fileName, header = TRUE)
		
		filteredN <- currentData[!is.na(currentData["nitrate"]), TRUE, drop=FALSE]
		filteredS <- filteredN[!is.na(filteredN["sulfate"]), TRUE, drop=FALSE]
		returnVal <- c(returnVal,cor(filteredS["nitrate"], filteredS["sulfate"]))

		
	}
	returnVal

}