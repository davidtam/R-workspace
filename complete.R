complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete cases
        
        
        emptydf <- data.frame(col1="id", col2="nobs")
        
        ids <- numeric()
        nobs <- numeric()
        
        for (currentDataFile in id){
        
		paddedCurrentDataFile <- formatC(currentDataFile, width = 3, format = "d", flag = "0") 
		
		currentData <- read.csv(paste(paste(directory , paddedCurrentDataFile , sep = "/"), ".csv", sep = ""))
		
		filteredN <- currentData[!is.na(currentData["nitrate"]), TRUE, drop=FALSE]
		filteredS <- filteredN[!is.na(filteredN["sulfate"]), TRUE, drop=FALSE]
		
		ids <- c(ids,currentDataFile)
		nobs <- c(nobs,nrow(filteredS))

	}
	data.frame(ids, nobs, stringsAsFactors=FALSE)
	
}