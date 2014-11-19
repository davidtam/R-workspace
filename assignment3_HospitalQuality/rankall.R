rankall <- function(outcome, num = "best") {
	## Read outcome data
	outcomeRaw <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

	## Check that state and outcome are valid
	## For each state, find the hospital of the given rank
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
	
	uniqueStates <- sort(unique(outcomeRaw$State))
	result <- data.frame(state=uniqueStates)
	row.names(result) <- result$state
	
	for(aState in result$state){
		result[aState, "hospital"] <- rankhospital(aState, outcome, num)
	}
	result[, c(2,1)]

}