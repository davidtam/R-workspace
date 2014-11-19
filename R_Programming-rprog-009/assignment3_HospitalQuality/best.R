best <- function(state, outcome) {
	## Read outcome data
	outcomeRaw <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
	
	## Check that state and outcome are valid
	if (!is.element(state, outcomeRaw$State)){
		stop("invalid state")
	}
	
	outcomeCol <- paste ("Hospital.30.Day.Death..Mortality..Rates.from.",
		gsub(" ", ".", simpleCap(outcome)), sep = "", collapse = NULL)
	if (!is.element(outcomeCol, names(outcomeRaw))){
		stop("invalid outcome")
	}
	
	## Return hospital name in that state with lowest 30-day death
	## rate
	outcomeFilteredState <- outcomeRaw[outcomeRaw$State == state, ]
	outcomeFilteredNA <- outcomeFilteredState[complete.cases(outcomeFilteredState[,outcomeCol]), ]
	outcomeFilteredNA[, outcomeCol] <- as.numeric(outcomeFilteredNA[, outcomeCol])
	
	
	outcomeFilteredNA[which.min(outcomeFilteredNA[, outcomeCol]),"Hospital.Name"]
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}