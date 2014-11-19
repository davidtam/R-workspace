rankhospital <- function(state, outcome, num = "best") {
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
	## Return hospital name in that state with the given rank
	## 30-day death rate
	
	outcomeFilteredState <- outcomeRaw[outcomeRaw$State == state, ]
	outcomeFilteredState[, outcomeCol] <- as.numeric(outcomeFilteredState[, outcomeCol])
	outcomeFilteredNA <- outcomeFilteredState[complete.cases(outcomeFilteredState[,outcomeCol]), ]

	outcomeFilteredNA <- outcomeFilteredNA[order(outcomeFilteredNA$Hospital.Name), ]
	
	
	outcomeFilteredNA$rank <- ave(outcomeFilteredNA[, outcomeCol], FUN=function(x) rank(x, ties.method = "first"))

	maxRank <- max(outcomeFilteredNA$rank, na.rm=TRUE)
	minRank <- min(outcomeFilteredNA$rank, na.rm=TRUE)
	actualRank <- 1
	if (identical(num,"best")){
		actualRank <- minRank
	} else if (identical(num, "worst")){
		actualRank <- maxRank
	} else {
		actualRank <- num
	}
	if (!identical(num,"best") && !identical(num,"worst") && !is.numeric(num)) {
		NA
	}else if (actualRank > maxRank || actualRank < minRank){
		NA                                    
	} else {
		outcomeFilteredNA[outcomeFilteredNA$rank == actualRank, "Hospital.Name"]
	}
	
}


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
      sep="", collapse=" ")
}