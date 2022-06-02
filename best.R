## 2 Finding the best hospital in a state

best <- function(state, outcome) {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")
  
  ## Check that state and outcome are valid
  outcomevalid = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% outcomevalid) { stop("invalid outcome")}
  
  statevalid = unique(data[,7])
  if (!state %in% statevalid) stop("invalid state")
  
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,outcomevalid)]
  
  ## Return hospital name in that state with lowest 30-day death rate
  data.state <- data[data$State==state,]
  hospname <- which.min(as.double(data.state[,colName]))
  data.state[hospname,"Hospital.Name"]
}
