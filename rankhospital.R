## 3 Ranking hospitals by outcome in a state

rankhospital <- function(state, outcome, num = "best") {
  
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")
  
  ## Check that state and outcome are valid
  outcomevalid = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% outcomevalid) { stop("invalid outcome")}
  
  statevalid = unique(data[,7])
  if (!state %in% statevalid) { stop("invalid state")}
  
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,outcomevalid)]
  
  ## Return hospital name in that state with the given rank 30-day death rate
  data.state <- data[data$State==state,]
  classed.state <- data.state[order(as.numeric(data.state[[colName]]),data.state[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
  if (num=="best") num = 1
  if (num=='worst') num = nrow(classed.state)
  classed.state[num,"Hospital.Name"]
}
