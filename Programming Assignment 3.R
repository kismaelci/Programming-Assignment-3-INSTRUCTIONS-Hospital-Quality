## 1 Plot the 30-day mortality rates for heart attack

## read the csv data
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
## look at the first few rows
head(outcome)
## look at some more informations about dataset
ncol(outcome)
nrow(outcome)
dim(outcome)
class(outcome)
summary(outcome)
names(outcome)
## let s set "30-day death rates from heart attack" (column 11 in the outcome dataset) as numeric
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11], col = 2, main = "Hospital 30-Day Death (mortality) rates for heart attack", xlab = "Deaths")

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

## 4 Ranking hospitals in all states

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings="Not Available")
  
  ## Check that state and outcome are valid
  outcomevalid = c("heart attack","heart failure","pneumonia")
  if (!outcome %in% outcomevalid) { stop("invalid outcome")}
  
  statevalid = sort(unique(data[,7]))
  
  fullColName <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  colName <- fullColName[match(outcome,outcomevalid)]
  
  ## For each state, find the hospital of the given rank
  hospital<-character(0)
  for (i in seq_along(statevalid)) {
    ## Return hospital name in that state with the given rank 30-day death rate
    data.state <- data[data$State==statevalid[i],]
    classed.state <- data.state[order(as.numeric(data.state[[colName]]),data.state[["Hospital.Name"]],decreasing=FALSE,na.last=NA), ]
    this.num = num
    if (this.num=="best") this.num = 1
    if (this.num=='worst') this.num = nrow(classed.state)
    hospital[i] <- classed.state[this.num,"Hospital.Name"]
  }