## 1 Plot the 30-day mortality rates for heart attack

## read the csv data
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
## look at the first few rows
head(outcome)
## look at some more informations about dataset
ncol(outcome)
nrow(outcome)
names(outcome)
## let s set "30-day death rates from heart attack" (column 11 in the outcome dataset) as numeric
outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11], col = 2, main = "Hospital 30-Day Death (mortality) rates for heart attack", xlab = "Deaths")
