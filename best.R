best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomelist <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(!state %in% data[, 7]) stop("invalid state")
  if(!outcome %in% outcomelist) stop("invalid outcome")
  
  ##Set outcome input to outcome column name
  if(outcome == "heart attack") outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  if(outcome == "heart failure") outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  if(outcome == "pneumonia") outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  
  data[, outcome] <- as.numeric(data[, outcome])
  
  ## Return hospital name in that state with lowest 30-day death
  statehospital <- data[data$"State" == state, ]
  bad <- is.na(statehospital[, outcome])
  listmin <- min(statehospital[, outcome][!bad])
  x<- statehospital[statehospital[, outcome] == listmin, "Hospital.Name"]
  hospitalList <- x[!is.na(x)]
  
  ## rate
  return(min(hospitalList))
}