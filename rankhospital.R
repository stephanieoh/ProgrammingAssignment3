rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
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
  ##data for hospital by state
  statehospital <- data[data$"State" == state, ]
  statehospital <- statehospital[!is.na(statehospital[, outcome]), ]
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  listHospital <- statehospital[order(statehospital[, outcome], statehospital$"Hospital.Name"), "Hospital.Name"]
  len <- length(listHospital)
  
  
  if(num == "best") num = 1
  if(num == "worst") num = len
  if(num > len) return(NA)
  
  return(listHospital[num])
 
}