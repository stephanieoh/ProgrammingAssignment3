rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  outcomelist <- c("heart attack", "heart failure", "pneumonia")
  
  ## Check that state and outcome are valid
  if(!outcome %in% outcomelist) stop("invalid outcome")
  
  ##Set outcome input to outcome column name
  if(outcome == "heart attack") outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  if(outcome == "heart failure") outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  if(outcome == "pneumonia") outcome <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  
  data[, outcome] <- as.numeric(data[, outcome])
  
  df <- c()
  
  ## For each state, find the hospital of the given rank
  states <- sort(unique(data[,7]))
  
  for(i in states){
    statehospital <- data[data$"State" == i, ][!is.na(data[data$"State" == i, ][, outcome]), ]
    listHospital <- statehospital[order(statehospital[, outcome], statehospital[, "Hospital.Name"]), "Hospital.Name"]
    len <- length(listHospital)
    
    if(num == "best") num = 1
    if(num == "worst"){
      df <- rbind(df, c(i, tail(listHospital, 1)))
      next()
    }
    if(num > len){
      df <- rbind(df, c(i, NA))
      next()
    }
    
    df <- rbind(df, c(i, listHospital[num]))
  }
  
  df <- data.frame("hospital" = df[, 2], "state" = df[, 1])
  rownames(df) <- states
  return(df)
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  
  
}