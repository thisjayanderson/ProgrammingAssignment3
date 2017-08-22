## Anderson
# best.R
# Finding the best hospital in a state


best <- function(state, outcome) {
      ## Read outcome data
      outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      ## Check that state and outcome are valid
      validStates <- unique(outcomeData$State)
      validOutcomes <- c("heart attack", "heart failure", "pneumonia")
      
      if(!(state %in% validStates)){
            stop("invalid state")
      }
      
      if(!(outcome %in% validOutcomes)){
            stop("invalid outcome")
      }
      
      ## Subset only selected state
      stateOutcomeData <- subset(outcomeData,outcomeData$State==state)

      ## Return hospital name in that state with lowest 30-day death
      ## rate
      if(outcome == "heart attack"){
            extColumns <- subset(stateOutcomeData, select=c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))
            suppressWarnings(extColumns[,2] <- as.numeric(extColumns[,2]))
            sortedColumns <- extColumns[order(extColumns[,2],extColumns[,1]),]
            return(sortedColumns[1,1])
      }
      
      if(outcome == "heart failure"){
            extColumns <- subset(stateOutcomeData, select=c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))
            suppressWarnings(extColumns[,2] <- as.numeric(extColumns[,2]))
            sortedColumns <- extColumns[order(extColumns[,2],extColumns[,1]),]
            return(sortedColumns[1,1])
      }
      
      if(outcome == "pneumonia"){
            extColumns <- subset(stateOutcomeData, select=c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
            suppressWarnings(extColumns[,2] <- as.numeric(extColumns[,2]))
            sortedColumns <- extColumns[order(extColumns[,2],extColumns[,1]),]
            return(sortedColumns[1,1])
      }
      
      
      }