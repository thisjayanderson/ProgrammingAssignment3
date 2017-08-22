## Anderson
# rankhospital.R
# Ranking hospitals by outcome in a state

rankhospital <- function(state, outcome, num = "best") {
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
            
      ## Return hospital name in that state with the given rank
      ## HEART ATTACK
      if(outcome == "heart attack"){
            # Select only two necessary columns
            extColumns <- subset(stateOutcomeData, select=c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"))
            
            # Coerce rate to be numeric for sorting
            suppressWarnings(extColumns[,2] <- as.numeric(extColumns[,2]))
            
            # Remove NA's
            extColumns <- subset(extColumns, !is.na(extColumns[,2]))
            
            # Sort
            sortedColumns <- extColumns[order(extColumns[,2],extColumns[,1]),]
            
            ## Validate num (desired rank) argument
            if(num=="best"){
                  rank <- 1
            }else if(num=="worst"){
                  rank <- nrow(sortedColumns)
            }else if(num > nrow(sortedColumns)){
                  stop("invalid num (desired rank)")
            }else{rank <- as.numeric(num)}
            
            # Return
            return(sortedColumns[rank,1])
      }
      
      ## HEART FAILURE
      if(outcome == "heart failure"){
            # Select only two necessary columns
            extColumns <- subset(stateOutcomeData, select=c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"))
            
            # Coerce rate to be numeric for sorting
            suppressWarnings(extColumns[,2] <- as.numeric(extColumns[,2]))
            
            # Remove NA's
            extColumns <- subset(extColumns, !is.na(extColumns[,2]))
            
            # Sort
            sortedColumns <- extColumns[order(extColumns[,2],extColumns[,1]),]
            
            ## Validate num (desired rank) argument
            if(num=="best"){
                  rank <- 1
            }else if(num=="worst"){
                  rank <- nrow(sortedColumns)
            }else if(num > nrow(sortedColumns)){
                  stop("invalid num (desired rank)")
            }else{rank <- as.numeric(num)}
            
            # Return
            return(sortedColumns[rank,1])
      }      
      
      ## PNEUMONIA
      if(outcome == "pneumonia"){
            # Select only two necessary columns
            extColumns <- subset(stateOutcomeData, select=c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"))
            
            # Coerce rate to be numeric for sorting
            suppressWarnings(extColumns[,2] <- as.numeric(extColumns[,2]))
            
            # Remove NA's
            extColumns <- subset(extColumns, !is.na(extColumns[,2]))
            
            # Sort
            sortedColumns <- extColumns[order(extColumns[,2],extColumns[,1]),]
            
            ## Validate num (desired rank) argument
            if(num=="best"){
                  rank <- 1
            }else if(num=="worst"){
                  rank <- nrow(sortedColumns)
            }else if(num > nrow(sortedColumns)){
                  stop("invalid num (desired rank)")
            }else{rank <- as.numeric(num)}
            
            # Return
            return(sortedColumns[rank,1])
      }
      
      

}