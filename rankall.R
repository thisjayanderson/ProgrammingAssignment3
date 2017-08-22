## Anderson
# rankall.R
# Ranking hospitals in all states



rankall <- function(outcome, num = "best") {
      ## Read outcome data
      outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      
      ## Check that state and outcome are valid
      validOutcomes <- c("heart attack", "heart failure", "pneumonia")
      
      if(!(outcome %in% validOutcomes)){
            stop("invalid outcome")
      }
      
      # Extract states and sort
      validStates <- sort(unique(outcomeData$State))
      
      ## For each state, find the hospital of the given rank

      ## HEART ATTACK
      if(outcome == "heart attack"){
            # Select only three necessary columns (Hospital Name, Rate, State)
            extColumns <- subset(outcomeData, select=c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack","State"))
            
            # Coerce rate to be numeric for sorting
            suppressWarnings(extColumns[,2] <- as.numeric(extColumns[,2]))
            
            # Remove NA's
            extColumns <- subset(extColumns, !is.na(extColumns[,2]))
            
            # Sort by State, Rate, then Hospital Name
            sortedColumns <- extColumns[order(extColumns[,3],extColumns[,2],extColumns[,1]),]
            
            # Establish return dataframe
            dfOutput <- data.frame(stringsAsFactors = FALSE)
                        
            # For each state
            for(stateNum in 1:length(validStates)){
            
                  ## Subset out rows for current State
                  dfCurrentState <- subset(sortedColumns,sortedColumns[,3]==validStates[stateNum])  

                  ## Validate num (desired rank) argument, store as rank
                  if(num=="best"){
                        rank <- 1
                  }else if(num=="worst"){
                        rank <- nrow(dfCurrentState)
                  }else if(num > nrow(dfCurrentState)){
                        rank <- NA # invalid rank, will return NA
                  }else{rank <- as.numeric(num)}
           
                  ## Check for invalid rank and assign hospital name
                  if(is.na(rank)){
                        currentHospital <- "NA"
                  }else{
                        currentHospital <- dfCurrentState[rank,1]
                  }

                  # Add selected Hospital and State to dfOutput
                  dfAppend <- data.frame(currentHospital,validStates[stateNum])
                  dfOutput <- rbind(dfOutput,dfAppend)
            } # end of for loop      
                  
            # Return
            colnames(dfOutput) <- c("hospital","state")
            return(dfOutput)
      }

      ## HEART FAILURE
      if(outcome == "heart failure"){
            # Select only three necessary columns (Hospital Name, Rate, State)
            extColumns <- subset(outcomeData, select=c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure","State"))
            
            # Coerce rate to be numeric for sorting
            suppressWarnings(extColumns[,2] <- as.numeric(extColumns[,2]))
            
            # Remove NA's
            extColumns <- subset(extColumns, !is.na(extColumns[,2]))
            
            # Sort by State, Rate, then Hospital Name
            sortedColumns <- extColumns[order(extColumns[,3],extColumns[,2],extColumns[,1]),]
            
            # Establish return dataframe
            dfOutput <- data.frame(stringsAsFactors = FALSE)
            
            # For each state
            for(stateNum in 1:length(validStates)){
                  
                  ## Subset out rows for current State
                  dfCurrentState <- subset(sortedColumns,sortedColumns[,3]==validStates[stateNum])  
                  
                  ## Validate num (desired rank) argument, store as rank
                  if(num=="best"){
                        rank <- 1
                  }else if(num=="worst"){
                        rank <- nrow(dfCurrentState)
                  }else if(num > nrow(dfCurrentState)){
                        rank <- NA # invalid rank, will return NA
                  }else{rank <- as.numeric(num)}
                  
                  ## Check for invalid rank and assign hospital name
                  if(is.na(rank)){
                        currentHospital <- "NA"
                  }else{
                        currentHospital <- dfCurrentState[rank,1]
                  }
                  
                  # Add selected Hospital and State to dfOutput
                  dfAppend <- data.frame(currentHospital,validStates[stateNum])
                  dfOutput <- rbind(dfOutput,dfAppend)
            } # end of for loop      
            
            # Return
            colnames(dfOutput) <- c("hospital","state")
            return(dfOutput)
      }
      
      
      ## Pneumonia
      if(outcome == "pneumonia"){
            # Select only three necessary columns (Hospital Name, Rate, State)
            extColumns <- subset(outcomeData, select=c("Hospital.Name","Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia","State"))
            
            # Coerce rate to be numeric for sorting
            suppressWarnings(extColumns[,2] <- as.numeric(extColumns[,2]))
            
            # Remove NA's
            extColumns <- subset(extColumns, !is.na(extColumns[,2]))
            
            # Sort by State, Rate, then Hospital Name
            sortedColumns <- extColumns[order(extColumns[,3],extColumns[,2],extColumns[,1]),]
            
            # Establish return dataframe
            dfOutput <- data.frame(stringsAsFactors = FALSE)
            
            # For each state
            for(stateNum in 1:length(validStates)){
                  
                  ## Subset out rows for current State
                  dfCurrentState <- subset(sortedColumns,sortedColumns[,3]==validStates[stateNum])  
                  
                  ## Validate num (desired rank) argument, store as rank
                  if(num=="best"){
                        rank <- 1
                  }else if(num=="worst"){
                        rank <- nrow(dfCurrentState)
                  }else if(num > nrow(dfCurrentState)){
                        rank <- NA # invalid rank, will return NA
                  }else{rank <- as.numeric(num)}
                  
                  ## Check for invalid rank and assign hospital name
                  if(is.na(rank)){
                        currentHospital <- "NA"
                  }else{
                        currentHospital <- dfCurrentState[rank,1]
                  }
                  
                  # Add selected Hospital and State to dfOutput
                  dfAppend <- data.frame(currentHospital,validStates[stateNum])
                  dfOutput <- rbind(dfOutput,dfAppend)
            } # end of for loop      
            
            # Return
            colnames(dfOutput) <- c("hospital","state")
            return(dfOutput)
      }
      
}