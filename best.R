## best.R
#R Programming Assignment 3
#Phillip.Escandon@gmail.com
# best(state,outcome)
## state - two char abbreviated name of a state
## outcome - 
## Reads outcome-of-care-measures.csv and returns char vector
## with the name os the best (lowest) 30 day mortality for the specified outcome
## 
library(dplyr)
options(warn = -1)

best <- function(state,outcome){
        ## Read outcome data
        
        outcomeDF <- read.csv("outcome-of-care-measures.csv",colClasses = "character")
        #outcomeDF <- read.csv("outcome-of-care-measures.csv")
        #outcome[,11] <- as.numeric(outcome[,11])
        #hist(outcome[,11])
        
        ## Check that state and outcome are valid
        
        # Divide up data by state: use outcome_state[state] or outcome_state$AK
        #outcome_state <- split(outcome, outcome$State)
        
        # or use the dyplr package - 
        outcomeByState <- filter(outcomeDF, State == state)
        if(nrow(outcomeByState) == 0){
                stop("invalid state")
        }
        
        
        # Create my subset of only the data we are interested in - 
        # should be names here, not numbers
        mySubset <- select(outcomeByState,2,11,17,23)

        # rename to make it readable
        mySubset <- rename(mySubset, Hospital = Hospital.Name)
        mySubset <- rename(mySubset, HeartAttack = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
        mySubset <- rename(mySubset, HeartFailure = Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
        mySubset <- rename(mySubset, Pneumonia = Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
      
        if(outcome == "heart attack"){
                mySubset$HeartAttack <- as.numeric(mySubset$HeartAttack)
                badNA <- is.na(mySubset$HeartAttack)
                bestHospital <- mySubset[which.min(mySubset$HeartAttack),1]
                
        }else if(outcome == "heart failure"){
                mySubset$HeartFailure <- as.numeric(mySubset$HeartFailure)
                badNA <- is.na(mySubset$HeartFailure)
                bestHospital <- mySubset[which.min(mySubset$HeartFailure),1]
                
        }else if(outcome == "pneumonia"){
                mySubset$Pneumonia <- as.numeric(mySubset$Pneumonia)
                badNA <- is.na(mySubset$Pneumonia)
                bestHospital <- mySubset[which.min(mySubset$Pneumonia),1]
                
        }else{stop("invalid outcome")}
        
        
        bestHospital
   
        
        
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate

}