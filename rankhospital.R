#rankhospital.R
#R Programming Assignment 3
#Phillip.Escandon@gmail.com


library(dplyr)
options(warn = -1)

rankhospital <- function(state, outcome, num = "best"){
        outcomeDF <- read.csv("outcome-of-care-measures.csv",colClasses = "character")

        
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
                mySubset <- mySubset[!badNA,]
                mySubset2 <- arrange(mySubset,HeartAttack)
                mySubset2 <- mySubset2[order(mySubset2$HeartAttack, mySubset2$Hospital), ]
                if(num == 'best'){
                        hospital <- head(mySubset2$Hospital,1)
                }else if(num == 'worst'){
                        hospital <- tail(mySubset2$Hospital,1)                        
                }else{
                        hospital <- mySubset2[num,1]
                }
                
        }else if(outcome == "heart failure"){
                mySubset$HeartFailure <- as.numeric(mySubset$HeartFailure)
                badNA <- is.na(mySubset$HeartFailure)
                mySubset <- mySubset[!badNA,]
                mySubset2 <- arrange(mySubset,HeartFailure)
                mySubset2 <- mySubset2[order(mySubset2$HeartFailure, mySubset2$Hospital), ]
                if(num == 'best'){
                        hospital <- head(mySubset2$Hospital,1)
                }else if(num == 'worst'){
                        hospital <- tail(mySubset2$Hospital,1)                        
                }else{
                        hospital <- mySubset2[num,1]
                }
        }else if(outcome == "pneumonia"){
                mySubset$Pneumonia <- as.numeric(mySubset$Pneumonia)
                badNA <- is.na(mySubset$Pneumonia)
                mySubset <- mySubset[!badNA,]
                mySubset2 <- arrange(mySubset,Pneumonia)
                mySubset2 <- mySubset2[order(mySubset2$Pneumonia, mySubset2$Hospital), ]
                if(num == 'best'){
                        hospital <- head(mySubset2$Hospital,1)
                }else if(num == 'worst'){
                        hospital <- tail(mySubset2$Hospital,1)                        
                }else{
                        hospital <- mySubset2[num,1]
                }
        }else{stop("invalid outcome")}
        
        hospital
        
}