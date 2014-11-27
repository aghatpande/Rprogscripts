## This script written as first part of Programming assignment 3 of the R Programming course I did in November 2014. The script takes as input two 
## variables: the two letter abbreviation of any U.S. state (incl. DC, Puerto Rico, Guam, Virgin Islands) and any one of 3 parameters to rank hospitals
## within that state. The outcomes are 30 day mortality rates in the hospital following admission of patients for a heart attack, heart failure or
## pneumonia. The script initally determines that the user inputs make sense, else it produces helpful error messages. It then subsets the 
## "outcome-of-cares" dataset from Medicare to only include hospitals from within the desired state. It then locates the hospital that has the lowest
## 30 day mortality rate for the desired outcome. If there are more than one hospital with the same minimum mortality rate, it sorts the hospital
##  names alphabetically and then returns only the first name in the list.

best<-function(state,outcome){
        options(warn=-1) ## suppressing the "NAs introduced by coercion" warning due to the 'as.numeric' function
        conditions=c("heart attack","heart failure","pneumonia") # list of possible values of the 'outcome' variable
        data<-read.csv("outcome-of-care-measures.csv",colClasses="character") # reading in the csv file
        
        if (nrow(data[(data$State==state), ]) > 0 # condition rejects a nonsensical 'State' name
           & outcome==conditions[1]) { # when this condition is satisfied, the script searches for the lowest mortality for heart attacks within the given 'State'
        statedata<-data[(data$State==state), ] # subsetting the entire dataset into data for the given 'State'
        statedata[,11]<-as.numeric(statedata[,11]) # converting the mortality data into numeric vector
        statedata<-statedata[!is.na(statedata[,11]), ] # removing the rows that correspond to NA's in the heart attack mortality column
        hosp<-c(statedata$Hospital.Name[which(statedata[,11]==min(statedata[, 11]))]) # creating list of hospitals that have minimum heart attack mortality in the state; could be one or more hospitals 
        hosp<-sort(hosp) # sorting the hospitals alphabetically
        hosp[1] # returning the first name in the list of hospitals in the State with minimum mortality following a heart attack
        }
        
        else if (nrow(data[(data$State==state), ]) > 0  # this loop is identical to the previous one, except that the variable used is mortality after "heart failure" instead of heart attacks
            & outcome==conditions[2]) {
                statedata<-data[(data$State==state), ]
                statedata[,17]<-as.numeric(statedata[,17])
                statedata<-statedata[!is.na(statedata[,17]), ]
                hosp<-c(statedata$Hospital.Name[which(statedata[,17]==min(statedata[, 17]))])
                hosp<-sort(hosp)
                hosp[1]
        }
        
        else if (nrow(data[(data$State==state), ]) > 0 # again this loop is similar to the two previous ones, except the variable used to list the best hospital is mortality after pneumonia
            & outcome==conditions[3]) {
                statedata<-data[(data$State==state), ]
                statedata[,23]<-as.numeric(statedata[,23])
                statedata<-statedata[!is.na(statedata[,23]), ]
                hosp<-c(statedata$Hospital.Name[which(statedata[,23]==min(statedata[, 23]))])
                hosp<-sort(hosp)
                hosp[1]
        }
        
        else if (nrow(data[(data$State==state), ])==0) { # this condition rejects a nonsensical "State" name
                stop("invalid state")
        }
        
        else { # this is by default a nonsensical "outcome" variable; it is the only option remaining after excluding all the conditions in the preceding loops
                stop("invalid outcome")
        }
}