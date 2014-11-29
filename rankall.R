rankall<-function(outcome,num="best"){
        options(warn=-1) ## suppressing the "NAs introduced by coercion" warning due to the 'as.numeric' function
        
        conditions=c("heart attack","heart failure","pneumonia") # list of possible values of the 'outcome' variable
        if (!sum(outcome==conditions)==1) {             # checking for a valid user-specified 'outcome' measure
                stop("invalid outcome")                 # error mesg. to user informing about problem with specified outcome
        }
        
        data<-read.csv("outcome-of-care-measures.csv",colClasses="character") # reading in the csv file
        states<-unique(data$State)                      # generating a vector of state names
        result<-data.frame("hospital"=character(), "state"=character())                                    # generating a placeholder result variable
        
        if (outcome==conditions[1]
            & num=="best") { # when this condition is satisfied, the script searches for the lowest mortality for heart attacks within the given 'State'
                for (i in states) {
                        statedata<-data[(data$State==i), ] # subsetting the entire dataset into data for the given 'State'
                        statedata[,11]<-as.numeric(statedata[,11]) # converting the mortality data into numeric vector
                        statedata<-statedata[!is.na(statedata[,11]), ] # removing the rows that correspond to NA's in the heart attack mortality column
                        hosp<-c(statedata$Hospital.Name[which(statedata[,11]==min(statedata[, 11]))]) # creating list of hospitals that have minimum heart attack mortality in the state; could be one or more hospitals 
                        hosp<-sort(hosp) # sorting the hospitals alphabetically
                        hospst<-data.frame("hospital"=hosp[1],"state"=i)
                        result<- rbind(result,hospst) # returning the first name in the list of hospitals in the State with minimum mortality following a heart attack        
                }
                return(result)
        }
        
        else if (outcome==conditions[1]
            & num=="worst") { # when this condition is satisfied, the script searches for the highest mortality for heart attacks within the given 'State'
                for (i in states) {
                        statedata<-data[(data$State==i), ] # subsetting the entire dataset into data for the given 'State'
                        statedata[,11]<-as.numeric(statedata[,11]) # converting the mortality data into numeric vector
                        statedata<-statedata[!is.na(statedata[,11]), ] # removing the rows that correspond to NA's in the heart attack mortality column
                        hosp<-c(statedata$Hospital.Name[which(statedata[,11]==max(statedata[, 11]))]) # creating list of hospitals that have maximum heart attack mortality in the state; could be one or more hospitals 
                        hosp<-sort(hosp) # sorting the hospitals alphabetically
                        hospst<-data.frame("hospital"=hosp[1],"state"=i)
                        result<- rbind(result,hospst)           # returning the first name in the list of hospitals in the State with maximum mortality following a heart attack        
                }
                return(result)
        }
        
        else if (outcome==conditions[1]
            & class(num)=="numeric") {          # when this condition is satisfied, the script searches for the lowest mortality for heart attacks within the given 'State'
                for (i in states) {
                        statedata<-data[(data$State==i), ]              # subsetting the entire dataset into data for the given 'State'
                        statedata[,11]<-as.numeric(statedata[,11])              # converting the mortality data into numeric vector
                        statedata<-statedata[!is.na(statedata[,11]), ]                  # removing the rows that correspond to NA's in the heart attack mortality column
                        statehosp<-statedata$Hospital.Name[order(statedata[,11],statedata[,2])]               # ordering the data according to rank and then arranging hospital names alphabetically
                        hosp<-statehosp[num]                # creating list of hospitals that have the specified heart attack mortality rank
                        hospst<-data.frame("hospital"=hosp[1],"state"=i)                # putting result in specified format 
                        result<- rbind(result,hospst)                   # returning the first name in the list of hospitals in the State with minimum mortality following a heart attack        
                }
                return(result)
        }
        
        else if (outcome==conditions[2]
            & num=="best") { # when this condition is satisfied, the script searches for the lowest mortality for heart failure within the given 'State'
                for (i in states) {
                        statedata<-data[(data$State==i), ] # subsetting the entire dataset into data for the given 'State'
                        statedata[,17]<-as.numeric(statedata[,17]) # converting the mortality data into numeric vector
                        statedata<-statedata[!is.na(statedata[,17]), ] # removing the rows that correspond to NA's in the heart failure mortality column
                        hosp<-c(statedata$Hospital.Name[which(statedata[,17]==min(statedata[, 17]))]) # creating list of hospitals that have minimum heart failure mortality in the state; could be one or more hospitals 
                        hosp<-sort(hosp) # sorting the hospitals alphabetically
                        hospst<-data.frame("hospital"=hosp[1],"state"=i)
                        result<- rbind(result,hospst) # returning the first name in the list of hospitals in the State with minimum mortality following a heart failure        
                }
                return(result)
        }
        
        else if (outcome==conditions[2]
            & num=="worst") { # when this condition is satisfied, the script searches for the highest mortality for heart failure within the given 'State'
                for (i in states) {
                        statedata<-data[(data$State==i), ] # subsetting the entire dataset into data for the given 'State'
                        statedata[,17]<-as.numeric(statedata[,17]) # converting the mortality data into numeric vector
                        statedata<-statedata[!is.na(statedata[,17]), ] # removing the rows that correspond to NA's in the heart failure mortality column
                        hosp<-c(statedata$Hospital.Name[which(statedata[,17]==max(statedata[, 17]))]) # creating list of hospitals that have maximum heart failure mortality in the state; could be one or more hospitals 
                        hosp<-sort(hosp) # sorting the hospitals alphabetically
                        hospst<-data.frame("hospital"=hosp[1],"state"=i)
                        result<- rbind(result,hospst)           # returning the first name in the list of hospitals in the State with maximum mortality following heart failure        
                }
                return(result)
        }
        
        else if (outcome==conditions[2]
            & class(num)=="numeric") {          # when this condition is satisfied, the script searches for the ranked mortality for heart failure within the given 'State'
                for (i in states) {
                        statedata<-data[(data$State==i), ]              # subsetting the entire dataset into data for the given 'State'
                        statedata[,17]<-as.numeric(statedata[,17])              # converting the mortality data into numeric vector
                        statedata<-statedata[!is.na(statedata[,17]), ]                  # removing the rows that correspond to NA's in the heart failure mortality column
                        statehosp<-statedata$Hospital.Name[order(statedata[,17],statedata[,2])]               # ordering the data according to rank and then arranging hospital names alphabetically
                        hosp<-statehosp[num]                # creating list of hospitals that have the specified heart failure mortality rank
                        hospst<-data.frame("hospital"=hosp[1],"state"=i)                # putting result in specified format 
                        result<- rbind(result,hospst)                   # returning the first name in the list of hospitals in the State with given rank in mortality following heart failure        
                }
                return(result)
        }
        
        else if (outcome==conditions[3]
                 & num=="best") { # when this condition is satisfied, the script searches for the lowest mortality for heart failure within the given 'State'
                for (i in states) {
                        statedata<-data[(data$State==i), ] # subsetting the entire dataset into data for the given 'State'
                        statedata[,23]<-as.numeric(statedata[,23]) # converting the mortality data into numeric vector
                        statedata<-statedata[!is.na(statedata[,23]), ] # removing the rows that correspond to NA's in the heart failure mortality column
                        hosp<-c(statedata$Hospital.Name[which(statedata[,23]==min(statedata[, 23]))]) # creating list of hospitals that have minimum heart failure mortality in the state; could be one or more hospitals 
                        hosp<-sort(hosp) # sorting the hospitals alphabetically
                        hospst<-data.frame("hospital"=hosp[1],"state"=i)
                        result<- rbind(result,hospst) # returning the first name in the list of hospitals in the State with minimum mortality following a heart failure        
                }
                return(result)
        }
        
        else if (outcome==conditions[3]
                 & num=="worst") { # when this condition is satisfied, the script searches for the highest mortality for heart failure within the given 'State'
                for (i in states) {
                        statedata<-data[(data$State==i), ] # subsetting the entire dataset into data for the given 'State'
                        statedata[,23]<-as.numeric(statedata[,23]) # converting the mortality data into numeric vector
                        statedata<-statedata[!is.na(statedata[,23]), ] # removing the rows that correspond to NA's in the heart failure mortality column
                        hosp<-c(statedata$Hospital.Name[which(statedata[,23]==max(statedata[, 23]))]) # creating list of hospitals that have maximum heart failure mortality in the state; could be one or more hospitals 
                        hosp<-sort(hosp) # sorting the hospitals alphabetically
                        hospst<-data.frame("hospital"=hosp[1],"state"=i)
                        result<- rbind(result,hospst)           # returning the first name in the list of hospitals in the State with maximum mortality following heart failure        
                }
                return(result)
        }
        
        else if (outcome==conditions[3]
                 & class(num)=="numeric") {          # when this condition is satisfied, the script searches for the ranked mortality for heart failure within the given 'State'
                for (i in states) {
                        statedata<-data[(data$State==i), ]              # subsetting the entire dataset into data for the given 'State'
                        statedata[,23]<-as.numeric(statedata[,23])              # converting the mortality data into numeric vector
                        statedata<-statedata[!is.na(statedata[,23]), ]                  # removing the rows that correspond to NA's in the heart failure mortality column
                        statehosp<-statedata$Hospital.Name[order(statedata[,23],statedata[,2])]               # ordering the data according to rank and then arranging hospital names alphabetically
                        hosp<-statehosp[num]                # creating list of hospitals that have the specified heart failure mortality rank
                        hospst<-data.frame("hospital"=hosp[1],"state"=i)                # putting result in specified format 
                        result<- rbind(result,hospst)                   # returning the first name in the list of hospitals in the State with given rank in mortality following heart failure        
                }
                return(result)
        }
                
        else { stop("invalid rank")}
}