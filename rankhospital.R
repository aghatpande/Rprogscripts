rankhospital<-function (state,outcome,num="best"){
        options(warn=-1)
        conditions<-c("heart attack","heart failure","pneumonia")
        data<-read.csv("outcome-of-care-measures.csv",colClasses="character")
        
        if (nrow(data[data$State==state, ])==0) {
                stop("invalid state")
        }
        
        if (sum(outcome==conditions)==0) {
                stop("invalid outcome")
        }
        
        statedata<-data[data$State==state, ]
        result=NULL
        
        if (outcome==conditions[1]) {
                if (num=="best") {
                        statedata[,11]<-as.numeric(statedata[,11])
                        statedata<-statedata[!is.na(statedata[,11]), ]
                        statedata<-statedata[order(statedata[,11],statedata[,2]), ]
                        result<-rbind(result,statedata[,2][1])
                        return(as.character(result))
                }
                
                else if (num=="worst") {
                        statedata[,11]<-as.numeric(statedata[,11])
                        statedata<-statedata[!is.na(statedata[,11]), ]
                        statedata<-statedata[order(statedata[,11],statedata[,2]), ]
                        result<-rbind(result,statedata[,2][length(statedata[,2])])
                        return(as.character(result))
                }
                
                else {
                        statedata[,11]<-as.numeric(statedata[,11])
                        statedata<-statedata[!is.na(statedata[,11]), ]
                        if (num<=length(statedata[,11])){
                                statedata<-statedata[order(statedata[,11],statedata[,2]), ]
                                result<-rbind(result,statedata[,2][num])
                        }
                        else{
                                result<-rbind(result,NA)
                        }
                        return(as.character(result))        
                }
        }
        
        if (outcome==conditions[2]) {
                if (num=="best") {
                        statedata[,17]<-as.numeric(statedata[,17])
                        statedata<-statedata[!is.na(statedata[,17]), ]
                        statedata<-statedata[order(statedata[,17],statedata[,2]), ]
                        result<-rbind(result,statedata[,2][1])
                        return(as.character(result))
                }
                
                else if (num=="worst") {
                        statedata[,17]<-as.numeric(statedata[,17])
                        statedata<-statedata[!is.na(statedata[,17]), ]
                        statedata<-statedata[order(statedata[,17],statedata[,2]), ]
                        result<-rbind(statedata[,2][length(statedata[,2])])
                        return(as.character(result))
                }
                
                else {
                        statedata[,17]<-as.numeric(statedata[,17])
                        statedata<-statedata[!is.na(statedata[,17]), ]
                        if (num<=length(statedata[,17])){
                                statedata<-statedata[order(statedata[,17],statedata[,2]), ]
                                result<-rbind(result,statedata[,2][num])
                                return(as.character(result))
                        }
                        else{
                                result<-rbind(result,NA)
                                return(as.character(result))
                        }
                        
                }
        }
        
        if (outcome==conditions[3]) {
                if (num=="best") {
                        statedata[,23]<-as.numeric(statedata[,23])
                        statedata<-statedata[!is.na(statedata[,23]), ]
                        statedata<-statedata[order(statedata[,23],statedata[,2]), ]
                        statedata[,2][1]
                }
                
                else if (num=="worst") {
                        statedata[,23]<-as.numeric(statedata[,23])
                        statedata<-statedata[!is.na(statedata[,23]), ]
                        statedata<-statedata[order(statedata[,23],statedata[,2]), ]
                        statedata[,2][length(statedata[,2])]
                }
                
                else {
                        statedata[,23]<-as.numeric(statedata[,23])
                        statedata<-statedata[!is.na(statedata[,23]), ]
                        if (num<=length(statedata[,23])){
                                statedata<-statedata[order(statedata[,23],statedata[,2]), ]
                                statedata[,2][num]
                        }
                        else{
                                NA
                        }
                        
                }
        }
        
}