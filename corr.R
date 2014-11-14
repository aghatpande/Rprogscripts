corr <- function(directory, threshold=0){
        ## 'directory' is a character vector of length 1 indicating the location of the csv files
        ## "threshold" is an integer vector indicating the number of complete observations (on all variables) required to compute the correlation between nitrate and sulfate; the deflault is 0.
        ## Return a numeric vector of correlations
        files<-list.files(directory,full.names=TRUE)
        dat=data.frame()
        x<-NULL
        for (i in 1:length(files)){
                y=read.csv(files[i])
                comp<-y[complete.cases(y), ]
                if(length(comp$ID) >= threshold){
                        dat<-rbind(dat,comp)
                }
                else{}
        }
        dat
        if (length(dat$ID)>0) {
                index<-c(unique(dat$ID))
                for (j in 1:length(index)){
                        x<-c(x,cor(c(dat$sulfate[dat$ID==index[j]]),c(dat$nitrate[dat$ID==index[j]])))
                }
        }
        else{}
        x
}
