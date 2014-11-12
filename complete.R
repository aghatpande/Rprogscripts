complete <- function(directory,id){
  ## 'directory' is a character vector of length 1 indicating the location of the csv files
  ## 'id' is an integer vector indicating the monitor numbers to be used
  ## Return the number of complete cases in files specified in id as a table with 'id' and 'nobs' (number of complete cases in file)
files<-list.files(directory,full.names=TRUE)
dat=data.frame()
tabl=data.frame()
for (i in id){
        dat<-rbind(dat,read.csv(files[i]))
}
for (j in id){
        dats<-dat[dat$ID==j, ]
        tabl<-rbind(tabl,data.frame("id"=j,"nobs"=sum(complete.cases(dats))))
}
tabl
}
