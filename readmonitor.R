readmonitor<-function(directory,ID){
                data<-data.frame(colnames(c("Date","sulfate","nitrate","ID")))
                        for (i in seq_along(ID))
                                fid<-sprintf("%03.f",ID[i])
                                file<-paste(c(directory,"/",fid,".csv"),collapse="")
                                fdata<-read.csv(file)
                                data<-rbind (data,fdata)
}
