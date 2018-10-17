###Runoff function to obtain runoff depths, runoff ratios and runoff ranks from the raw runoff data

f.runoff<-function(DailyQ,AnnualQ,year){
  
  rday <- read.csv(paste(path,DailyQ,sep=""),header=T,sep=",")
  
  Rday <- subset(rday,Year==year)
  
  
  bf<-BaseflowSeparation(Rday$Flow, filter_parameter = 0.95, passes = 1)
  
  Rday.bf<-cbind(Rday[,1:4],bf$bt)
  colnames(Rday.bf)[5]<-"bf"
  
  
  Rday.bf <-  mutate (Rday.bf,RDsub = bf*3600*24/620000,RDsur = (Flow-bf)*3600*24/620000) 
  Rday.bf <-  mutate (Rday.bf,DRsub = RDsub / (RDsub+RDsur),DRsur = 1-RDsub / (RDsub+RDsur),TF = RDsub + RDsur) 
  
  RDsub<-round(tapply(Rday.bf$RDsub,Rday.bf$Year,FUN=sum,simplify=T),digits = 2)
  RDsur<-round(tapply(Rday.bf$RDsur,Rday.bf$Year,FUN=sum,simplify=T),digits = 2)
  DRsub<-round(tapply(Rday.bf$DRsub,Rday.bf$Year,FUN=mean,simplify=T),digits = 2)
  DRsur<-round(tapply(Rday.bf$DRsur,Rday.bf$Year,FUN=mean,simplify=T),digits = 2)
  TF<-round(tapply(Rday.bf$TF,Rday.bf$Year,FUN=sum,simplify=T),digits = 2)
  
  ####TFranks: the TFrank in any year was estimated by the ECDF of long term total runoff series.
  
  tFts <- read.csv(paste(path,AnnualQ,sep=""),header=T,sep=",") 
  
  arrange(tFts,TF)
  
  f.ecdf<-ecdf(tFts$TF)
  
  TFranks <- f.ecdf(TF)
  
  return(cbind(TF,RDsub,RDsur,DRsub,DRsur,TFranks))
}

