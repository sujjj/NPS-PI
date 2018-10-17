#### Manure P layer digitalization

f.Pman <- function (lvstockfn, eccfn,areafn,perc.areafn,lu,year,countybd){
  
  livestock <- read.csv(paste(path,lvstockfn,sep=""),head=T,sep=",")
  ecc.TP <- read.csv(paste(path,eccfn,sep=""),header=T,sep=",")
  area <- read.csv(paste(path,areafn,sep=""),header=T,sep=",")
  perc.area <- read.csv(paste(path,perc.areafn,sep=""),header=T,sep=",")
  
  
  #####Mannure TP production
  livestck <- stack(livestock[livestock$Year==year,3:8])
  livestck$County <-livestock[livestock$Year==year,1]
  livestck$Year <- rep(year,2)
  
  
  livestck <- merge(livestck,ecc.TP,by.x="ind",by.y="Type",all=T)
  livestck <- merge(livestck,area,by="County",all=T)
  livestck <- merge(livestck,perc.area,by="County",all=T)
  
  livestck$excr.TP <-livestck$values * livestck$TP *livestck$perc.area / livestck$SQKM_COUN #######TP (kg/ha) in each adminstration unit
  
  result <- aggregate (livestck$excr.TP, by=list(livestck$Year,livestck$County),FUN = sum)
  colnames(result)<-c("year","County","excr.Pman (kg/ha)")
  
  county.r <- rasterize (countybd,lu,field ="CountyCode") ###countycode 1,2分别指的是兴隆县和营子区
  
  temp <- county.r
  for (j in 1:length(county.bd$NAME)){
    temp [ temp == j] <-  result[j,3]
    temp <- resample( temp,mask,method="ngb")
  }
  
  return( temp)
}


