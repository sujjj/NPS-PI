#########Functions 

f.Pfer <- function(year){ ###calcuate the P fertilization rate from county level census and digitize it to spatial map
  
  ###Cropland extraction
  AgriLand <- Landuse
  AgriLand[AgriLand != 412] <- 0 
  AgriLand[AgriLand == 412] <- 1
  
  ###administration unit extration
  county.x <- rasterize (countybd,AgriLand,field ="CountyCode") ###countycode 1,2,3等分别代指研究区内各行政单元
  
 
  inter.r <- AgriLand*county.x
  
  ###generating the series map of fertilization rates
  temp <- inter.r
  for (j in 1:length(countyname))
  {      
    temp [temp == j] <-  Pfer[Pfer$Year==year,j+1][[1]]
    temp <- resample(temp,mask,method="ngb") 
  }
  
  return(temp)
  
}



f.Pman <- function (year){###calcuate mannure P generation rate from county level census and digitize it to spatial map
  
  #####Mannure TP generation estimation
  livestck <- stack(livestock[livestock$Year==year,3:8])
  livestck$County <-livestock[livestock$Year==year,1]
  livestck$Year <- rep(year,2)
  
  
  livestck <- merge(livestck,ecc.TP,by.x="ind",by.y="Type",all=T)
  livestck <- merge(livestck,area,by="County",all=T)
  livestck <- merge(livestck,perc.area,by="County",all=T)
  
  livestck$excr.TP <-livestck$values * livestck$TP *livestck$perc.area / livestck$SQKM_COUN 
  
  result <- aggregate (livestck$excr.TP, by=list(livestck$Year,livestck$County),FUN = sum)
  colnames(result)<-c("year","County","excr.Pman (kg/ha)")
  
  county.y <- rasterize (countybd,mask,field ="CountyCode") 
  
  for (j in 1:length(countyname)){
    county.y [county.y == j] <-  result[j,3]}
  return(county.y)
  
}


f.runoff<-function(year){###prepare the daily runoff data to model inputs meeting the format requirement of the model
  
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
  
  arrange(tFts,TF)
  
  f.ecdf<-ecdf(tFts$TF)
  
  TFranks <- f.ecdf(TF)
  
  return(cbind(TF,RDsub,RDsur,DRsub,DRsur,TFranks))
}



###################################################USLE MODULE
##(1) LS factors
f.S<-function(x){
  x[is.na(x)]<-NA
  x[x >= 0 & x < 0.08726646] <- 10.8 * sin(x[x >= 0 & x < 0.08726646]) +0.03
  x[x >= 0.08726646 & x < 0.17453293]<- 16.8 * sin(x[x >= 0.08726646 & x < 0.17453293]) - 0.5
  x[x >= 0.17453293] <- 21.91 * sin(x[x >= 0.17453293]) - 0.96
  return(x)
}  ######### x: the slope degrees in radians; 0.08726646,0.17453293: slope thresholds in radians in the empirical equations



f.li<-function(x,y){
  
  flowdir <- terrain(x, opt='flowdir', neighbors=8)
  Di <- flowdir
  Di [Di==2 | Di==8 | Di==32 | Di==128 ] <- sqrt(2)*30
  Di [Di!=2 & Di!=8 & Di!=32 & Di!=128 ] <- 30
  
  return(Di/cos(y))
} #### x: DEM; y : slope degrees in radians


f.m <- function(x){
  m <- tan(x)
  m[m >= 0.05] <- 0.5
  m[m >= 0.03 & m < 0.05] <- 0.4
  m[m >= 0.01 & m < 0.03] <- 0.3
  m[m < 0.01] <- 0.2
  return(m)
} #########m值,公式中使用百度比坡度


##（2））K factor estimated following the empirical k equation in Beijing soils
f.k<-function(x,y,z,v){
  temp1<-0.2+0.3*exp(-0.0256*x*(1-y/100))
  temp2<-(y/(z+y))^0.3
  temp3<-1-0.25*v/(v+exp(3.72-2.95*v))
  temp4<-1-0.7*(1-x/100)/(1-x/100+exp(-5.51+22.9*(1-x/100)))
  k<-0.0726*temp1*temp2*temp3*temp4-0.0102
  return(k)
}


##（4）Rain and rain erosivity factor

f.Rain<-function(year){
  
  k <- length(RainSite$Name)
  temp <- as.matrix(subset(Rain,Year==year))
  
  rain <-data.frame(names(temp[,2:(k+1)]),as.vector(temp[1,2:(k+1)]))
  names(rain)[1:2] <- c("Name","Rain")
  
  rain <- merge(rain,RainSite,by ="Name")
  
  coordinates(rain) <- c("lon","lat")
  proj4string(rain) <- crs.geo ###
  
  require(gstat)
  
  idw.out <- idw( formula = Rain~ 1, locations = rain, 
                  newdata = bound,idp=1)####the inverse distance interpolation 
  
  R <- rasterize(idw.out,mask,field ="var1.pred") 
  
  return(R)
}


f.R<-function(year,param11,param12){###rain erosivity factor
  
  k <- length(RainSite$Name)
  Rain <- subset(Rain,Year==year)
  
  rain <- matrix(data=NA,nrow=k)
  for (i in 1:k){
    rain[i,1] <- round(param11 * (Rain[,i+1]) ^ param12,digits = 1)
  } ######Rain to Rain erosivity
  
  rain <- data.frame(names(Rain)[2:(k+1)],as.vector(rain))
  names(rain)[1:2] <- c("Name","R")
  
  rain <- merge(rain,RainSite,by ="Name")
  
  coordinates(rain) <- c("lon","lat")
  proj4string(rain) <- crs.geo ###
  
  require(gstat)
  
  idw.out <- idw( formula = R~ 1, locations = rain, 
                  newdata = bound,idp=1)###the inverse distance interpolation
  
  R <- rasterize(idw.out,mask,field ="var1.pred") 
  
  return(R)
}



##（5）CP factors were assigned with emprical values according to land use types (Numbers are the land use type ID in Chinese landuse classification method)

f.c<-function(x){
  x[x == 112 | x == 113 | x == 114 | x == 122] <- 0.006
  x[x == 213] <- 0.15
  x[x == 322 | x == 323 | x == 511] <- 0
  x[x == 412] <- 0.22
  x[x == 613] <- 1
  return(x)
}


f.p<-function(x){
  x[x == 112 | x == 113 | x == 114 | x == 122 | x == 213 | x == 511 | x == 613 ] <- 1
  x[x == 322 | x == 323] <- 0
  x[x == 412] <- 0.35
  return(x)
}


f.usle<-function (year,param11,param12){
  
  slope.beta <- terrain(dem,opt='slope', unit='radians', neighbors=8) 

  ##################################（1）Calculation of each factor
  S <- f.S(slope.beta)
  L <- (f.li(dem,slope.beta)/22.13)^ f.m(slope.beta)
  k <- f.k(sand,silt,clay,oc)
  P <- f.p(Landuse)
  C <- f.c(Landuse)
  
  ##################################（2）Convert coordinate system and boundary
  
  S.resamp <- resample(S,mask,method="ngb")
  L.resamp <- resample(L,mask,method="ngb")
  k.resamp <- resample(k,mask,method="ngb")
  C.resamp <- resample(C,mask,method="ngb")
  P.resamp <- resample(P,mask,method="ngb")
  
  R.resamp <- f.R(year,param11,param12)
  
  ##################################（3）Calculation of erosion rates
  mydata<-stack(S.resamp,L.resamp,k.resamp,C.resamp,P.resamp,R.resamp)
  
  f.A <- function(x) {prod(x)} 
  
  return(f.A(mydata)) #######t•hm•a-1
}


f.dis <- function(x,y){
  
  ####boundary raster with NA values
  r <- raster(ext=extent(x),resolution=res(x))
  
  p <- as(r,"SpatialPoints")#########raster to points
  
  dist <- gDistance(y,p, byid=TRUE)
  r[] <- apply(dist,1,min)
  
  return(r)
}


f.Pind <- function(year,param){
  
  Pfer.r <- f.Pfer(year)
  Pman.r <- f.Pman(year)
  Runoff <- as.data.frame(f.runoff(year))
  
  ##############DP calcualtion
  
  ###(1)Calculation of soil soluble P loss considering soil P conversion and DP delivery efficiency.
  
  DPsoi_sur <- Olsenp *prod(Runoff$RDsur,param[[1]],param[[2]],0.01)
  DPsoi_sub <- Olsenp *prod(Runoff$RDsub,param[[1]],param[[2]],0.01,param[[3]])
  
  ###(2)Caculation of Mannure DP considering losable percentage of manure P,DP delivery efficiency and P concentration ratio between diffrent runoff pahtways. 
  
  DPman_sur <- Pman.r*prod(Runoff$TFranks,Runoff$DRsur,param[[4]],param[[5]],param[[2]])
  DPman_sub <- Pman.r*prod(Runoff$TFranks,Runoff$DRsub,param[[4]],param[[5]],param[[2]],param[[3]])
  
  ###(3)Caculation of fertiluzer DP considering fertilizer P utilization rates,DP delivery efficiency and P concentration ratio between diffrent runoff pahtways. 
  
  DPfer_sur <- Pfer.r*prod(Runoff$TFranks,Runoff$DRsur,param[[6]],param[[2]])
  DPfer_sub <- Pfer.r*prod(Runoff$TFranks,Runoff$DRsub,param[[6]],param[[2]],param[[3]])
  
  
  TDPsur <- sum(DPsoi_sur,DPman_sur,DPfer_sur)
  TDPsub <- sum(DPsoi_sub,DPman_sub,DPfer_sub)
  TDP <- TDPsur + TDPsub
  
  ##############PP calcualtion
  
  A <- f.usle(year,param[[11]],param[[12]])
  
  PER <- calc(A,fun=function(x){exp( param[[7]] - param[[8]] * log(1000 * x))})
  
  SDR <- 1/((((113000*f.dis(x=mask,y=liying))^param[[9]])*((113000*f.dis(x=mask,y=river))^param[[10]])))
  
  Aoutlet <- overlay(A,SDR,fun=function(x,y){return(x*y*Runoff$TFranks)})###单位ton/ha
  
  PPsoi <- overlay(Aoutlet,TPsoi,PER,fun=function(x,y,z){return(x*y*z*0.001)})####Soil产生的PP,单位kg/ha
  
  PPman <- Pman.r*(1-param[[4]])*SDR*Runoff$TFranks
  
  TPP <- sum(PPsoi, PPman)
  
  TP <- sum(TDPsur,TDPsub,TPP)
  
  plot.ls <- list(TP,TPP/TP,TDPsur/TDP,A)
  
  return(plot.ls)
  
}



f.leaf <- function(x){ ####Loading leaflet online maps and add raster to overlay 
  pal <- colorBin(c("white","green","red"),values(x),10,na.color = "transparent")
  leaflet()%>%setView(lng=117.65,lat=40.50,zoom=10)%>%addTiles()%>%addRasterImage(x,colors=pal,opacity=0.5)%>%addLegend(pal = pal, values = values(x))
  
}


f.summary <- function (x){return(cellStats(x,"mean",na.rm=T))}




################Customized Plot functions
f.comp <- function(y2,z,v,inv,x,y){
  
  plot(Year,y2,col="grey70",type="l",lwd=2,lty=5,ann=F,axes=F)
  axis(2,las=2,at=seq(0,max(y2[!is.na(y2)]),by=inv),lab=seq(0,max(y2[!is.na(y2)]),by=inv),cex.axis=1)
  axis(1,las=1,at=Year,lab=Year,cex.axis=0.9)
  title(ylab=z,main=v,xlab="Year")
  grid(nx=5,ny=6,lwd=1,lty=3,col="grey")
  points(x,y,pch=15,col="red",cex=1.5)
  legend("topright",c("Observed","Simulated"),lty=c(5,NA),pch=c(NA,15),col=c("grey70","red"),bty="n")
  box()
  }


plot.dig <- function(r){
  df <- as.data.frame(r,xy=TRUE)
  df <- df[!is.na(df$layer),]
  ggplot(df) + labs(x="",y="")+
    geom_raster(aes(x, y, fill=layer)) +
    scale_fill_gradient(high = "darkblue",low = "lightblue")+
    geom_path(data=River,aes(x=long, y=lat),colour="skyblue")+
    geom_point(data=Liying,aes(X, Y),size=3,colour="black")+
    theme(legend.title=element_blank())
}


plot.dig1 <- function(r){
  
  p <-rasterToPoints(r)#####Raster to dataframe data before plotting
  df <- data.frame(p)
  colnames(df) <- c("x","y","variable")
  
  ggplot()+labs(x = "",y =" ") +
    geom_tile(data=df,aes(x,y,fill=variable))+
    scale_fill_gradient(high = "darkblue",low = "lightblue")+
    theme(legend.title=element_blank(),
          plot.title = element_text(hjust = 0,size=10,face="bold"))+
    geom_path(data=River,aes(x=long, y=lat),colour="black")+
    geom_point(data=Liying,aes(X, Y),size=3,colour="red")
}
