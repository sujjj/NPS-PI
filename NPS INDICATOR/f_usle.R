###############################USLE module
####Functions for each factor


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
  
  return(Di/cos(y))   #### x: DEM; y : slope degrees in radians
} 


f.m <- function(x){
  m <- tan(x)
  m[m >= 0.05] <- 0.5
  m[m >= 0.03 & m < 0.05] <- 0.4
  m[m >= 0.01 & m < 0.03] <- 0.3
  m[m < 0.01] <- 0.2
  return(m) #### x :  slope degrees in %
} 


##（2）K factor estimated following the empirical k equation in Beijing soils
f.k<-function(x,y,z,v){
  temp1<-0.2+0.3*exp(-0.0256*x*(1-y/100))
  temp2<-(y/(z+y))^0.3
  temp3<-1-0.25*v/(v+exp(3.72-2.95*v))
  temp4<-1-0.7*(1-x/100)/(1-x/100+exp(-5.51+22.9*(1-x/100)))
  k<-0.0726*temp1*temp2*temp3*temp4-0.0102
  return(k)
} 




##（4）rain erosivity factor

f.R<-function(rainsite,dailyR,year,param1,param2,countybd,Mask){

  k <- length(RainSite$Name)
  dailyR <- 
  Rain <- subset(dailyR,Year==year)
  
  rain <- matrix(data=NA,nrow=k)
  for (i in 1:k){
    rain[i,1] <- round(param1 * (Rain[,i+1]) ^ param2,digits = 1)
  } ######formula converting rain to rain erosivity
  
  rain <- data.frame(names(Rain)[2:(k+1)],as.vector(rain))
  names(rain)[1:2] <- c("Name","R")
  
  rain <- merge(rain,RainSite,by ="Name")
  
  coordinates(rain) <- c("lon","lat")
  proj4string(rain) <- crs.geo
  
  require(gstat)
  
  idw.out <- idw( formula = R~ 1, locations = rain, 
                  newdata = countybd,idp=1)####the inverse distance exploration 
  
  R <- rasterize(idw.out,mask,field ="var1.pred") 
  
  return(R)
}


##（5）CP factors were assigned with emprical values according to land use types (Numbers are the land use type ID in Chinese landuse classification method)

f.c<-function(x){
  x[x == 112 | x == 113 | x == 114 | x == 122] <- 0.006  ####Forest type
  x[x == 213] <- 0.15    ####Grassland type
  x[x == 322 | x == 323 | x == 511] <- 0
  x[x == 412] <- 0.22 ####Agrland type
  x[x == 613] <- ####Bare soil
    return(x)
}


f.p<-function(x){
  x[x == 112 | x == 113 | x == 114 | x == 122 | x == 213 | x == 511 | x == 613 ] <- 1
  x[x == 322 | x == 323] <- 0
  x[x == 412] <- 0.35
  return(x)
}



f.usle<-function (year,dem,Soisand,Soisilt,Soiclay,om,lu,rainsite,dailyR,param1,param2,countybd,Mask){
  
  slope.beta <- terrain(dem,opt='slope', unit='radians', neighbors=8)  ####slope degrees in radians
  
  ################################## 
  S <- f.S(slope.beta)
  L <- (f.li(dem,slope.beta)/22.13)^ f.m(slope.beta)
  k <- f.k(Soisand,Soisilt,Soiclay,om/1.724)
  P <- f.p(lu)
  C <- f.c(lu)
  R <- f.R(rainsite,dailyR,year,param1,param2,countybd,Mask)###rain erosivity in each year
  
  ##################################Erosion rates calculation
  mydata<-stack(S,L,k,C,P,R)
  
  f.A <- function(x) {prod(x)} 
  
  return(f.A(mydata)) ######kg/ha
}



