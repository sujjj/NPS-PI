######Fertilizer P layer digitalization
f.Pfer <- function(year,lu,Pferfn,countybd){
  
  lu[lu != 412] <- 0 
  lu[lu == 412] <- 1
  
  Pfer <- read.csv(paste(path,Pferfn,sep=""),header=T,sep=",")###Fertilization rates in agricultural land, kg/ha
  
  
  ###提取出县市区
  county.r <- rasterize (countybd,lu,field ="CountyCode") ###countycode 1,2,3等分别代指研究区内各行政单元
  
  ###intersect the administration unit layer and agricultural land use layer
  inter.r <- lu*county.r
  
  ###generate the fertilizer P application rates by assigning the fertilization rates into cells according to county names and years
  temp <- inter.r
  
  for (j in 1:length(county.bd$NAME))
  {      
    temp [temp == j] <-  Pfer[Pfer$Year==year,j+1] 
    
    temp <- resample(temp,mask,method="ngb") 
  }
  
  return(temp)
  
}


