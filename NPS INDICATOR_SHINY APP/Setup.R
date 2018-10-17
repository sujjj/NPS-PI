library(sp)
library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)
library(grid)
library(rgeos) ###geodistance
library(gstat) ###inverse distance
library(EcoHydRology) ###baseflow separation
library(leaflet) 


#####################################################################################Pre-treatment
path <- 
setwd(path)


##############################(1)Spatial data
## vector data
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")   ####defined geo-coordinate system

Boundfile <- paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\boundLiu.shp",sep="")    ###Watershed boundary
bound <- readOGR(Boundfile) 
bound <- spTransform(bound,crs.geo)

river <- readOGR(paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\river.shp",sep="")) ##river network
liying <- readOGR(paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\liying.shp",sep="")) ## hydrological station
river <- spTransform(river,crs.geo)
liying <- spTransform(liying,crs.geo)

River <- fortify(river)
Liying <- as.data.frame(liying)

##raster data
mask <- raster(paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\mask.c.grd",sep=""))         ###Mask with defined boundary,spatial resolution and coordinate system
dem <- raster(readGDAL(paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\dem30.tif",sep="")))  ###DEM
clay <- raster(readGDAL(paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\clay.tif",sep="")))   ###Soil clay layer derived from soil map (90m*90m)
sand<-raster(readGDAL(paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\sand1.tif",sep="")))  ###Soil sand layer derived from soil map (90m*90m)
silt <- raster(readGDAL(paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\silt.tif",sep="")))   ###Soil silt layer derived from soil map (90m*90m)
Rain <- read.csv(paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\P12.csv",sep=""),head=T,sep=",")  ###Daily rainfall above 12mm were classified as effective rainfall (mm).
RainSite <- read.csv(paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\RainSite.csv",sep=""),head=T,sep=",")  ##Rain station sites within watershed
om <- raster(readGDAL(paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\OM.tif",sep="")))  ###Soil OM layer derived from soil map (90m*90m)
oc <- om/1.724   ###OM TO OC
om  <- resample(om,mask,method="ngb")

Olsenp <- raster(readGDAL(paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\OlsenP1.tif",sep="")))
Olsenp <- resample(Olsenp,mask,method="ngb") 
TPsoi <- (13+2.7*om + 0.06*Olsenp)^2


##############################(2)Tabular data
##Fertilization realted data
Pfer <- read.csv(paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\Pfer_kgha.csv",sep=""),header=T,sep=",") ###Fertilization rates in agricultural land, kg/ha
Landuse  <- raster(readGDAL(paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\landsue.tif",sep="")))       
countybd <- readOGR(paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\county1.shp",sep=""))     ###Administration unit boundary within watershed
countyname <- c("Xinglong","Yingzi") 

##Rurual population and ivestock related data
livestock <- read.csv(paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\Livestock.csv",sep=""),head=T,sep=",")
ecc.TP <- read.csv(paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\ECC.csv",sep=""),header=T,sep=",")
area <- read.csv(paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\countyarea.csv",sep=""),header=T,sep=",")
perc.area <- read.csv(paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\perc.area.csv",sep=""),header=T,sep=",")


##Runoff
rday <- read.csv(paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\Rday.csv",sep=""),header=T,sep=",")
tFts <- read.csv(paste(path,"\\NPS INDICATOR_SHINY APP\\Data\\TF.csv",sep=""),header=T,sep=",") 

##Simulation period and observations 

Year <- 2000:2017
A.obs <- c(6.9, 25.3,NA, 0.2,6.3,19.4, 2.5,1.2,4.8,6.7,0.8,NA,NA,NA,NA,NA,NA,NA)  ###Annual Sediment loads in outlet
P.obs <- c(0.193,1.558,0.070,0.158,0.982,1.321,0.580,0.860,0.982,0.961,0.751,NA,NA,NA,NA,NA,NA,NA) ###Annual NPS P loads in outlet
obs <- data.frame(cbind(Year,A.obs,P.obs))



