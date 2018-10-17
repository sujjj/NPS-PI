
########################################################Load libraries
library(sp)
library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)
library(grid)
library(rgeos) ###geodistance
library(gstat) ###inverse distance

library(EcoHydRology) ###baseflow separation
library(abind)
library(deSolve)
library(rootSolve)
library(coda)
library(FME)


################################################################SET WORKING DIRECTORY
path <- 
setwd(path)


####################################################################################LOAD FUNCTIONS
source(paste(path,"\\NPS INDICATOR\\gis_pretreat.R",sep="")) ###for ggplot plotting
source(paste(path,"\\NPS INDICATOR\\f_Pfer.R",sep="")) ###calcuate the P fertilization rate from county level census and digitize it to spatial map
source(paste(path,"\\NPS INDICATOR\\f_Pman.R",sep="")) ###calcuate mannure P generation rate from county level census and digitize it to spatial map
source(paste(path,"\\NPS INDICATOR\\f_runoff.R",sep="")) ###prepare the daily runoff data to model inputs meeting the format requirement of the model
source(paste(path,"\\NPS INDICATOR\\f_dis.R",sep="")) ###the inverse distance between to spatial subjects.
source(paste(path,"\\NPS INDICATOR\\ecdf.R",sep=""))###empirical distribution functions
source(paste(path,"\\NPS INDICATOR\\f_usle.R",sep="")) ###USLE calcuation function
source(paste(path,"\\NPS INDICATOR\\f_Pind.R",sep="")) ###P indicator calcuation function



############################################################################Inputs preparation
########generation a mask layer with specified coordinate system and spatial resolution
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  
watershed.bd <- spTransform(readOGR(paste(path,"\\NPS INDICATOR\\DATA\\Boundary.shp",sep="")),crs.geo) 
r <- raster(extent(watershed.bd),resolution=0.01022727)
mask <- rasterize (watershed.bd,r,vals=1)

###spatial inputs pretreatment
river <- gis.pretreat("\\NPS INDICATOR\\DATA\\River.shp")
liying <- gis.pretreat("\\NPS INDICATOR\\DATA\\Liying.shp")
county.bd <- gis.pretreat("\\NPS INDICATOR\\DATA\\County.shp")
county.names <- county.bd$NAME

OlsenP <- gis.pretreat("\\NPS INDICATOR\\DATA\\OlsenP.tif")
Landuse <- gis.pretreat("\\NPS INDICATOR\\DATA\\Landuse.tif")
DEM <- gis.pretreat("\\NPS INDICATOR\\DATA\\Dem.tif")
Clay <- gis.pretreat("\\NPS INDICATOR\\DATA\\Clay.tif")
Sand <- gis.pretreat("\\NPS INDICATOR\\DATA\\Sand.tif")
Silt <- gis.pretreat("\\NPS INDICATOR\\DATA\\Silt.tif")
OM <- gis.pretreat("\\NPS INDICATOR\\DATA\\OM.tif")
TPsoi <- (13+2.7*OM + 0.06*OlsenP)^2


#############Rainfall
Rain <- read.csv(paste(path,"\\NPS INDICATOR\\DATA\\P12.csv",sep=""),head=T,sep=",")  ###Daily rainfall more than 12 mm were considered to be effective to generate runoff. 
RainSite <- read.csv(paste(path,"\\NPS INDICATOR\\DATA\\RainSite.csv",sep=""),head=T,sep=",")  

###Rainfall was used as Rainfall erosivity in this study. The conversion of rainfall to Rainfall erosivity followed the equations of Beijing moutain area.
###the digitalizaiton of Rainfall erosivity was performed by IDW interplolation method. code would be presented in the section introducing how to generate USLE factors.

###Observation data
obs <- read.csv(paste(path,"\\NPS INDICATOR\\DATA\\Obs.csv",sep=""),header=T,sep=",") 

###parameters
para.names<-c("RegR1","RegR2","RegSDR1","RegSDR2","RegER1","RegER2","ConvPsoi","DPDR","PDR","PercWSP","PlossDPman","PlossDPfer")
para.sel <- as.matrix(read.csv(paste(path,"\\NPS INDICATOR\\DATA\\para.selected.csv",sep=""),header=T,sep=","))


######Run the model

###numeric outputs
Output <- do.call(f.Pind,args=list(year=2005,lu=Landuse,Pferfn="\\NPS INDICATOR\\DATA\\Pfer_kgha.csv",countybd=county.bd,lvstockfn="\\NPS INDICATOR\\DATA\\Livestock.csv", eccfn="\\NPS INDICATOR\\DATA\\ECC.csv",areafn="\\NPS INDICATOR\\DATA\\countyarea.csv",perc.areafn="\\NPS INDICATOR\\DATA\\perc.area.csv",DailyQ="\\NPS INDICATOR\\DATA\\Rday.csv",AnnualQ="\\NPS INDICATOR\\DATA\\TF.csv",dem=DEM,Soisand=Sand,Soisilt=Silt,Soiclay=Clay,om=OM,rainsite=RainSite,dailyR=Rain,Mask=mask,param=para.sel[3,],Plot=FALSE)) 

###graphical outputs
Output <- do.call(f.Pind,args=list(year=2005,lu=Landuse,Pferfn="\\NPS INDICATOR\\DATA\\Pfer_kgha.csv",countybd=county.bd,lvstockfn="\\NPS INDICATOR\\DATA\\Livestock.csv", eccfn="\\NPS INDICATOR\\DATA\\ECC.csv",areafn="\\NPS INDICATOR\\DATA\\countyarea.csv",perc.areafn="\\NPS INDICATOR\\DATA\\perc.area.csv",DailyQ="\\NPS INDICATOR\\DATA\\Rday.csv",AnnualQ="\\NPS INDICATOR\\DATA\\TF.csv",dem=DEM,Soisand=Sand,Soisilt=Silt,Soiclay=Clay,om=OM,rainsite=RainSite,dailyR=Rain,Mask=mask,param=para.sel[3,],Plot=TRUE)) 


