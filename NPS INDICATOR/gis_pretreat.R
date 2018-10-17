####判断vector or raster,then transform them into spatial layers with defined boudary,cell size and corrdinate system)
gis.pretreat <- function (x)
{
  if (grepl(".shp",x,fixed=T)==TRUE)
  {y <- spTransform(readOGR(paste(path,"\\",x,sep="")),crs.geo)
  z <- crop(y,mask)
  return(z)}
  else if (grepl(".tif$",x)==TRUE)
  {y <- raster(readGDAL(paste(path,"\\",x,sep="")))
  z <- resample(y,mask,method="ngb")
  return(z)}
}






