###以mask图层为边界，求mask上各点到river的距离

f.dis <- function(Mask,y){
  
  ####boundary raster with NA values
  r <- raster(ext=extent(Mask),resolution=res(Mask))
  
  p <- as(r,"SpatialPoints")#########raster 转化为points
  
  dist <- gDistance(y,p, byid=TRUE)
  r[] <- apply(dist,1,min)
  
  return(r)
}


########################
####The Wikipedia page Decimal Degrees has a table on Degree Precision vs. Length. Also the accuracy of your coordinates depends on the instrument used to collect the coordinates - A-GPS used in cell phones, DGPS etc.

########decimal
########places   degrees          distance
########-------  -------          --------
############0        1                111  km
############1        0.1              11.1 km
############2        0.01             1.11 km
############3        0.001            111  m
############4        0.0001           11.1 m
############5        0.00001          1.11 m
############6        0.000001         11.1 cm
############7        0.0000001        1.11 cm
############8        0.00000001       1.11 mm