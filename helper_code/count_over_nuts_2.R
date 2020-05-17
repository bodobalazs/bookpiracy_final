library(data.table)  ## For faster reading of large CSV file.
library(eurostat)    ## for NUTS2 map
library(sp)          ## re-projections of map, count over shapefiles

dwnld <- fread("C:/Users/Daniel Antal/OneDrive - Visegrad Investments/_data/Europe_download/Europe_download.csv")

#create datasets with number of downloads, rather than libgenIDs
dwnld_count<-dwnld[,.(count=.N), by=.(longitude, latitude)]
coords <- dwnld_count[, c("longitude", "latitude")]

map_nuts2 <- eurostat::get_eurostat_geospatial(resolution = "60",
                                               nuts_level = "2",
                                               year = 2016)

class ( map_nuts2)


dld.sp <- sp::SpatialPointsDataFrame( as.matrix(coords),
                                  data = dwnld_count[, "count"], 
                                  proj4string = sp::CRS (
                                    raster::crs(map_nuts2) ))
##beware  sp::CRS creates the coordinate reference system, 
##raster::crs reads it from another object. 

## map_nuts2 is an sf object that works better with ggplot2
## we need to convert back to 'Spatial' object to use the sp functions.

download_nuts2 = sp::over(sp::geometry(
                            as(map_nuts2, 'Spatial')), 
                          dld.sp, fn=sum)
download_data <- cbind (
  data.frame ( 
  id = as.character(map_nuts2$NUTS_ID), 
  stringsAsFactors = FALSE ), 
  download_nuts2 )

sum(dwnld_count$count)
sum(download_data$count, na.rm=TRUE)


getwd()
saveRDS(download_data, file.path("data","downloads_nuts_2016.rds"))
