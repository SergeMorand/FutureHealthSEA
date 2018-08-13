library(raster)
library(rgdal)
library(dismo)
library(ggplot2)
library(shapefiles)
library(rgeos)
library(rasterVis)

#### Adominstrative boundaries

### reading LDD GIS (Nan province) (added in folder LDD)
nan <- shapefile("./LDD/nan-rgdal.shp")
# extracting shapefile of Tha Wangpha
thawangpha<-subset(nan,AMPHOE_E=="Amphoe Tha Wangpha")
thawangpha$TAMBON_IDN # only IDN are given for subdistrict
# selecting shapefile of Saen Thong subdisctrict (national ID="550608")
thawangpha_st<-subset(thawangpha,TAMBON_IDN=="550608")
# plot
plot(thawangpha_st)

### reading Gisda GIS for Thailand
# as the files are quite heavy, you have to download by your self at:
# http://www.diva-gis.org/gdata
# selecting "Thailand" and subject = "Administrative Areas"
# as an example, I have only added the shapefile of provinces of Thailand in the folder "Thailand_adm"
thailand <- shapefile("./Thailand_adm/THA_adm1.shp")
# list of province
thailand$NAME_1 
# shapefile of Nan province
thai_nan <- thailand[thailand[["NAME_1"]] == "Nan", ]

# when the shapefile is heavy and long to manipulate and plot:
# the function gSimplify is a way to simplify its contour
thailand_s <- gSimplify(thailand, tol=0.01, topologyPreserve=TRUE)
plot(thailand_s)

# shapefile of subdistrict 
# you have to download from # http://www.diva-gis.org/gdata
# thailand3 <- shapefile("THA_adm3.shp")
#list of subdistrict
thailand3$NAME_3 
# shape file of Saen Thong subdistrict
thai_nan_st<-subset(thailand3,NAME_3=="Saen Thong")
plot(thai_nan_st)


## now, you can compare the two shapefiles of Saen Thong (LDD and GIS DIVA)
plot(thawangpha_st)
plot(thai_nan_st, add=TRUE) # they don't match!


#### 
#### adding villages of Saen Tong
# the file "SaenThong_8villages.csv" is also in the folder kml
village_all<-read.csv("./SaenThong_8villages.csv")
village_st<-village_all
coordinates(village_st)<- c("lon", "lat")

## we can look which shapefile of Saen Thong includes all villages
plot(thawangpha_st)
plot(thai_nan_st,add=TRUE)
plot(village_st, add=TRUE) # villages of Saen Thong are included in the GIS DIVA


#### 
#### adding elevation data
# part of this script is from:
# https://www.r-bloggers.com/how-to-quickly-enrich-a-map-with-natural-and-anthropic-details/

### elevation using data from Shuttle Radar Topography Mission (SRTM)
# https://en.wikipedia.org/wiki/Shuttle_Radar_Topography_Mission
# use coordinate point for getting the raster around the point
# use:
# dem.raster <- getData("./STRM_Nan/SRTM", lat = 19.132, lon = 100.791, download = TRUE)
# I have already downloaded and cropped to the size of Nan province using:
# dem.raster_nan <- crop(dem.raster,nan)
# and saving using:
# writeRaster(dem.raster_nan, filename="./STRM/STRM_nan.grd",datatype='INT4S', overwrite=TRUE)
# get the prepared raster

## for Nan province 
dem.raster_nan <- raster("./STRM/STRM_nan")
plot(dem.raster_nan)
# crop for Thawangpha
dem.raster_thawangpha <- crop(dem.raster,thawangpha)
plot(dem.raster_thawangpha)

# The third column of the dem.df contains the altitude value (alt).
dem.m  <-  rasterToPoints(dem.raster_nan)
dem.df <-  data.frame(dem.m)
colnames(dem.df) = c("lon", "lat", "alt")

# Transforming in hill shade 
# Extract slope and aspect of the terrain with raster::terrain(). 
# Compute the hill shade with raster::hillShade(), 
# setting the elevation angle (of the sun) to 40 and the direction angle of the light to 270 
# transform the resulting hill.raster into a data.frame
slope.raster <- terrain(dem.raster_nan, opt='slope')
aspect.raster <- terrain(dem.raster_nan, opt='aspect')
hill.raster <- hillShade(slope.raster, aspect.raster, 40, 270)

hill.m <- rasterToPoints(hill.raster)
hill.df <-  data.frame(hill.m)
colnames(hill.df) <- c("lon", "lat", "hill")

# look the results for Nan
plot(hill.raster,col=gray.colors(10)) # to get range of grey colors
plot(thai_nan_st,add=TRUE)
plot(thai_nan,add=TRUE)
plot(village_st
     ,add=TRUE)


## now, for Thanwgpha district
dem.m_thawangpha  <-  rasterToPoints(dem.raster_thawangpha)
dem.df_thawangpha  <-  data.frame(dem.m_thawangpha )
colnames(dem.df_thawangpha ) = c("lon", "lat", "alt")

slope.raster_thawangpha  <- terrain(dem.raster_thawangpha , opt='slope')
aspect.raster_thawangpha  <- terrain(dem.raster_thawangpha, opt='aspect')
hill.raster_thawangpha  <- hillShade(slope.raster_thawangpha, aspect.raster_thawangpha,
                                     40, 270)

hill.m_thawangpha  <- rasterToPoints(hill.raster_thawangpha)
hill.df_thawangpha  <-  data.frame(hill.m_thawangpha)
colnames(hill.df_thawangpha) <- c("lon", "lat", "hill")

# look the results for Thawangpha
plot(hill.raster_thawangpha,col=gray.colors(10)) # to get range of grey colors
plot(thai_nan_st,add=TRUE)
plot(thai_nan,add=TRUE)
plot(village_st
     ,add=TRUE)


### another elevation data
## which allows to add elevation lines to a map
## see http://topotools.cr.usgs.gov/GMTED_viewer/viewer.htm
## Explanation of the 0.5 arc-sec shift (pp. 9--10):
## http://pubs.usgs.gov/of/2011/1073/pdf/of2011-1073.pdf
## file downloaded on 2015-08-25 from:
## http://topotools.cr.usgs.gov/gmted_viewer/gmted2010_global_grids.php
# the DEM of Indochina has been already prepared and added in the folder "dem"
demindochina<-raster("./dem/dem_indochina")
dem_nan<- crop(demindochina, extent(100.3415 , 101.3582, 18.00819 , 19.63319 ))
dem_nan_t <- mask(dem_nan,nan)
dem_nan_line_t <- rasterToContour(dem_nan_t, nlevels = 50)

# getting elevation for each village
my.pts.elevation <- extract(dem_nan,village_st)
my.pts.elevation

# getting elevation by village
village_7<-subset(village_st,name=="7 Ban Santisouk")
plot(thai_nan_st)
plot(village_7, add=TRUE)
my.pt.elevation <- extract(dem_nan,village_7)
my.pt.elevation

village_1<-subset(village_st,name=="1 Ban Na Non")
plot(thai_nan_st)
plot(village_1, add=TRUE)
my.pt.elevation <- extract(dem_nan,village_1)
my.pt.elevation


## draw a line between 2 villages
library(dplyr) # prepare from two villages
my_points<-select(village_all, "lon","lat")
ban7<-filter(my_points, row_number() == 7L)
ban1<-filter(my_points, row_number() == 1L)
ban1_7<-full_join(ban1,ban7)

library(sf) # this library helps at getting a spatial line between two spatial points
# you need to add a reference system
my_points.sf.7_1 <- st_as_sf(ban1_7, coords = c("lon", "lat"), crs = 4326)
my_line.sf.7_1<-SpatialLines(list(Lines(Line(ban1_7), ID="a")))
my_line.sf.7_1@proj4string<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

plot(thai_nan_st)
plot(my_points.sf.7_1,add=TRUE)
plot(my_line.sf.7_1,add=TRUE)

# extract elevation along the line
my.pt.elevation <- extract(dem_nan,my_line.sf.7_1)
my.pt.elevation # elevation data in meter along the line


####
### land use
## land use from UCLouvain ESA Globcover dataset, to get the world data
## Download at: http://due.esrin.esa.int/page_globcover.php 
# here, I prepared the land use of nan
# landuse <- raster("./ressources/Globcover2009_V2.3_Global/GLOBCOVER_L4_200901_200912_V2.3.tif")
# landuse_nan <- crop(landuse, extent(100.3415 , 101.3582, 18.00819 , 19.63319 ))
# writeRaster(landuse_nan, filename="landuse_nan", bandorder='BIL', overwrite=TRUE)

landuse <- raster("./landuse/landuse_nan")

# landuse of Nan
landuse_nan<-mask(landuse,nan)
plot(landuse_nan)
cellStats(landuse_nan,'mean') # cellStats to compute some stats from the raster

# landuse of Thawangpha
landuse_nan_twp<- crop(landuse, extent(100.56 , 100.97, 18.975 , 19.313 ))
landuse_nan_twp_m<-mask(landuse_nan_twp,thawangpha)

# getting information on land use, patches, superficy, etc 
# using "fragstats" implemented in the package SDMTools
library(SDMTools)

ps.data = PatchStat(landuse_nan)
ps.data
PatchStat(landuse_nan) # metadata for patchID is given in "Globcover2009_Legend.xls"
PatchStat(landuse_nan_twp_m)



####   
### calculating a buffer
## create buffer size around each village using the pachage dismo
library(dismo)
# use village_st, already as a spatialpoints of all villages
village_st
# give a projection to village_st (same as landuse_nan)
proj4string(village_st) <- proj4string(landuse_nan)

village_st$name # list of the villages in village_st
# isolate each village as a unique spatial point
village_1<-subset(village_st,name=="1 Ban Na Non")
village_2<-subset(village_st,name=="2 Ban Na Sai")
village_3<-subset(village_st,name=="3 Ban Poh")
village_4<-subset(village_st,name=="4 Ban Huak")
village_5<-subset(village_st,name=="5 Ban Nam Krai")
village_6<-subset(village_st,name=="6 Ban Huay Muang")
village_7<-subset(village_st,name=="7 Ban Santisouk")
village_8<-subset(village_st,name=="8 Ban Hae")

# draw a buffer of distance "d" in meter, here d= 1000 meters
# ans look at the results
cs1 <- circles(village_1, d=1000)
plot(thai_nan_st)
plot(village_1,add=TRUE)
plot(polygons(cs1), add=TRUE)

cs2 <- circles(village_2, d=1000)
plot(village_2,add=TRUE)
plot(polygons(cs2), add=TRUE)

cs3 <- circles(village_3, d=1000)
plot(village_3,add=TRUE)
plot(polygons(cs3), add=TRUE)

cs4 <- circles(village_4, d=1000)
plot(village_4,add=TRUE)
plot(polygons(cs4), add=TRUE)

cs5 <- circles(village_5, d=1000)
plot(village_5,add=TRUE)
plot(polygons(cs5), add=TRUE)

cs6 <- circles(village_6, d=1000)
plot(village_6,add=TRUE)
plot(polygons(cs6), add=TRUE)

cs7<- circles(village_7, d=1000)
plot(village_7,add=TRUE)
plot(polygons(cs7), add=TRUE)

cs8<- circles(village_8, d=1000)
plot(village_8,add=TRUE)
plot(polygons(cs8), add=TRUE)

# extract the landuse data for each buffer
vil1_circle<-polygons(cs1)
landcover_vil1<-crop(landuse_nan,vil1_circle)

vil2_circle<-polygons(cs2)
landcover_vil2<-crop(landuse_nan,vil2_circle)

vil3_circle<-polygons(cs3)
landcover_vil3<-crop(landuse_nan,vil3_circle)

vil4_circle<-polygons(cs4)
landcover_vil4<-crop(landuse_nan,vil4_circle)

vil5_circle<-polygons(cs5)
landcover_vil5<-crop(landuse_nan,vil5_circle)

vil6_circle<-polygons(cs6)
landcover_vil6<-crop(landuse_nan,vil6_circle)

vil7_circle<-polygons(cs7)
landcover_vil7<-crop(landuse_nan,vil7_circle)

vil8_circle<-polygons(cs8)
landcover_vil8<-crop(landuse_nan,vil8_circle)


# extract patches, area, perimeter, perimeter to area ... 
# using the package SDMTools
# metadata for patchID is given in "Globcover2009_Legend.xls"
ps.data.1 = PatchStat(landcover_vil1)
ps.data.2 = PatchStat(landcover_vil2)
ps.data.3 = PatchStat(landcover_vil3)
ps.data.4 = PatchStat(landcover_vil4)
ps.data.5 = PatchStat(landcover_vil5)
ps.data.6 = PatchStat(landcover_vil6)
ps.data.7 = PatchStat(landcover_vil7)
ps.data.8 = PatchStat(landcover_vil8)

ps.data.1
ps.data.2
ps.data.3
ps.data.4
ps.data.5
ps.data.6
ps.data.7
ps.data.8


############
#####
### plot your maps
# fix / choose colors using function rasterTheme from library rasterVis
mapTheme <- rasterTheme(region=brewer.pal(8,"Greens"))

# landuse of Nan using rasterVis
planduse<-levelplot(landuse_nan,margin=F, par.settings=mapTheme,
                    sub = expression("Land USe"), 
                    xlab = NULL, ylab = NULL)+ 
  layer(sp.lines(thawangpha, col="black", lwd=0.5))+ 
  layer(sp.lines(thai_nan_st, col="black", lwd=0.5))+
  layer(sp.lines(vil1_circle, col="black", lwd=2))+
  layer(sp.points(village_1, col="red", lwd=2))
planduse

# landuse of Thawangpha with villages and buffers using rasterVis
planduse_twp<-levelplot(landuse_nan_twp_m,margin=F, par.settings=mapTheme,
                    sub = expression("Land Use Thawangpha"), 
                    xlab = NULL, ylab = NULL)+
  layer(sp.lines(thai_nan_st, col="black", lwd=1))+
  layer(sp.lines(vil1_circle, col="black", lwd=2))+
  layer(sp.lines(vil2_circle, col="black", lwd=2))+
  layer(sp.lines(vil3_circle, col="black", lwd=2))+
  layer(sp.lines(vil4_circle, col="black", lwd=2))+
  layer(sp.lines(vil5_circle, col="black", lwd=2))+
  layer(sp.lines(vil6_circle, col="black", lwd=2))+
  layer(sp.lines(vil7_circle, col="black", lwd=2))+
  layer(sp.lines(vil7_circle, col="black", lwd=2))+
  layer(sp.lines(vil8_circle, col="black", lwd=2))+
  layer(sp.points(village_st, col="red", lwd=2))
planduse_twp

## elevation map
# change the color to a range of greys
mapTheme <- rasterTheme(region=brewer.pal(9,"Greys"))

# elevation map of Thawangpha
pdem<-levelplot(hill.raster_thawangpha,margin=F, par.settings=mapTheme,
                    sub = expression(" "), 
                    xlab = NULL, ylab = NULL)+ 
  layer(sp.lines(thai_nan, col="black", lwd=0.5))+ 
  layer(sp.lines(thai_nan_st, col="black", lwd=0.5))+
  layer(sp.lines(vil1_circle, col="black", lwd=2))+
  layer(sp.points(village_1, col="red", lwd=2))
pdem
