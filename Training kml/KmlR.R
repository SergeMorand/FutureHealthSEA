
## Produce kml for Google Earth
## using package plotKML

# read file with coordinates as "lat" and "lon" (csv file with columns separated by ",")
dat<-read.csv("SaenThong_8villages.csv")
names(dat)  # see your variables

# select the useful columns in your file using the package dplyr
# select("file",var1, var2, ...)
library(dplyr)
dat2 <- select(dat,
                 lat, lon,
                 name)


## create a SpatialPointsDataframe object and add the appropriate CRS
# 
require(maptools)
require(rgdal)
coordinates(dat2)<- c("lon", "lat")

# now, use the appropriate CRS, here espg:4326
BNG<- CRS("+init=epsg:4326")

proj4string(dat2)<-BNG

## In order for the points to be displayed in the correct place 
# the points need to be re-projected to WGS84 geographical coordinates
p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

dat2_wgs84<- spTransform(dat2, CRS= p4s)

## Using the OGR KML driver we can then export the data to KML. 
# dsn should equal the name of the exported file and the dataset_options argument allows us to specify the labels displayed by each of the points.
writeOGR(dat2_wgs84, dsn="villages.kml", layer= "dat2_wgs84", driver="KML")


library(plotKML)
# choose the design of the points
# see at http://maps.google.com/mapfiles/kml/
# shape = "http://maps.google.com/mapfiles/kml/shapes/homegardenbusiness.png"
# shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png"
# as an example, choose pushpin with color
shape ="http://maps.google.com/mapfiles/kml/pushpin/blue-pushpin.png"

# start to create, shape is shape, size is the size of the pushpin on the map
# if add labels, it gives the name of the point (here locality)
kml_open("villages_SaenThong.kml")
kml_layer(dat2["name"],  z.lim=c(50,80),colour=name,
          colour_scale=SAGA_pal[[1]], shape=shape, size = 1,
          labels = name)
kml_close("villages_SaenThong.kml") # finish your kml file



## ## ## ## 
## an other example with rodents trapped in the province from 2008 to 2017
rat<-read.csv("rodents.thawangpha.csv")
rat2 <- select(rat,
               longitude, latitude,taxonomyID,
               trappingDate)

## filter for the district
rat2<-rat2 %>% 
  filter( 
    latitude>18.9 & longitude<100.8)

# or the sampling of 2018
rat<-read.csv("rodents.saenthong.2018.csv")
names(rat)
rat2 <- select(rat,
               longitude, latitude,taxonomyID,
               trappingDate)




## create a SpatialPointsDataframe object and add the appropriate CRS
require(maptools)
require(rgdal)
coordinates(rat2)<- c("longitude", "latitude")

#
BNG<- CRS("+init=epsg:4326")
proj4string(rat2)<-BNG

## In order for the points to be displayed in the correct place they need to be re-projected to WGS84 geographical coordinates.
p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
rat2_wgs84<- spTransform(rat2, CRS= p4s)

## Using the OGR KML driver we can then export the data to KML. dsn should equal the name of the exported file and the dataset_options argument allows us to specify the labels displayed by each of the points.
writeOGR(rat2_wgs84, dsn="RodentTWP.kml", layer= "dat2_wgs84", driver="KML")

## if you have Google Earth installed double click on the kml file you just created to open it. The points should be loaded as labelled pins on the map.If you click on the pin you will be able to see its full name and capacity. 
library(plotKML)
# you can use your own drawing image as a shape
shape="rodent.png"

# add colour according to rodent taxonomy, by adding "colour=taxonomyID"
kml_open("RodentTWP.kml")
kml_layer(rat2["taxonomyID"],  z.lim=c(50,80),colour="blue",
          colour_scale=SAGA_pal[[1]], shape=shape, size=1,
          points_names="",
          labels = rat2$taxonomyID)
kml_close("RodentTWP.kml")



## a shape file in kml
#### shapefile of the tamboon Saenthong
library(dismo)
## you need to gave a shape file, read the shape file
saenthong <- shapefile("SaenThong-rgdal.shp")
plot(saenthong) # see the shapefile

library(sp)
##
# area in black
kml(saenthong, file = "SaenThong.kml",colour_scale=rgb(0,0,0, alpha=0.0))
# transparent, boundary in white
kml(saenthong, file = "SaenThong.kml",alpha = 0.0)



