
# you need the package "measurements"
library(measurements)

# read the file
example=read.csv("data.coord.degree.csv")

# transform lat (latitude) and lon (longitude) from degree to decimal
example$lat = measurements::conv_unit(example$lat, from = 'deg_dec_min', to = 'dec_deg')
example$lon = measurements::conv_unit(example$lon, from = 'deg_dec_min', to = 'dec_deg')

# transform to numeric values (as the two columns were initially "text")
example$lat<-as.numeric(example$lat)
example$lon<-as.numeric(example$lon)

# you can save the file for the coordinates in decimal
write.csv(example,file="data.coord.decimal.csv")

# do a kml
## create a SpatialPointsDataframe object and add the appropriate CRS
require(maptools)
require(rgdal)
coordinates(example)<- c("lon", "lat")

# use the appropriate CRS, here espg:4326
BNG<- CRS("+init=epsg:4326")
proj4string(example)<-BNG

# the points need to be re-projected to WGS84 geographical coordinates
p4s <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")
example_wgs84<- spTransform(example, CRS= p4s)

## Using the OGR KML driver we can then export the data to KML.
# dsn should equal the name of the exported file and the dataset_options argument allows us
# to specify the labels displayed by each of the points.
writeOGR(example_wgs84, dsn="example.kml", layer= "example_wgs84", driver="KML")

