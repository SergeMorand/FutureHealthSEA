# Extracting exif data from photos using R
# using smart phones with app gps camera or gps device
# This script is based on:
# http://www.seascapemodels.org/rstats/2016/11/14/extract-exif.html

library(exifr)
library(dplyr)
library(leaflet)

# extract all images of the folder
# there are 4 images: one from a wiko cell phone and three from a GPS Garmin 
# extract data, be carefull of the extension of the files: "*.jpg" or "*.jpeg", or "*.JPG"
# (here ".jpg" extension)
files <- list.files(pattern = "*.jpg")
pics <- exifr(files)

# investigate pics and the number of variables encrypted in a picture from a smart phone or GPS device
names(pics)

# select the useful columns (file name, date of the pictures, longitude, latitude)
pics2 <- select(pics,FileName,
               DateTimeOriginal,
                 GPSLongitude, GPSLatitude)
names(pics2) # see the file

# map using leaflet (see training leaflet)
leaflet(pics2) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(~ GPSLongitude, ~ GPSLatitude)


# Add markers showing the pictures on the map
# be aware of the size of the images (reduce the size)
# map using leaflet (see training leaflet)

map<-leaflet(pics2) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(data = pics2, lng =~pics2$GPSLongitude[1], lat = ~pics2$GPSLatitude[1],
                 popup = paste0("<image src=",pics2$FileName[1],">"))%>%
  addCircleMarkers(data = pics2, lng =~pics2$GPSLongitude[2], 
                   lat = ~pics2$GPSLatitude[2],
                 popup = paste0("<image src=",pics2$FileName[2],">"))%>%
  addCircleMarkers(data = pics2, lng =~pics2$GPSLongitude[3], 
                   lat = ~pics2$GPSLatitude[3],
                   popup = paste0("<image src=",pics2$FileName[3],">"))%>%
  addCircleMarkers(data = pics2, lng =~pics2$GPSLongitude[4], 
                   lat = ~pics2$GPSLatitude[4],
                   popup = paste0("<image src=",pics2$FileName[4],">"))
map

## save to a html file, to open with you browser
## save your leaflet map with popup images
library(htmlwidgets)
library(webshot)

saveWidget(map, "mymapwithpics.html", selfcontained = FALSE)




