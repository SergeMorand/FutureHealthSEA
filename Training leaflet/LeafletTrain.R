# 

library(leaflet)

#
# use the csv file "SaenThong_8villages.csv" and shapefile "SaenThong-rgdal" 
# from training kml file

ban<-read.csv("SaenThong_8villages.csv")
names(ban)

library(raster)
st<-shapefile("SaenThong-rgdal")


leaflet(ban) %>%
  addTiles(group = "OSM") %>%
  addProviderTiles("Esri.WorldImagery",group ="Esri World Imagery") %>%
  addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",
           attribution = 'Google',group ="Google") %>%
  addProviderTiles("Esri.WorldShadedRelief",group ="World Shaded Relief") %>%
  addProviderTiles("Esri.NatGeoWorldMap",group ="Nat Geo World Map") %>%
  addMarkers(~ lon, ~ lat,  popup = ~as.character(name)) %>%
  addPolygons(data = st,
              color = "green", 
              fillColor = "gray", 
              fillOpacity = 0.25, 
              weight = 1) %>%
  addLayersControl(baseGroups = c("OSM", "Esri World Imagery","Google","World Shaded Relief",
                                  "Nat Geo World Map"), 
                   overlayGroups = c("SESYNC"),
                   options = layersControlOptions(collapsed = FALSE))


library(mapview)
## 'leaflet' objects (image above)
m <- leaflet(ban) %>%
  addTiles(group = "OSM") %>%
  addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",
           attribution = 'Google',group ="Google") %>%
  addProviderTiles("Esri.NatGeoWorldMap",group ="Nat Geo World Map") %>%
  addMarkers(~ lon, ~ lat,  popup = ~as.character(name)) %>%
  addPolygons(data = st,
              color = "green", 
              fillColor = "gray", 
              fillOpacity = 0.25, 
              weight = 1) %>%
  addLayersControl(baseGroups = c("OSM","Google",
                                  "Nat Geo World Map"), 
                   overlayGroups = c("SESYNC"),
                   options = layersControlOptions(collapsed = FALSE))


library(htmlwidgets)
library(webshot)

## save to a html file, to open with you browser
saveWidget(m, "Saenthong.html", selfcontained = FALSE)

