library(leaflet)
library(RJSONIO)
library(githubinstall)
#githubinstall("rCharts")
library(rCharts)
library(rjson)
library(readr)

json <- read_file("continents.txt")
continents <- RJSONIO::fromJSON(json)

mymap <- Leaflet$new()

mymap$tileLayer(provider = 'Stamen.TonerLite')
mymap$setView(c(-37, 145), zoom = 1.2)
mymap$geoJson(continents)

mymap

renderLeaflet({
    
    leaflet() %>%
        setView(lng = 0, lat = 0, zoom = 2) %>%
        addTiles() %>%
        addTiles(options = tileOptions(useCache = FALSE)) %>%
        addGeoJSON(geojson = json)
})
