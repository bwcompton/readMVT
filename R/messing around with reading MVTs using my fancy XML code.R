library(sf)
library(protolite)
protolite.patch()
library(leaflet)

xml <- readXML()
info <- layer.info(xml)


'getzoom' <- function(zoom = 10) {
   library(sf)
   library(protolite)
   protolite.patch()
   library(leaflet)

   xml <- readXML()
   info <- layer.info(xml)

   url <- sub('\\{zoom\\}', zoom, info$url)
   q <- info$tiles[info$tiles$zoom == zoom,]
   rc <- c(q$rowmin, q$colmin) + round((c(q$rowmax, q$colmax) - c(q$rowmin, q$colmin)) / 2, digits = 0)
   url <- sub('\\{TileRow\\}', rc[1], url)
   url <- sub('\\{TileCol\\}', rc[2], url)

   x <- read_mvt_sf(url, zxy = c(zoom, rc[2], rc[1]))
   x <- st_as_sf(x$streamlines)
   leaflet(x) |>
      addTiles() |>
      addPolylines()
}




########## THIS WORKS, READING FROM FIXED URL
url <- 'https://umassdsl.webgis1.com/geoserver/gwc/service/wmts/rest/testbed:streamlines/simple_streams/EPSG:900913/EPSG:900913:9/189/153?format=application/vnd.mapbox-vector-tile'
x <- read_mvt_sf(url, zxy = c(9, 153, 189))
x <- st_as_sf(x$streamlines)
leaflet(x) |>
   addTiles() |>
   addPolylines()

##########
