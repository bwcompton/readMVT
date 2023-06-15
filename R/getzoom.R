'getzoom' <- function(zoom = 10, r = 0, c = 0, info = NULL) {

   # getzoom - read and display MVT streams at specified zoom; picks the center tile with optional offset
   # Arguments:
   #     zoom     zoom level
   #     r        row offset
   #     c        column offset
   #     info     info from previous call to layer.info
   # B. Compton, 14 Jun 2023



   library(sf)
   library.protolite()
   library(leaflet)


   if(is.null(info)) {
      xml <- readXML()
      info <- layer.info(xml, 'testbed:streamlines')
   }

   url <- sub('\\{zoom\\}', zoom, info$url)
   q <- info$tiles[info$tiles$zoom == zoom,]
   rc <- c(q$rowmin, q$colmin) + round((c(q$rowmax, q$colmax) - c(q$rowmin, q$colmin)) / 2, digits = 0) + c(r, c)
   url <- sub('\\{TileRow\\}', rc[1], url)
   url <- sub('\\{TileCol\\}', rc[2], url)

   x <- read_mvt_sf(url, zxy = c(zoom, rc[2], rc[1]))
   x <- st_as_sf(x$streamlines)

   leaflet(x) |>
      addTiles() |>
      addPolylines()
}




########## THIS WORKS, READING FROM FIXED URL
#url <- 'https://umassdsl.webgis1.com/geoserver/gwc/service/wmts/rest/testbed:streamlines/simple_streams/EPSG:900913/EPSG:900913:9/189/153?format=application/vnd.mapbox-vector-tile'
#x <- read_mvt_sf(url, zxy = c(9, 153, 189))
#x <- st_as_sf(x$streamlines)
#leaflet(x) |>
#   addTiles() |>
#   addPolylines()
##########
