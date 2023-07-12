'get.tile' <- function(zoom, lat, long) {

   #' Get Mapbox Vector Tile row and column
   #'
   #' Translates latitude and longitude at a given zoom level to row and column of a
   #' Mapbox Vector Tile.
   #'
   #' @param zoom       zoom level
   #' @param lat,long   latitude and longitude of target point
   #'
   #' @return
   #'     rowcol      two element vector with row and column in MVT
   #'
   #' @details
   #'
   #' Mapbox Vector Tiles use a common addressing scheme shared with other web mapping
   #' services, such as Open Street Map Slippy Tiles. Tiles numbers are the same for any
   #' data source, based only on zoom level, latitude, and longitude. This function returns
   #' the tile row and column.
   #' Translated from Python and JavaScript, thanks to
   #'     \url{https://stackoverflow.com/questions/29218920/how-to-find-out-map-tile-coordinates-from-latitude-and-longitude}
   #'
   #' @export
   #'
   #' @examples
   #' require(readMVT)
   #' xml <- read.XML('https://umassdsl.webgis1.com/geoserver')
   #' info <- layer.info(xml, 'testbed:streamlines')
   #' rc <- get.tile(10, 48.0096, -88.7712)
   #'
   # B. Compton, 16 Jun 2023



   n <- 2 ^ zoom
   lat_rad <- pi * lat / 180
   row = floor((1 - asinh(tan(lat_rad)) / pi) / 2 * n)
   col <- floor(n * ((long + 180) / 360))
   return(c(row, col))
}
