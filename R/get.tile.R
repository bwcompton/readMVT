'get.tile' <- function(zoom, lat, long) {

   # get.tile - given lat and long, find the correct Mapbox Vector Tile and return numeric row, col
   # Arguments:
   #     zoom        zoom level
   #     lat         latitude of target point
   #     long        longitude
   # Result:
   #     rowcol      two element vector with row and column in MVT
   # Translated from Python and JavaScript, thanks to
   #     https://stackoverflow.com/questions/29218920/how-to-find-out-map-tile-coordinates-from-latitude-and-longitude
   # B. Compton, 16 Jun 2023
   #' @export



   n <- 2 ^ zoom
   lat_rad <- pi * lat / 180
   row = floor((1 - asinh(tan(lat_rad)) / pi) / 2 * n)
   col <- floor(n * ((long + 180) / 360))
   return(c(row, col))
}
