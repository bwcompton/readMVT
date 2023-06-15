'read.tile' <- function(info, zoom, row, col) {


   # read.tile
   # Read Mapbox Vector Tile for layer described by info, and specified zoom, row, and colum
   # Arguments:
   #     info        layer info for target layer, from layer.info
   #                 (consists of box, tiles, and url)
   #     zoom        zoom level
   #     row         tile row
   #     col         tile column
   # Result:
   #     sf object
   # B. Compton, 15 Jun 2023



   url <- sub('\\{zoom\\}', zoom, info$url)
   q <- info$tiles[info$tiles$zoom == zoom,]
   url <- sub('\\{TileRow\\}', row, url)
   url <- sub('\\{TileCol\\}', col, url)

   z <- read_mvt_sf(url, zxy = c(zoom, col, row))
   #  z <- st_as_sf(z$streamlines)
  # z <- z[names(z)]
   z <- z[[1]]
   return(z)
}
