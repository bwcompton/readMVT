'read.tile' <- function(info, zoom, row, col) {

   # read.tile - read Mapbox Vector Tile for layer described by info, specified zoom, row, and colum
   # Arguments:
   #     info        layer info for target layer, from layer.info
   #     zoom        zoom level
   #     row         tile row
   #     col         tile column
   # Result:
   #     sf object
   # B. Compton, 15-16 Jun 2023



   url <- sub('\\{zoom\\}', zoom, info$url)
   url <- sub('\\{TileRow\\}', row, url)
   url <- sub('\\{TileCol\\}', col, url)

   z <- read_mvt_sf(url, zxy = c(zoom, col, row))
   if(length(z) == 0) return (NULL)
   return(z[[1]])
}
