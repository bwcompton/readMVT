'read.tile' <- function(info, zoom, row, col) {

   #' Read Mapbox Vector Tile from a GeoServer
   #'
   #' Read Mapbox Vector Tile for layer described by info, specified zoom, row, and column
   #' from a GeoServer and return as an sf object.
   #'
   #' @param info    layer info for target layer, from [layer.info()]
   #' @param zoom    zoom level
   #' @param row,col tile row and column (from [get.tile()])
   #'
   #' @return
   #' sf object
   #'
   #' @details
   #' This uses the URL in the MVT object created by layer.info to produce a URL for
   #' the specified row and column at the specified zoom level to fetch the MVT data
   #' from a GeoServer and return the data as an sf object.
   #'
   #' Note that this functions requires protolite >= 2.2.1. If it's not yet up on CRAN,
   #' you can get it with
   #'
   #'    \code{remotes::install_github('jeroen/protolite')}
   #'
   #' @section Author:
   #' Bradley W. Compton <bcompton@@umass.edu>
   #'
   #' @export
   #' @import protolite
   #' @import sf
   #'
   #' @examples
   #' require(readMVT)
   #' require(leaflet)
   #' xml <- read.XML('https://umassdsl.webgis1.com/geoserver')
   #' info <- layer.info(xml, 'testbed:streamlines')
   #' rc <- get.tile(12, 42.394, -72.5312)
   #' x <- read.tile(info, 12, rc[1], rc[2])
   #' leaflet(x) |> addTiles() |> addPolylines(data = x)
   #'
   # B. Compton, 15-16 Jun 2023



   url <- sub('\\{zoom\\}', zoom, info$url)
   url <- sub('\\{TileRow\\}', row, url)
   url <- sub('\\{TileCol\\}', col, url)

   z <- read_mvt_sf(url, zxy = c(zoom, col, row))
   if(length(z) == 0) return (NULL)
   return(z[[1]])
}
