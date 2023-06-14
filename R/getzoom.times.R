'getzoom.times' <- function(zoom = 10, r = 0, c = 0, info = NULL) {

   # getzoom - read and display MVT streams at specified zoom; picks the center tile with optional offset
   # Arguments:
   #     zoom     zoom level
   #     r        row offset
   #     c        column offset
   #     info     info from previous call to layer.info
   # B. Compton, 14 Jun 2023


   T <- t <- Sys.time()
   library(sf)
   cat(round(Sys.time() - t, 8), 'library(sf)\n'); t <- Sys.time()
   library(leaflet)
   cat(round(Sys.time() - t, 8), 'library(leaflet)\n'); t <- Sys.time()
   library(protolite)
   cat(round(Sys.time() - t, 8), 'library(protolite)\n')
   protolite.patch()

   t <- Sys.time()

   if(is.null(info)) {
      xml <- readXML()
      cat(round(Sys.time() - t, 8), 'readXML \n'); t <- Sys.time()
      info <- layer.info(xml, 'testbed:streamlines')
      cat(round(Sys.time() - t, 8), 'layer.info\n')
   }

   t <- Sys.time()


   url <- sub('\\{zoom\\}', zoom, info$url)
   q <- info$tiles[info$tiles$zoom == zoom,]
   rc <- c(q$rowmin, q$colmin) + round((c(q$rowmax, q$colmax) - c(q$rowmin, q$colmin)) / 2, digits = 0) + c(r, c)
   url <- sub('\\{TileRow\\}', rc[1], url)
   url <- sub('\\{TileCol\\}', rc[2], url)

   cat(round(Sys.time() - t, 8), 'prep\n'); t <- Sys.time()

   x <- read_mvt_sf(url, zxy = c(zoom, rc[2], rc[1]))
   x <- st_as_sf(x$streamlines)

   cat(round(Sys.time() - t, 8), 'read_mvt_sf\n'); t <- Sys.time()

   leaflet(x) |>
      addTiles() |>
      addPolylines() |>
      print()

   cat(round(Sys.time() - t, 8), 'leaflet\n')
   cat(round(Sys.time() - T, 8), 'total time\n')
}
