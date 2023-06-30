'read.viewport.tiles' <- function(info, nw, se, data.zoom = 14, drawn) {

   # read.viewport.tiles -read all Mapbox Vector Tiles in viewport
   # Arguments:
   #     info        a GeoServer MVT info object for target layer, from layer.info
   #     nw          northwestern corner tile in viewport, from get.tile
   #     se          southeastern corner tile
   #     data.zoom   zoom level read data at
   #     drawn       from session$userData, bit matrix of tiles we've already drawn
   # Result (2 element list):
   #     1. tiles    an sf object with vector data from tiles, or NULL of no data available
   #     2. drawn    updated drawn matrix
   # Side-effects:
   #     a memoise cache is used to cache read tiles, shared among users
   #     Note: calling function must execute read.tile.C <<- memoise(read.tile)  # global to share among users
   # B. Compton, 29-30 Jun 2023
   #' @export




   z <- NULL

   tiles <- info$tiles[info$tiles$zoom == data.zoom,]       # full tile range of data
   if(!any(c(nw > tiles[c('rowmax', 'colmax')], se < tiles[c('rowmin', 'colmin')]))) {    # if viewport overlaps data,
      nw <- pmax(nw, tiles[c('rowmin', 'colmin')])          # clip viewport to data
      se <- pmin(se, tiles[c('rowmax', 'colmax')])

      for(i in nw[1]:se[1])                                 # for each tile,
         for(j in nw[2]:se[2]) {

            if(!drawn[i - tiles$rowmin + 1, j - tiles$colmin + 1]) {
               drawn[i - tiles$rowmin + 1, j - tiles$colmin + 1] <- TRUE
               x <- read.tile.C(info, data.zoom, i, j)
               if(is.null(z)) z <- x else z <- rbind(z, x)
            }
         }
   }
   return(list(tiles = z, drawn = drawn))
}
