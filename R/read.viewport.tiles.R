'read.viewport.tiles' <- function(info, nw, se, data.zoom = 14, env = parent.frame()) {

   # read.viewport.tiles
   # Read all Mapbox Vector Tiles in viewport
   # Arguments:
   #     info        a GeoServer MVT info object, from layer.info
   #     nw          northwest tile in viewport, from get.tile
   #     se          southeast tile in viewport
   #     data.zoom   zoom level of data to read
   # Result:
   #     sf object with vector data from tiles, or NULL of no data available
   # Side-effects:
   #     a memoise cache is used to cache read tiles, shared among users
   #     session$userData[info$layer]  this Boolean matrix records displayed tiles for this layer
   # B. Compton, 29 Jun 2023

   # env$session$userData$streamlines <- 1:10
   # print(env$session$userData['streamlines'])
   # env$session$userData[['streamlines2']] <- matrix(1:15, 3, 5)
   # print(env$session$userData$streamlines2)
   # env$session$userData[[info$layer]] <- matrix(15:1, 3, 5)
   # print(env$session$userData[[info$layer]])
   #
   # return()


   cat('sub: ', info$layer, '\n')
   cat('sub dim: ', dim(env$session$userData[[info$layer]]), 'sum: ', sum(env$session$userData[[info$layer]]), '\n')




   tiles <- info$tiles[info$tiles$zoom == data.zoom,]
   # if(is.null(env$session$userData[[info$layer]]))
   #    env$session$userData[[info$layer]] <- matrix(FALSE, tiles$rowmax - tiles$rowmin + 1, tiles$colmax - tiles$colmin + 1)

   z <- NULL

   if(!any(c(nw > tiles[c('rowmax', 'colmax')], se < tiles[c('rowmin', 'colmin')]))) {
      nw <- pmax(nw, tiles[c('rowmin', 'colmin')])
      se <- pmin(se, tiles[c('rowmax', 'colmax')])

      #print(sum(env$session$userData[[info$layer]]))

      for(i in nw[1]:se[1])
         for(j in nw[2]:se[2]) {

            if(!env$session$userData[[info$layer]][i - tiles$rowmin + 1, j - tiles$colmin + 1]) {
               env$session$userData[[info$layer]][i - tiles$rowmin + 1, j - tiles$colmin + 1] <- TRUE

               x <- read.tile.C(info, data.zoom, i, j)
              # cat('reading & returning', info$layer, i, j, '\n')
               if(is.null(z)) z <- x else z <- rbind(z, x)
            }
         }
   }
   return(z)
}
