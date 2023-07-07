'read.viewport.tiles' <- function(info, nw, se,
                                  data.zoom = 14, drawn = NULL) {

   #' Read all Mapbox Vector Tiles in viewport
   #'
   #' Reads all Mapbox Vector Tiles that fall within the supplied viewport, merging
   #' into a single sf object. This function is optimized for running with Leaflet
   #' under Shiny with multiple users, avoiding re-fetching and redrawing tiles.
   #'
   #' @param info       a GeoServer MVT info object for target layer, from layer.info
   #' @param nw         northwestern corner tile in viewport, from get.tile
   #' @param se         southeastern corner tile
   #' @param data.zoom  zoom level read data at
   #' @param drawn      from session$userData, bit matrix of tiles we've already drawn
   #'                   or NULL for the first call. Omit this argument if you don't
   #'                   need to track whether tiles have been drawn
   #'
   #' @return
   #' A two-element list:
   #'     \enumerate{
   #'     \item tiles    an sf object with vector data from tiles, or NULL if no new data
   #'                    are available in viewport
   #'     \item drawn    updated drawn matrix
   #'     }
   #'
   #' @details
   #' Reads all tiles within viewport, merging them into a single sf object.
   #'
   #' This function is intended to be used with Leaflet under Shiny with multiple users, though it
   #' is expected to work fine in other applications that are probably less demanding. As multiple
   #' users under Shiny may share an R session, data fetched by any user may be used by other users
   #' without re-fetching. This is accomplished by caching with the memoise package. In order to
   #' enable caching, the calling function must execute
   #'
   #'    \code{read.tile.C <<- memoise(read.tile)}
   #'
   #' The global assignment allows cached tiles to be shared among users. If read.tile.C hasn't
   #' been executed, tile reads will not be cached.
   #'
   #' Additionally, Leaflet internally caches data that have already been drawn, so it is
   #' undesirable for this function to return tiles that have already been drawn when using with
   #' Leaflet. Tiles are only read and returned if they haven't been drawn yet for this user.
   #' This behavior is tracked with the drawn argument. After calling this function, you should
   #' save the drawn result in the Shiny user data like this:
   #'
   #'    \code{x <- read.viewport.tiles(..., drawn = session$userData[[layername$layer]])}
   #'    \code{session$userData[[layername$layer]] <- x$drawn}
   #'
   #' Note that saving drawn as a local variable will not preserve it across repeated observe
   #' calls in Shiny, and saving it as a global variable will interfere with other users.
   #'
   #' If there are no new data available in the viewport, NULL will be returned. You'll need to
   #' check for NULL before calling a leaflet drawing function.
   #'
   #' @section Author:
   #' Bradley W. Compton <bcompton@@umass.edu>
   #'
   #' @export
   #' @import sf
   #'
   # B. Compton, 29 Jun-7 Jul 2023



   z <- NULL

   tiles <- info$tiles[info$tiles$zoom == data.zoom,]       # full tile range of data

   if(is.null(drawn))                                       # if empty drawn, set it up for initial call
      drawn <- matrix(FALSE, tiles$rowmax - tiles$rowmin + 1, tiles$colmax - tiles$colmin + 1)

   if(!exists('read.tile.C'))                               # if memoise hasn't been set up, don't worry about caching
      read.tile.C <- read.tile

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
