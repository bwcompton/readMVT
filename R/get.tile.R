'get.tile' <- function(info, zoom, long, lat) {

   # get.tile
   # given long and lat, find the correct MVT tile and return numeric row, col
   # Arguments:
   #     info        layer info for target layer, from layer.info
   #                 (consists of box, tiles, and url)
   #     zoom        zoom level
   #     long        longitude of target point
   #     lat         latitude   #
   # Result:
   #     rowcol      two element vector with row and column in MVT
   # B. Compton, 15 Jun 2023


   minmax <- info$tiles[info$tiles$zoom == zoom, ]
   ntiles <- minmax[c('rowmax', 'rowmin')] - minmax[c('colmax', 'colmin')]    # number of row and column tiles
   boxrange <- info$box[c(3, 4)] - info$box[c(1, 2)]                          # range of latitude, longitude
