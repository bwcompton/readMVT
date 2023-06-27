'get.tile' <- function(info, zoom, lat, long) {

 #######  THIS IS MY NAIVE ATTEMPT. It's superceded by a new version that uses Python code from the web.


   # get.tile
   # given long and lat, find the correct MVT tile and return numeric row, col
   # Arguments:
   #     info        layer info for target layer, from layer.info
   #                 (consists of box, tiles, and url)
   #     zoom        zoom level
   #     lat         latitude of target point
   #     long        longitude
   # Result:
   #     rowcol      two element vector with row and column in MVT
   # B. Compton, 15 Jun 2023


   # from Leaflet: Lat = 42.650, long = -72.314
   # lat = rows, long = cols    we are using rows/cols, lat/long
   # minmax is row/col
   # ntiles is row/col
   # info$box is long/lat *** backwards!!!!***
   # boxrange is lat/long
   # row/col origin is top left corner
   # lat/long origin is bottom right corner!

   # zoom <- 12; lat <- 42.64975450330437; long <- -72.3137352742557      # my house
   # zoom <- 12; lat <- 42.274; long <- -72.338                           # Swift River at Rt. 9
   # zoom <- 12; lat <- 42.101; long <- -73.354                # Barnard Cemetary, Sheffield
   # zoom <- 12; lat <- 41.813; long <- -70.907                  # Lakeville

   minmax <- info$tiles[info$tiles$zoom == zoom, ]
   ntiles <- minmax[c('rowmax', 'colmax')] - minmax[c('rowmin', 'colmin')]    # number of row and column tiles
   boxrange <- info$box[c(4, 3)] - info$box[c(2, 1)]                          # range of latitude, longitude

   z <- minmax[c('rowmax', 'colmax')] + c(-1, -1) * floor(ntiles * abs((c(lat, long) - info$box[c(2, 1)]) / boxrange - c(0, 1)) )
   # cat('Tile should be\n')
   # print(z)
   names(z) <- c('row', 'col')
   return(unlist(z))
}

# cat('My house is in tile', c(1510, 1225), '\n')       # my house
# cat('Swift River is in tile 1516 1224\n')
# cat('Sheffield is in tile 1518 1213\n')
# cat('Lakeville is in tile 1523 1241\n')
#
#
#
#
#
# my house: ROW is correct, COL is +13
# Swift: ROW is correct, COL is +14
# Sheffield: ROW is +1, COL is +37
# Lakeville: ROW is correct, COL is -19
#
# ROW is close. COL is backwards!
#
#    # degtile <- boxrange / ntiles
#    # c(lat, long) - info$box[c(1, 2)]
#
