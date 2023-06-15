'test' <- function(lat, long, info = inf, zoom = 12) {


   rc <- get.tile(info, zoom, lat, long)
   read.tile(info, 12, rc[1], rc[2]) |>
      leaflet() |>
      addTiles() |>
      addPolylines()
}
