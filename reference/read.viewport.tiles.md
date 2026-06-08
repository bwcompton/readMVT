# Read all Mapbox Vector Tiles in viewport

Reads all Mapbox Vector Tiles that fall within the supplied viewport,
merging into a single sf object. This function is optimized for running
with Leaflet under Shiny with multiple users, avoiding re-fetching and
redrawing tiles.

## Usage

``` r
read.viewport.tiles(info, nw, se = nw, data.zoom = 14, drawn = NULL)
```

## Arguments

- info:

  a GeoServer MVT info object for target layer, from layer.info

- nw:

  northwestern corner tile in viewport, from get.tile

- se:

  southeastern corner tile (default: just read a single tile at nw)

- data.zoom:

  zoom level read data at

- drawn:

  from session\$userData, bit matrix of tiles we've already drawn or
  NULL for the first call. Omit this argument if you don't need to track
  whether tiles have been drawn

## Value

A two-element list:

1.  tiles an sf object with vector data from tiles, or NULL if no new
    data are available in viewport

2.  drawn updated drawn matrix

## Details

Reads all tiles within viewport, merging them into a single sf object.

This function is intended to be used with Leaflet under Shiny with
multiple users, though it is expected to work fine in other applications
that are probably less demanding. As multiple users under Shiny may
share an R session, data fetched by any user may be used by other users
without re-fetching. This is accomplished by caching with
[`memoise::memoise()`](https://memoise.r-lib.org/reference/memoise.html),
which will be initialized in this function if needed.

Additionally, Leaflet internally caches data that have already been
drawn, so it is undesirable for this function to return tiles that have
already been drawn when using with Leaflet. Tiles are only read and
returned if they haven't been drawn yet for this user. This behavior is
tracked with the drawn argument. After calling `read.viewport.tiles()`,
you should save the drawn result in the Shiny user data like this:

`x <- read.viewport.tiles(..., drawn = session$userData[[layername$layer]])`
`session$userData[[layername$layer]] <- x$drawn`

Note that saving drawn as a local variable will not preserve it across
repeated observe calls in Shiny, and saving it as a global variable will
interfere with other users.

If there are no new data available in the viewport, NULL will be
returned. You'll need to check for NULL before calling a leaflet drawing
function.

## Author

Bradley W. Compton <bcompton@umass.edu>
