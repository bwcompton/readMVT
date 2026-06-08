# Read Mapbox Vector Tile from a GeoServer

Read Mapbox Vector Tile for layer described by info, specified zoom,
row, and column from a GeoServer and return as an sf object.

## Usage

``` r
read.tile(info, zoom, row, col)
```

## Arguments

- info:

  layer info for target layer, from
  [`layer.info()`](https://bwcompton.github.io/readMVT/reference/layer.info.md)

- zoom:

  zoom level

- row, col:

  tile row and column (from
  [`get.tile()`](https://bwcompton.github.io/readMVT/reference/get.tile.md))

## Value

sf object

## Details

This uses the URL in the MVT object created by layer.info to produce a
URL for the specified row and column at the specified zoom level to
fetch the MVT data from a GeoServer and return the data as an sf object.

Note that this functions requires protolite \>= 2.2.1. If it's not yet
up on CRAN, you can get it with

`remotes::install_github('jeroen/protolite')`

## Author

Bradley W. Compton <bcompton@umass.edu>

## Examples

``` r
require(readMVT)
require(leaflet)
#> Loading required package: leaflet
xml <- read.XML('https://marsh01.ecs.umass.edu/geoserver')
info <- layer.info(xml, 'DEPMEP:streams')
rc <- get.tile(12, 42.394, -72.5312)
x <- read.tile(info, 12, rc[1], rc[2])
#> Error in curl::curl_fetch_memory(data, handle = curl::new_handle(failonerror = TRUE)): HTTP response code said error [marsh01.ecs.umass.edu]:
#> The requested URL returned error: 404
leaflet(x) |> addTiles() |> addPolylines(data = x)
#> Warning: restarting interrupted promise evaluation
#> Warning: restarting interrupted promise evaluation
#> Error: object 'x' not found
```
