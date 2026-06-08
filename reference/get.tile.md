# Get Mapbox Vector Tile row and column

Translates latitude and longitude at a given zoom level to row and
column of a Mapbox Vector Tile.

## Usage

``` r
get.tile(zoom, lat, long)
```

## Arguments

- zoom:

  zoom level

- lat, long:

  latitude and longitude of target point

## Value

    rowcol      two element vector with row and column in MVT

## Details

Mapbox Vector Tiles use a common addressing scheme shared with other web
mapping services, such as Open Street Map Slippy Tiles. Tiles numbers
are the same for any data source, based only on zoom level, latitude,
and longitude. This function returns the tile row and column. Translated
from Python and JavaScript, thanks to
<https://stackoverflow.com/questions/29218920/how-to-find-out-map-tile-coordinates-from-latitude-and-longitude>

## Examples

``` r
require(readMVT)
xml <- read.XML('https://marsh01.ecs.umass.edu/geoserver')
info <- layer.info(xml, 'DEPMEP:streams')
rc <- get.tile(10, 48.0096, -88.7712)
```
