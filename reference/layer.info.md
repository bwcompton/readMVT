# Read info for a Mapbox Vector Tile layer on a GeoServer

Reads all necessary info for reading Mapbox Vector Tiles from a
GeoServer.

## Usage

``` r
layer.info(xml, layer, crs = "EPSG:900913", base.url = NULL)
```

## Arguments

- xml:

  XML from getcapabilities. Use
  [`read.XML()`](https://bwcompton.github.io/readMVT/reference/read.XML.md)
  to get this

- layer:

  name of the layer, e.g., DEPMEP:streams

- crs:

  coordinate reference system. For Mapbox Vector Tiles, you'll want
  EPSG:900913 (= EPSG:3857)

- base.url:

  optional override for the server base URL
  (scheme+host+port+/geoserver). When NULL (default), the base is
  extracted from the ResourceURL in the capabilities XML. Supply the URL
  passed to read.XML() when the server's proxy base URL is misconfigured
  (e.g., missing a non-standard port), so tile requests use the correct
  host and port rather than what the XML reports.

## Value

An MVT object, a four element list with:

1.  layer: name of layer (colon converted to underscore)

2.  box: bounding box (x-min, y-min, x-max, y-max)

3.  tiles: n x 5 matrix of zoom level, rowmin, rowmax, colmin, colmax

4.  url: TMS URL template; leaves `{zoom}`, `{TileRow}`, and `{TileCol}`
    to be replaced on reads

## Details

Pulls necessary information for reading Mapbox Vector Tiles from a
GeoServer's capabilities XML. Builds a template URL, with tags to be
replaced by read.tile, when the target zoom level, row, and column are
known.

## Author

Bradley W. Compton <bcompton@umass.edu>

## Examples

``` r
require(readMVT)
xml <- read.XML('https://marsh01.ecs.umass.edu/geoserver')
info <- layer.info(xml, 'DEPMEP:streams')
```
