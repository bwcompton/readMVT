# readMVT: R functions to read Mapbox Vector Tiles

<!-- badges: start -->
[![R-CMD-check](https://github.com/bwcompton/readMVT/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bwcompton/readMVT/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This package supports reading vector data (points, lines, polygons) served as [Mapbox Vector Tiles](https://docs.mapbox.com/data/tilesets/guides/vector-tiles-introduction/) (MVT) by a 
[GeoServer](https://geoserver.org/). It is designed to work with Leaflet under Shiny, but should
work fine in simpler situations. Vector data are returned as sf objects, so it's possible to 
process vector data in R and return attributes as popups within Leaflet.

readMVT includes functions to query data sources and capabilities on 
a GeoServer, to obtain metadata on specific featuress, to translate latitude-longitude to MVT
tile rows and columns, to read a tile, and to read all tiles in the current viewport.

## Installation

You can install the development version of readMVT from [GitHub](https://github.com/) with:

``` r
if(!require('remotes'))
  install.packages('remotes') 
remotes::install_github('bwcompton/readMVT')
```

## Example

For a detailed example, see `vignette('shiny-example')`.
