# Get capabilities XML from a GeoServer

This is a necessary first step before reading Mapbox Vector Tile data
from a GeoServer.

## Usage

``` r
read.XML(site)
```

## Arguments

- site:

  base GeoServer site name, ending with /geoserver

## Value

The raw XML capabilities file

## Details

Each GeoServer has an XML file that describes all available data in
excruciating detail. This function reads the XML from a GeoServer,
setting you up to pull the key information about Mapbox Vector tiles
using layer.info.

## Author

Bradley W. Compton <bcompton@umass.edu>

## Examples

``` r
require(readMVT)
xml <- read.XML('https://marsh01.ecs.umass.edu/geoserver')
```
