'layer.info' <- function(xml, layer = 'testbed:streamlines', crs = 'EPSG:900913') {



   # layer.info - read info for a layer on a GeoServer
   # Arguments:
   #     xml      XML from getcapabilities. Use readXML to get this
   #     layer    name of the layer, e.g., testbed:streamlines
   #     crs      coordinate reference system. For Mapbox Vector Tiles, you'll want EPSG:900913
   # Results:
   #     1. bounding box
   #     2. tiles: a n x 3 matrix of zoom level, rows, cols
   #     3. url: constructed url, with {style} and {TileMatrixSet} replaced, and {TileMatrix} replaced with {zoom}
   #              leaves {zoom}, {TileRow}, and {TileCol} to be replaced on reads
   # B. Compton, 12 Jun 2023



library(httr)
library(xml2)

need to get bounding box
need to get style
need to get rows and cols for each zoom level
need to get url
need to replace stuff in url


# bounding box
box <- xml_children((xml_find_all(xml, paste0("//Layer[ows:Identifier = '", layer, "']/ows:WGS84BoundingBox"))))
box <- ((xml_find_all(xml, paste0("//Layer[ows:Identifier = '", layer, "']/ows:WGS84BoundingBox/ows:LowerCorner"))))
box <- c(box, xml_text(xml_find_all(xml, paste0("//Layer[ows:Identifier = '", layer, "']/ows:WGS84BoundingBox/ows:UpperCorner"))))

# Style
style <- xml_text(xml_find_all(xml, paste0("//Layer[ows:Identifier = '", layer, "']/Style/ows:Identifier")))




xml_text(xml_find_all(xml, paste0("//Layer[ows:Identifier = '", layer, "']/TileMatrixSetLink/TileMatrixSetLimits/TileMatrixLimits")))


# Find the valid tile matrix set for the specified layer
tile_matrix_set <- xml_text(xml_find_all(xml, paste0("//Layer[ows:Identifier = '", layer, "']/TileMatrixSetLink/TileMatrixSetLimits/TileMatrixLimits")))
#tile_matrix_set <- xml_text(xml_find_first(xml, paste0("//ows:Identifier[text() = '", layer, "']/ancestor::Layer/TileMatrixSetLink/TileMatrixSetLimits")))
