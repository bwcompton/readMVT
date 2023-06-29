'layer.info' <- function(xml, layer = 'testbed:streamlines', crs = 'EPSG:900913') {



   # layer.info - read info for a layer on a GeoServer
   # Arguments:
   #     xml      XML from getcapabilities. Use readXML to get this
   #     layer    name of the layer, e.g., testbed:streamlines
   #     crs      coordinate reference system. For Mapbox Vector Tiles, you'll want EPSG:900913
   # Results:
   #     1. layer: name of layer (colon converted to underscore)
   #     2. box: bounding box (x-min, y-min, x-max, y-max)
   #     3. tiles: a n x 3 matrix of zoom level, rows, cols
   #     4. url: constructed url, with {style} and {TileMatrixSet} replaced, and {TileMatrix} replaced with {zoom}
   #              leaves {zoom}, {TileRow}, and {TileCol} to be replaced on reads
   # B. Compton, 12-14 and 29 Jun 2023



   library(httr)
   library(xml2)

   # layer name
   l <- sub(':', '_', layer)

   # bounding box
   x <- xml_children((xml_find_first(xml, paste0("//Layer[ows:Identifier = '", layer, "']/ows:WGS84BoundingBox"))))
   box <- unlist(lapply(strsplit(xml_text(x), ' '), as.numeric))


   # get rows and cols for each zoom level
   x <- xml_find_all(xml, paste0('//Layer[ows:Identifier = \'', layer, '\']//TileMatrixSetLink[TileMatrixSet = \'', crs, '\']//TileMatrixLimits'))
   z <- data.frame(matrix(NA, length(x), 5))       # columns are Tile Matrix, min row, max row, min col, max col
   names(z) <- c('zoom', 'rowmin', 'rowmax', 'colmin', 'colmax')

   for (i in 1:length(x)) {
      z[i, 1] <- xml_text(xml_find_first(x[[i]], "./TileMatrix"))
      z[i, 2] <- xml_integer(xml_find_first(x[[i]], "./MinTileRow"))
      z[i, 3] <- xml_integer(xml_find_first(x[[i]], "./MaxTileRow"))
      z[i, 4] <- xml_integer(xml_find_first(x[[i]], "./MinTileCol"))
      z[i, 5] <- xml_integer(xml_find_first(x[[i]], "./MaxTileCol"))
   }
   z[, 1] <- as.numeric(sub(paste0(crs, ':'), '', z[, 1]))


   # Style
   style <- xml_text(xml_find_first(xml, paste0("//Layer[ows:Identifier = '", layer, "']/Style/ows:Identifier")))

   # url
   url <- trimws(noquote(xml_find_first(xml, paste0('concat(//Layer[ows:Identifier = \'', layer, '\']//ResourceURL[@format="application/vnd.mapbox-vector-tile"]/@template, \' \', string(@template))'))))
  ### url <- xml_find_first(xml, paste0('concat(//Layer[ows:Identifier = \'', layer, '\']//ResourceURL[@format="application/vnd.mapbox-vector-tile"]/@template, \' \', string(@template))'))

   # clean up url
   url <- sub('\\{style\\}', style, url)                             # insert style
   url <- sub('\\{TileMatrixSet\\}', crs, url)                       # insert CRS
   url <- sub('\\{TileMatrix\\}', paste0(crs, ':{zoom}'), url)       # insert CRS for zoom, leaving {zoom} for later replacement


   return(list(layer = l, box = box, tiles = z, url = url))
}
