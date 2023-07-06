'read.XML' <- function(site) {

   #' Get capabilities XML from specified GeoServer
   #'
   #' This is a necessary first step before reading Mapbox Vector Tile data
   #' from a GeoServer
   #'
   #' @param site     base GeoServer site name, ending with /geoserver
   #' @return
   #' The raw XML capabilities file
   #'
   #' @details
   #' Each GeoServer has an XML file that describes all available data in
   #' excruciating detail. This function reads the XML from a GeoServer, setting
   #' you up to pull the key information about Mapbox Vector tiles using layer.info.
   #' @section Author:
   #' Bradley W. Compton <bcompton@@umass.edu>
   #' @export
   #' @importFrom httr GET content
   #' @import xml2
   #' @examples
   #' require readMVT
   #' xml <- read.XML('https://umassdsl.webgis1.com/geoserver')
   #'
   # B. Compton, 12 Jun 2023



   z <- GET(paste0(site, '/gwc/service/wmts?REQUEST=GetCapabilities'))
   z <- xml_ns_strip(read_xml(content(z, as = "text")))
   return(z)
}
