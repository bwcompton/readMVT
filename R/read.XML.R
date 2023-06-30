'read.XML' <- function(site = 'https://umassdsl.webgis1.com/geoserver') {

   # read.XML - get capabilities XML from specified GeoServer
   # Arguments:
   #     site = base site name, through /geoserver
   # Result:
   #     the raw XML file. Process this with layer.info
   # B. Compton, 12 Jun 2023
   #' @export
   #' @importFrom httr GET content
   #' @import xml2


#   library(httr)
#   library(xml2)

   z <- GET(paste0(site, '/gwc/service/wmts?REQUEST=GetCapabilities'))
   z <- xml_ns_strip(read_xml(content(z, as = "text")))
   return(z)
}
