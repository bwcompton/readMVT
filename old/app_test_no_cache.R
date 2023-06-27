# readMVT/app_test_no_cache
# This version omits caching, so it reads data over and over as you pan. I've fixed it at zoom = 14
# Although it's not as snappy as my caching, there is clearly some caching happening. I don't know where.
# Shiny sessions
# B. Compton, 22 Jun 2023 (from app_test.R)



library(shiny)
library(leaflet)
library('leaflet.lagniappe')
source('G:/R/readMVT/R/getzoom.times.R')
source('G:/R/readMVT/R/getzoom.R')
source('G:/R/readMVT/R/library.protolite.R')
source('G:/R/readMVT/R/layer.info.R')
source('G:/R/readMVT/R/read.XML.R')
source('g:/r/readmvt/r/get.tile.R')
source('g:/r/readmvt/r/read.tile.R')
library.protolite()


home <- c(-71.6995, 42.1349)
zoom <- 8       # starting zoom level (shows all of Massachusetts)
zoom2 <- 14     # all MVT tiles are read at this zoom level, no matter our current zoom level. This simplifies caching in Shiny
trigger <- 14   # zooms >= trigger show vector data
x <- NULL


xml <- read.XML()
info <- layer.info(xml, 'testbed:streamlines')

q <- info$tiles[info$tiles$zoom == zoom2,]


ui <- fluidPage(
   sidebarPanel(
      textOutput("selected_var")
   ),
   mainPanel(
      leafletOutput("map", height = '60vh')
   )
)


server <- function(input, output, session) {
   output$map <- renderLeaflet({
      leaflet() |>
         addProviderTiles(providers$CartoDB.Voyager) |>
         setView(lng = home[1], lat = home[2], zoom = zoom) |>
         osmGeocoder(email = 'bcompton@umass.edu')
   })
   observe({
      if(!is.null(input$map_zoom)) zoom <- input$map_zoom
      longlat <- as.numeric(as.vector(input$map_center))
      bounds <- input$map_bounds
      output$selected_var <- renderText({
         paste0('Lat = ',format(round(longlat[2], 3), nsmall = 3),
                ', long = ', format(round(longlat[1], 3), nsmall = 3),
                ', zoom = ', zoom,
                if(zoom >= trigger) '  TRIGGERED!')
      })

      m <- leafletProxy('map', session)
      if(zoom < trigger) {
         hideGroup(m, 'vector') # clear streams
      }
      else {
         showGroup(m, 'vector') # clear streams
         nw <- get.tile(zoom2, bounds$north, bounds$west)
         se <- get.tile(zoom2, bounds$south, bounds$east)

         for(i in nw[1]:se[1])
            for(j in nw[2]:se[2]) {
               x <- read.tile(info, zoom2, i, j)
               cat('R')
               if(!is.null(x)) {
                  cat('r')
                  m <- addPolylines(m, data = x, group = 'vector', opacity = 1, popup = format(x$STREAMLINE))
               }
            }
      }
   })
}

shinyApp(ui, server)
