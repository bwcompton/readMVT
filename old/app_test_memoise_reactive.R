# readMVT/app_test_memoise.R
# This version uses the memoise package for caching.
# It includes new attempts to avoid redrawing, using session$userData
# THIS WORKS!!
# B. Compton, 27 Jun 2023 (from app_test.R)



library(shiny)
library(leaflet)
library(memoise)
library('leaflet.lagniappe')
source('library.protolite.R')
source('layer.info.R')
source('read.XML.R')
source('get.tile.R')
source('read.tile.R')
library.protolite()


home <- c(-71.6995, 42.1349)
zoom <- 8       # starting zoom level (shows all of Massachusetts)
zoom2 <- 13     # all MVT tiles are read at this zoom level, no matter our current zoom level. This simplifies caching in Shiny
trigger <- 14   # zooms >= trigger show vector data
x <- NULL

read.tile.C <<- memoise(read.tile)

xml <- read.XML()
streamlines <- layer.info(xml, 'testbed:streamlines')
culverts <- layer.info(xml, 'testbed:CL_crossings7')
tiles <- streamlines$tiles[streamlines$tiles$zoom == zoom2,]


ui <- fluidPage(
   sidebarPanel(
      textOutput('dashboard')
   ),
   mainPanel(
      leafletOutput('map', height = '60vh')
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
      m <- leafletProxy('map', session)
      if(!is.null(input$map_zoom)) zoom <- input$map_zoom
      if(zoom < trigger)
         hideGroup(m, 'vector') # clear streams

      if(is.null(session$userData$drawn))
         session$userData$drawn <- matrix(0, tiles$rowmax - tiles$rowmin + 1, tiles$colmax - tiles$colmin + 1)

      longlat <- as.numeric(as.vector(input$map_center))
      bounds <- input$map_bounds
      output$dashboard <- renderText({
         paste0('Lat = ',format(round(longlat[2], 3), nsmall = 3),
                ', long = ', format(round(longlat[1], 3), nsmall = 3),
                ', zoom = ', zoom,
                if(zoom >= trigger) '  TRIGGERED!')
      })

      if(zoom >= trigger) {
         showGroup(m, 'vector') # clear streams
         nw <- get.tile(zoom2, bounds$north, bounds$west)
         se <- get.tile(zoom2, bounds$south, bounds$east)

         for(i in nw[1]:se[1])
            for(j in nw[2]:se[2]) {
               # cat('\nij', i, j, '...')
               if(!session$userData$drawn[i - tiles$rowmin + 1, j - tiles$colmin + 1]) {
                  session$userData$drawn[i - tiles$rowmin + 1, j - tiles$colmin + 1] <- 1

                  #  cat('reading...')
                  x <- read.tile.C(streamlines, zoom2, i, j)
                  y <- read.tile.C(culverts, zoom2, i, j)

                  #  cat('DRAWING!')
                  if(!is.null(x))
                     m <- addPolylines(m, data = x, group = 'vector', opacity = 0.4, color = 'cornflowerblue', weight = 3,
                                       popup = format(x$STREAMLINE))
                  if(!is.null(y))
                     m <- addCircleMarkers(m, data = y, group = 'vector', opacity = 1, color = 'orange', radius = 4,
                                           popup = format(y$rank))
               }
            }
      }
   })
}

shinyApp(ui, server)
