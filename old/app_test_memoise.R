# readMVT/app_test_memoise.R
# This version uses the memoise package for caching.
# It still does excess drawing in leaflet, so opacity must be 1
# To eliminate excess drawing I'd need to find a way to remember local variables or have session-specific variables
# B. Compton, 22 Jun 2023 (from app_test.R)



library(shiny)
library(leaflet)
library(memoise)
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

read.tile.C <<- memoise(read.tile)

xml <- read.XML()
streamlines <- layer.info(xml, 'testbed:streamlines')
culverts <- layer.info(xml, 'testbed:CL_crossings7')


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
      if(!is.null(input$map_zoom)) zoom <- input$map_zoom
      longlat <- as.numeric(as.vector(input$map_center))
      bounds <- input$map_bounds
      output$dashboard <- renderText({
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

        if(is.null(session$userData$one)) session$userData$one <- 0
          session$userData$one <- session$userData$one + 1
         print(session$userData$one)

         for(i in nw[1]:se[1])
            for(j in nw[2]:se[2]) {
               x <- read.tile.C(streamlines, zoom2, i, j)
               if(!is.null(x))
                  m <- addPolylines(m, data = x, group = 'vector', opacity = 1, color = 'cornflowerblue', weight = 3,
                                    popup = format(x$STREAMLINE))
               y <- read.tile.C(culverts, zoom2, i, j)
               if(!is.null(y))
                  m <- addCircleMarkers(m, data = y, group = 'vector', opacity = 1, color = 'orange', radius = 4,
                                        popup = format(y$rank))
            }
      }
   })
}

shinyApp(ui, server)
