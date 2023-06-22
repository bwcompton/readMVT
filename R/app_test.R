# readMVT/app.test
# messing around with loading MVT data on hitting a particular zoom level trigger
# This works, but it's assigning global variables, which supposedly will conflict with other
# Shiny sessions
# B. Compton, 16-20 Jun 2023 (from getzoom-2)



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


home <- c(-71.6995, 42.1349)
zoom <- 8       # starting zoom level (shows all of Massachusetts)
zoom2 <- 13     # all MVT tiles are read at this zoom level, no matter our current zoom level. This simplifies caching in Shiny
trigger <- 13   # zooms >= trigger show vector data
x <- NULL


xml <- read.XML()
info <- layer.info(xml, 'testbed:streamlines')

q <- info$tiles[info$tiles$zoom == zoom2,]
cached <- matrix(0, q$rowmax - q$rowmin + 1, q$colmax - q$colmin + 1)    # status of cached tiles (0 = not cached, 2 = cached, 3 = cached and rendered)
rownames(cached) <- as.character(q$rowmin:q$rowmax)
colnames(cached) <- as.character(q$colmin:q$colmax)
stream.cache <-  stream.cache <- as.list(rep.int(0, length(cached)))     # stores stream tiles cached in Shiny
dim(stream.cache) <- dim(cached)
rownames(stream.cache) <- rownames(cached)
colnames(stream.cache) <- colnames(cached)
culvert.cahce <- stream.cache                                                  # cached culverts, and so on
zoomed <- FALSE


ui <- fluidPage(
   sidebarPanel(
      textOutput("selected_var"),
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
         if(zoomed) {
            hideGroup(m, 'vector') # clear streams
            zoomed <<- FALSE
         }
      }
      else {
         zoomed <<- TRUE
         showGroup(m, 'vector') # clear streams
         nw <- get.tile(zoom2, bounds$north, bounds$west)
         se <- get.tile(zoom2, bounds$south, bounds$east)

         for(i in nw[1]:se[1])
            for(j in nw[2]:se[2]) {
               if(cached[as.character(i), as.character(j)] == 0) {
                  x <- read.tile(info, zoom2, i, j)
                  cat('R')
                  stream.cache[[as.character(i), as.character(j)]] <<- x
               }
               if(cached[as.character(i), as.character(j)] <= 1) {
                  cached[as.character(i), as.character(j)] <<- 2             # this may bleed into other users in production version - figure out scoping
                  if(!is.null(x)) {
                     cat('r')
                     m <- addPolylines(m, data = x, group = 'vector', opacity = 0.2, popup = format(x$STREAMLINE))
                     m
                  }
               }
            }
      }
   })
}

shinyApp(ui, server)
