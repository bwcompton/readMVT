# getzoom-3
# messing around with loading MVT data on hitting a particular zoom levelrigger level
# B. Compton, 16 Jun 2023 (from getzoom-2)



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
dim(stream.cache) <- c(dim(cache))
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
      print(bounds)
      output$selected_var <- renderText({
         paste0('Lat = ',format(round(longlat[2], 3), nsmall = 3),
                ', long = ', format(round(longlat[1], 3), nsmall = 3),
                ', zoom = ', zoom,
                if(zoom >= trigger) '  TRIGGERED!')
      })

      rect <- c(longlat[1] - 0.1, longlat[2] - 0.07, longlat[1] + 0.1, longlat[2] + 0.07)

      m <<- leafletProxy('map', session)
      ####      if(zoom >= trigger) {
      if(zoom < trigger) {
         print('* zoom < trigger *')
         print(zoomed)
         if(zoomed) {
            hideGroup(m, 'vector') # clear streams
            print('*** Trying to hide ***')
            zoomed <<- FALSE
         }
      }
      else {
         zoomed <<- TRUE
         showGroup(m, 'vector') # clear streams
         m <<- addMarkers(m, lng = longlat[1], lat = longlat[2], popup = paste0('zoom = ', zoom))
         # rc <- get.tile(zoom, longlat[2], longlat[1])
         # x <- read.tile(info, zoom, rc[1], rc[2])
         # m <- addPolylines(m, data = x)
         # m
         print(bounds)
         nw <- get.tile(zoom2, bounds$north, bounds$west)
         se <- get.tile(zoom2, bounds$south, bounds$east)
         cat('/nzoom =', zoom, '  nw =', nw, '  se =', se, '\n')
         cat('/n')

         for(i in nw[1]:se[1])
            for(j in nw[2]:se[2]) {

               if(cached[as.character(i), as.character(j)] == 0) {
                  x <- read.tile(info, zoom2, i, j)
                  cat('R')
                  stream.cache[[as.character(i), as.character(j)]] <<- x
               }
               if(cached[as.character(i), as.character(j)] <= 1) {
                  cached[as.character(i), as.character(j)] <<- 2             # this may bleed into other users in production version - figure out scoping
                  # print(sum(cached))
                  if(!is.null(x)) {
                     cat('r')
                     #o <- ifelse(zoom > 13, 0.6, 0.1)
                     m <<- addPolylines(m, data = x, group = 'vector', opacity = 0.2, popup = format(x$STREAMLINE))
                     m
                  }
               }
               # if(is.null(x))
               #    x <- read.tile(info, zoom, i, j)
               # else
               #    x <- rbind(x, read.tile(info, zoom, i, j))
            }
         #   m <- addPolylines(m, data = x)
         #print(nw)
         #print(se)
         #print('---')
         #print(rc)
         #    x <- read.tile(info, zoom, rc[1], rc[2])
         #       m <- addPolylines(m, data = x)
         #print(input$map_bounds)
         #m <- addRectangles(m, rect[1], rect[2], rect[3], rect[4], color = 'green', fillOpacity = 0.1)
      }
      # m
   })
}

shinyApp(ui, server)
