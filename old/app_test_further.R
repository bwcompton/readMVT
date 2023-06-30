# readMVT/app_test_memoise.R
# This version uses the memoise package for caching.
# avoid redrawing; use groupOptions to hide at higher zoom levels, show (i) button for doc,
# don't crash off or at edges (but not right yet)
# B. Compton, 28-29 Jun 2023 (from app_test.R)



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

zoom2 <- 14     # all MVT tiles are read at this zoom level, no matter our current zoom level. This simplifies caching in Shiny
trigger <- 14   # zooms >= trigger show vector data

connect_text <- includeMarkdown('inst/connect_text.md')

read.tile.C <<- memoise(read.tile)

xml <- read.XML()
streamlines <- layer.info(xml, 'testbed:streamlines')
culverts <- layer.info(xml, 'testbed:CL_crossings7')
tiles <- streamlines$tiles[streamlines$tiles$zoom == zoom2,]


   #         streamlines<<-streamlines


ui <- fluidPage(
   fluidRow(
      column(2,
             wellPanel(
                uiOutput('dashboard')
             ),
             actionButton('connect_info', label = icon('circle-info'), style = 'color: blue;
                           background-color: white; font-size:120%')
             ),
      column(10,
             leafletOutput('map', height = '80vh')
      ))
)


server <- function(input, output, session) {

   observeEvent(input$connect_info, {
      observeEvent(input$connect_info, {
         showModal(modalDialog(
            connect_text, title = 'Regional Connectivity',
            easyClose = TRUE, fade = TRUE, footer = modalButton('OK')
         ))
      })
   })

   output$map <- renderLeaflet({
      leaflet() |>
         #addProviderTiles(providers$CartoDB.Voyager) |>
         addTiles(urlTemplate = '', attribution = '<a href="https://www.mass.gov/orgs/massachusetts-department-of-environmental-protection">Mass DEP | </a><a href="https://umassdsl.org">UMass DSL</a>') |>
         addProviderTiles(providers$Esri.WorldStreetMap) |>
         setView(lng = home[1], lat = home[2], zoom = zoom) |>
         osmGeocoder(email = 'bcompton@umass.edu')
   })
   observe({
      if(is.null(session$userData$drawn))
         session$userData$drawn <- matrix(FALSE, tiles$rowmax - tiles$rowmin + 1, tiles$colmax - tiles$colmin + 1)
      # so up here, we need to use both sets of rowmax, etc. maybe do that in a function that sets $drawn and userData$offset
      # oh! can set up inside function!

      longlat <- as.numeric(as.vector(input$map_center))
      bounds <- input$map_bounds
      output$dashboard <- renderUI({
         tagList(div(paste0('lat = ',format(round(longlat[2], 3), nsmall = 3))),
                 div('long = ', format(round(longlat[1], 3), nsmall = 3)),
                 div('zoom = ', input$map_zoom,),
                 div(strong(if(isTRUE(input$map_zoom >= trigger)) ' TRIGGERED!')))
      })

      if(isTRUE(input$map_zoom >= trigger)) {
         nw <- get.tile(zoom2, bounds$north, bounds$west)   # corners of viewport
         se <- get.tile(zoom2, bounds$south, bounds$east)

         ###########################################
         # x <- function(nw, se, streamlines, zoom2) - returns data for addXXX call; NULL if nothing
         # side effect is setting session$userData$drawn
         # but the fucking thing won't agree, so need separate $drawn variables
         # or else take the max of our tilesets above when setting it, and pass in offsets

         cat(nw, se, '/', unlist(tiles)[-1], '\n')
         cat('test: ', nw > tiles[c('rowmax', 'colmax')], se < tiles[c('rowmin', 'colmin')], '\n')

         if(!any(c(nw > tiles[c('rowmax', 'colmax')], se < tiles[c('rowmin', 'colmin')]))) {
            cat('before: ', nw, se, '\n')
            nw <- pmax(nw, tiles[c('rowmin', 'colmin')])
            se <- pmin(se, tiles[c('rowmax', 'colmax')])
            cat('after: ', nw, se, '\n')
            for(i in nw[1]:se[1])
               for(j in nw[2]:se[2]) {
                  # cat('\nij', i, j, '...')
                  # CHECK FOR OUT OF BOUNDS!!!!!!!!!!!!!
                  if(!session$userData$drawn[i - tiles$rowmin + 1, j - tiles$colmin + 1]) {
                     session$userData$drawn[i - tiles$rowmin + 1, j - tiles$colmin + 1] <- TRUE

                     cat('reading...')
                     cat('i, j = ', i, j, '\n')
                     x <- read.tile.C(streamlines, zoom2, i, j)
                  #   cat('**** read.tile(streamlines, 14, ', i, ',', j, ')\n')
                     y <- read.tile.C(culverts, zoom2, i, j)

                     #  cat('DRAWING!')
                     ###########################################
                     m <- leafletProxy('map', session)
                     if(!is.null(x))
                        m <- addPolylines(m, data = x, group = 'vector', opacity = 0.4, color = 'cornflowerblue', weight = 3,
                                          popup = format(x$STREAMLINE))
                     if(!is.null(y))
                        m <- addCircleMarkers(m, data = y, group = 'vector', opacity = 1, color = 'orange', radius = 4,
                                              popup = format(y$rank))

                     m <- groupOptions(m, 'vector', zoomLevels = 14:20)
                  }
               }
         }
      }
   })
}

shinyApp(ui, server)
