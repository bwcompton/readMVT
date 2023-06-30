# readMVT/app_test_further2.R
# This version uses the memoise package for caching.
# avoid redrawing; use groupOptions to hide at higher zoom levels, show (i) button for doc,
# don't crash off or at edges (but not right yet)
# Here, I'm refactoring to push get.tile and read.tile into a subroutine. This is needed for vector
# data with windows that don't coincide.
# B. Compton, 29 Jun 2023 (from app_test_further.R)



library(shiny)
library(leaflet)
library(memoise)
library('leaflet.lagniappe')
source('library.protolite.R')
source('layer.info.R')
source('read.XML.R')
source('get.tile.R')
source('read.tile.R')
source('read.viewport.tiles.R')
library.protolite()


home <- c(-71.6995, 42.1349)  # center of Massachusetts
zoom <- 8                     # starting zoom level (shows all of Massachusetts)

data.zoom <- 14               # all MVT tiles are read at this zoom level to simplify caching
trigger <- 14                 # show vector data when zoomed in this far or more
zoom.levels = 14:22           # show vector data at these zoom levels

help_text <- includeMarkdown('inst/connect_text.md')        # markdown file with primary help text, including links to more help text

read.tile.C <<- memoise(read.tile)                          # set up read.tile to cache vector tiles (shared among users)

xml <- read.XML('https://umassdsl.webgis1.com/geoserver')   # get capabilties of our GeoServer
streamlines <- layer.info(xml, 'testbed:streamlines')       # get info for stream linework
culverts <- layer.info(xml, 'testbed:CL_crossings7')        # get info for crossing points






# User interface ---------------------
ui <- fluidPage(
   fluidRow(
      column(2,
             wellPanel(
                uiOutput('dashboard')
             ),
             actionButton('help_text', label = icon('circle-info'), style = 'color: blue;
                           background-color: white; font-size:120%')
      ),
      column(10,
             leafletOutput('map', height = '80vh')
      ))
)


# Server -----------------------------
server <- function(input, output, session) {

   observeEvent(input$connect_info, {
      showModal(modalDialog(
         connect_text, title = 'Regional Connectivity',
         easyClose = TRUE, fade = TRUE, footer = modalButton('OK')
      ))
   })

   output$map <- renderLeaflet({
      leaflet() |>
         addTiles(urlTemplate = '', attribution = '<a href="https://www.mass.gov/orgs/massachusetts-department-of-environmental-protection">Mass DEP | </a><a href="https://umassdsl.org">UMass DSL</a>') |>
         addProviderTiles(providers$Esri.WorldStreetMap) |>
         setView(lng = home[1], lat = home[2], zoom = zoom) |>
         osmGeocoder(email = 'bcompton@umass.edu')
   })

   observe({

      if(is.null(session$userData[[streamlines$layer]])) {           # this is fucked up. It fails if I set it in read.viewport.tiles
         tiles <- streamlines$tiles[streamlines$tiles$zoom == data.zoom,]
         session$userData[[streamlines$layer]] <- matrix(FALSE, tiles$rowmax - tiles$rowmin + 1, tiles$colmax - tiles$colmin + 1)
         session$userData[[streamlines$layer]][1,1] <- TRUE         # this makes no sense, but fails without

         tiles <- culverts$tiles[culverts$tiles$zoom == data.zoom,]
         session$userData[[culverts$layer]] <- matrix(FALSE, tiles$rowmax - tiles$rowmin + 1, tiles$colmax - tiles$colmin + 1)
         session$userData[[culverts$layer]][1,1] <- FALSE
      }


      # ############
      # tiles <- streamlines$tiles[streamlines$tiles$zoom == data.zoom,]
      # if(is.null(session$userData[[streamlines$layer]])) {
      #    cat('setting!\n')
      #    session$userData[[streamlines$layer]] <- matrix(FALSE, tiles$rowmax - tiles$rowmin + 1, tiles$colmax - tiles$colmin + 1)
      # }
      # session$userData[[streamlines$layer]][1,1] <- FALSE
      #
      # ###########
      #
       cat('caller: ', streamlines$layer, '\n')
       cat('caller dim: ', dim(session$userData[[streamlines$layer]]), 'sum: ', sum(session$userData[[streamlines$layer]]), '\n')
      # cat('caller2 dim: ', dim(session$userData[[streamlines$layer]]), 'sum: ', sum(session$userData[[streamlines$layer]]), '\n')
      #
      #   print(session$userData[[streamlines$layer]])

      output$dashboard <- renderUI({
         longlat <- as.numeric(as.vector(input$map_center))
         output$dashboard <- renderUI({
            tagList(div(paste0('lat = ',format(round(longlat[2], 3), nsmall = 3))),
                    div('long = ', format(round(longlat[1], 3), nsmall = 3)),
                    div('zoom = ', input$map_zoom,),
                    div(strong(if(isTRUE(input$map_zoom >= trigger)) ' TRIGGERED!')))
         })
      })

      if(isTRUE(input$map_zoom >= trigger)) {
         nw <- get.tile(data.zoom, input$map_bounds$north, input$map_bounds$west)   # corners of viewport
         se <- get.tile(data.zoom, input$map_bounds$south, input$map_bounds$east)

         m <- leafletProxy('map', session)

         x <- read.viewport.tiles(streamlines, nw, se, data.zoom)
         cat('--- sum = ', sum(session$userData[[streamlines$layer]]), '---\n')
         if(!is.null(x))
            m <- addPolylines(m, data = x, group = 'vector', opacity = 0.4, color = 'cornflowerblue', weight = 3,
                              popup = format(x$STREAMLINE))

         y <- read.viewport.tiles(culverts, nw, se, data.zoom)
         if(!is.null(y))
            m <- addCircleMarkers(m, data = y, group = 'vector', opacity = 1, color = 'orange', radius = 4,
                                  popup = format(y$rank))
         m <- groupOptions(m, 'vector', zoomLevels = zoom.levels)
      }
   })
}


shinyApp(ui, server)
