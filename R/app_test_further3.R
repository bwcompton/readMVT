# readMVT/app_test_further3.R
# calls read.viewport.tiles to read tiles, passing session$userData$layer in and back
# B. Compton, 29 Jun 2023 (from app_test_further2.R)



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

read.tile.C <<- memoise(read.tile)                          # set up read.tile to cache vector tiles (global to share among users)

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
             actionButton('help', label = icon('circle-info'), style = 'color: blue;
                           background-color: white; font-size:120%')
      ),
      column(10,
             leafletOutput('map', height = '80vh')
      ))
)


# Server -----------------------------
server <- function(input, output, session) {

   observeEvent(input$help, {
      showModal(modalDialog(
         help_text, title = 'Regional Connectivity',
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

      if(is.null(session$userData[[streamlines$layer]])) {                 # track which tiles have been drawn
         tiles <- streamlines$tiles[streamlines$tiles$zoom == data.zoom,]
         session$userData[[streamlines$layer]] <- matrix(FALSE, tiles$rowmax - tiles$rowmin + 1, tiles$colmax - tiles$colmin + 1)

         tiles <- culverts$tiles[culverts$tiles$zoom == data.zoom,]
         session$userData[[culverts$layer]] <- matrix(FALSE, tiles$rowmax - tiles$rowmin + 1, tiles$colmax - tiles$colmin + 1)
      }

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
         print(nw)
         m <- leafletProxy('map', session)

         x <- read.viewport.tiles(streamlines, nw, se, data.zoom, session$userData[[streamlines$layer]])
         session$userData[[streamlines$layer]] <- x$drawn
         if(!is.null(x$tiles))
            m <- addPolylines(m, data = x$tiles, group = 'vector', opacity = 0.4, color = 'cornflowerblue', weight = 3,
                              popup = format(x$tiles$STREAMLINE))


         x <- read.viewport.tiles(culverts, nw, se, data.zoom, session$userData[[culverts$layer]])
         session$userData[[culverts$layer]] <- x$drawn
         if(!is.null(x$tiles))
            m <- addCircleMarkers(m, data = x$tiles, group = 'vector', opacity = 1, color = 'orange', radius = 4,
                                  popup = format(x$tiles$rank))

         m <- groupOptions(m, 'vector', zoomLevels = zoom.levels)
      }
   })
}


shinyApp(ui, server)
