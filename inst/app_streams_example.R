# readMVT/app_streams_example.R
# Minimal example for readMVT package
# To install readMVT:
#    remotes::install_github('bwcompton/readMVT')
# B. Compton, 29 Jun-10 Jul 2023 (from app_test_further2.R)



library(shiny)
library(leaflet)
library(readMVT)
library(memoise)

home <- c(-71.6995, 42.1349)  # center of Massachusetts
zoom <- 8                     # starting zoom level (shows all of Massachusetts)

data.zoom <- 14               # all MVT tiles are read at this zoom level to simplify caching
trigger <- 14                 # show vector data when zoomed in this far or more
zoom.levels = 14:22           # show vector data at these zoom levels


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
             )
      ),
      column(10,
             leafletOutput('map')
      ))
)


# Server -----------------------------
server <- function(input, output, session) {


   output$map <- renderLeaflet({
      leaflet() |>
         addProviderTiles(providers$Esri.WorldStreetMap) |>
         setView(lng = home[1], lat = home[2], zoom = zoom)
   })

   observe({
      output$dashboard <- renderUI({
         output$dashboard <- renderUI({
            tagList(div('zoom = ', input$map_zoom,),
                    div(strong(if(isTRUE(input$map_zoom >= trigger)) ' TRIGGERED!')))
         })
      })

      if(isTRUE(input$map_zoom >= trigger)) {                                       # if above trigger zoom, draw features
         nw <- get.tile(data.zoom, input$map_bounds$north, input$map_bounds$west)   # corners of viewport
         se <- get.tile(data.zoom, input$map_bounds$south, input$map_bounds$east)
         m <- leafletProxy('map', session)

         x <- read.viewport.tiles(streamlines, nw, se, data.zoom, session$userData[[streamlines$layer]])
         session$userData[[streamlines$layer]] <- x$drawn
         if(!is.null(x$tiles))
            m <- addPolylines(m, data = x$tiles, group = 'vector', opacity = 0.4, color = 'cornflowerblue', weight = 3)

         x <- read.viewport.tiles(culverts, nw, se, data.zoom, session$userData[[culverts$layer]])
         session$userData[[culverts$layer]] <- x$drawn
         if(!is.null(x$tiles))
            m <- addCircleMarkers(m, data = x$tiles, group = 'vector', opacity = 1, color = 'orange', radius = 4)

         groupOptions(m, 'vector', zoomLevels = zoom.levels)
      }
   })
}

shinyApp(ui, server)
