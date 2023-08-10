# test.rbinds
# rbind in loop                        0.22 s
# make a list and lapply(zl, rbind)    0.11-0.12 s
# do.call('rbind', zl)                 0.09-0.11 s <- use this, I guess
# rbind.data.frame                     fails
# rbindlist                            fails
# B. Compton, 8 Aug 2023



library(readMVT)
source('r/read.viewport.tiles2.R')


xml <- read.XML('https://umassdsl.webgis1.com/geoserver')   # get capabilities of our GeoServer
streamlines <- layer.info(xml, 'testbed:streamlines')       # get info for stream linework

nw <- c(6055, 4880)
se <- c(6057, 4883)

system.time(x <- read.viewport.tiles2(streamlines, nw, se, 14))
dim(x$tiles)
