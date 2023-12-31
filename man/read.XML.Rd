% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.XML.R
\name{read.XML}
\alias{read.XML}
\title{Get capabilities XML from a GeoServer}
\usage{
read.XML(site)
}
\arguments{
\item{site}{base GeoServer site name, ending with /geoserver}
}
\value{
The raw XML capabilities file
}
\description{
This is a necessary first step before reading Mapbox Vector Tile data
from a GeoServer.
}
\details{
Each GeoServer has an XML file that describes all available data in
excruciating detail. This function reads the XML from a GeoServer, setting
you up to pull the key information about Mapbox Vector tiles using layer.info.
}
\section{Author}{

Bradley W. Compton \href{mailto:bcompton@umass.edu}{bcompton@umass.edu}
}

\examples{
require(readMVT)
xml <- read.XML('https://umassdsl.webgis1.com/geoserver')

}
