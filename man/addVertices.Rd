% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addVertices.R
\name{addVertices}
\alias{addVertices}
\title{addVertices}
\usage{
addVertices(sp, each = 0.1, parallel = FALSE, ...)
}
\arguments{
\item{sp}{an object of class "SpatialPolygons"}

\item{each}{the step value to use to create vertices}

\item{parallel}{run in parallel}

\item{...}{parallel options}
}
\value{
an object of class "SpatialPolygons"
}
\description{
Densify a spatial polygon by adding vertices
}
\author{
Emmanuel Blondel \email{emmanuel.blondel1@gmail.com}
}
