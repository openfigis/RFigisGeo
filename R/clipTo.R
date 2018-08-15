#' 
#' @name clipToExtent
#' @title clipToExtent
#' @description Clips a spatial object given a geographic extent
#'
#' @param sp an object of class "Spatial"
#' @param bb an object of class "matrix"
#' @return an object clipped by the extent specified
#'
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' 
clipToExtent <- function(sp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  proj4string(b_poly) <- CRS(proj4string(sp))
  return(RFigisGeo::intersection(sp, b_poly))
}

#' 
#' @name clipToGlobalExtent
#' @title clipToGlobalExtent
#' @description Clips a spatial object given the global lon/lat extent
#'
#' @param sp an object of class "Spatial"
#' @return an object clipped by the extent specified
#'
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' 
clipToGlobalExtent <- function(sp){
  bb <- matrix(c(-180,180,-90,90), nrow = 2, ncol = 2, byrow = TRUE,
               dimnames = list(c("x","y"), c("min","max")))
  return(RFigisGeo::clipToExtent(sp, bb))
}