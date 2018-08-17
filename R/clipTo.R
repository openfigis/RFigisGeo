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
  
  if(!is(bb, "matrix")) stop("'bb' argument should be a 'matrix' object")
  
  #first check bb
  sp.bbox <- bbox(sp)
  if(bb[1,1] < sp.bbox[1,1]) bb[1,1] <- sp.bbox[1,1]
  if(bb[2,1] < sp.bbox[2,1]) bb[2,1] <- sp.bbox[2,1]
  if(bb[1,2] > sp.bbox[1,2]) bb[1,2] <- sp.bbox[1,2]
  if(bb[2,2] > sp.bbox[2,2]) bb[2,2] <- sp.bbox[2,2]
     
  #coonvert bb to poly
  b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
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
