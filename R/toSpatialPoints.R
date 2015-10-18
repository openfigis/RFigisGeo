#' 
#' @name toSpatialPoints
#' @title toSpatialPoints
#' @description Convert a data.frame with lonlat columns to a SpatialPoints object
#'
#' @param x an object of class "data.frame"
#' @param lonlat a vector of length 2 given the lon/lat column names e.g. c("Lon","Lat")
#' @param verbose TRUE (by default) to display logs
#' @return an object of class "SpatialPoints"
#'
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#' 
toSpatialPoints <- function(x, lonlat, verbose = TRUE){
  if(missing(lonlat) | is.null(lonlat)) stop("Lon/Lat columns are missing")
  if(length(lonlat) != 2) stop("lonlat should be a vector of length 2")
  if(class(lonlat) != "character") stop("lonlat should be of class 'character'")
  
  #geo-reference the data
  if(verbose) message(paste0("Input data has ", nrow(x), " records"))
  qFilter <- is.na(x[,lonlat[1]]) | x[,lonlat[1]]=="" | is.na(x[,lonlat[2]]) | x[,lonlat[2]]==""
  toremove <- x[qFilter,]
  if(nrow(toremove) > 0){
    if(verbose) message(paste0("Removing ", nrow(toremove), " records with empty coordinates"))
    x <- x[!qFilter,]
  }
  x[,lonlat[1]] <- as.numeric(x[,lonlat[1]])
  x[,lonlat[2]] <- as.numeric(x[,lonlat[2]])
  spdf <- x
  coordinates(spdf) <- lonlat
  proj4string(spdf) <- CRS("+init=epsg:4326")
  return(spdf)
}
