#' 
#' @name joinByArea
#' @title spatial join by area
#' @description performs a spatial join by area
#' 
#' @param x an object of class "Spatial"
#' @param features an object of class "Spatial" from which we want to inherit a geocode
#' @param refCode an object of class = "character" representing the column to enrich x with
#' @param filter an optional filter in the form list(key="key",value="value") to apply to the 'features' object
#' @param na.rm TRUE (by default) if NA values (no spatial join) have to be removed
#' @return an object of class "Spatial" (x enriched with the refCode from features)
#' 
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#'
joinByArea <- function(x, features, refCode, filter = NULL, na.rm = TRUE) {
  
  if(!is.null(filter)){
    if(class(filter) != "list") stop("'filter' argument should a named list with 'key' and 'value'")
    if(!all(names(filter) %in% c("key","value"))) stop("'filter' names are incorrect. Filter should contain a 'key' and a 'value'")
    features <- features[features@data[,filter$key] == filter$value,]
  }
  
  #spatial join
  code <- over(x, features[,refCode])
  names <- colnames(x@data)
  x@data <- cbind(x@data, code)
  colnames(x@data) <- c(names, refCode)
  
  if(na.rm) x <- x[!is.na(x@data[,refCode]),]
  
  return(x)
}