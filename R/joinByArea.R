# joinByArea.R
# Authors:
#  Emmanuel Blondel <emmanuel.blondel@fao.org>
#
# Creation Date: 2015/10/16
# Revision Date: -
#=======================

# Performs a spatial join
#
# Arguments:
# - x an object of class "Spatial"
# - features an object of class "Spatial" from which we want to inherit a geocode
# - filter an optional filter in the form list(key="key",value="value") to apply to the 'features' object
# - na.rm TRUE (by default) if NA values (no spatial join) have to be removed

# Notes:
# - only supported for GML and SHAPE-ZIP
#
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