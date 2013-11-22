# Intersection.R
# Author: Emmanuel Blondel <emmanuel.blondel@fao.org>
#
# Description: computes an intersection
# Creation Date: 2013/11/14
# Revision Date: 2013/11/18
#=======================

# Computes an Intersection and returns a sp object
#
# Arguments:
# - features1: a first sp object
# - features2: a second sp object
# - gmlIdAttributeName: specific to GML, the name of the ID attribute, by default "gml_id"
# - areaCRS: a CRS object used as reference for area calculation
#
# Notes:
# - only supported for GML2 for now
#
getIntersection <- function(features1, features2, gmlIdAttributeName="gml_id", areaCRS){
  
  #check CRS
  if(proj4string(features1) != proj4string(features2)){
    print("CRS differ, try to project the second feature collection")
    features2 <- spTransform(features2, CRS(proj4string(features1)));  	
  }
  
  #compute the intersection possibility
  features.intersects <- gIntersects(features1, features2, byid=TRUE)
  #keep only relevant data
  features1 <- features1[apply(features.intersects, 2, function(x) {sum(x)}) > 0, ]
  features2 <- features2[apply(features.intersects, 1, function(x) {sum(x)}) > 0, ]
  #compute the intersection
  intersection <- gIntersection(features1, features2, byid=TRUE)
  
  intersection.structure <- data.frame(features1=colnames(features.intersects)[as.vector(col(features.intersects))], 
                                       features2=rownames(features.intersects)[as.vector(row(features.intersects))], 
                                       inter=c(features.intersects), 
                                       stringsAsFactors=FALSE)
  
  intersection.structure <- intersection.structure[intersection.structure$inter,]
  ID <- paste(intersection.structure$features1, "_x_", intersection.structure$features2, sep="")
  intersection <- spChFIDs(intersection, ID)
  
  merge.df <- merge(merge(features1@data, intersection.structure, by.x=gmlIdAttributeName, by.y="features1"), features2@data, by.x="features2", by.y=gmlIdAttributeName)
  rownames(merge.df) <- paste(merge.df[, gmlIdAttributeName], "_x_", merge.df$features2, sep="")
  merge.df$features2 <- NULL
  merge.df[, gmlIdAttributeName] <- NULL
  merge.df$inter <- NULL
  merge.df$ID <- ID
  
  if( ! missing(areaCRS)) {
    merge.df$custom_area <- gArea(spTransform(intersection, areaCRS, byid=TRUE))
  }
  
  spdf <- SpatialPolygonsDataFrame(Sr=intersection, data=merge.df, match.ID=TRUE)
  
  return(spdf);
}