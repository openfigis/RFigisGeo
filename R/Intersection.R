# Intersection.R
# Author: Emmanuel Blondel <emmanuel.blondel@fao.org>
# Author: Norbert Billet <norbert.billet@ird.fr>
#
# Description: computes an intersection
# Creation Date: 2013/11/14
# Revision Date: 2013/11/18
# Revision Date: 2013/12/05 : Improve performance
#=======================

# Computes an Intersection and returns a sp object
#
# Arguments:
# - features1: a first sp object
# - features2: a second sp object
# - gmlIdAttributeName: specific to GML, names of the ID attributes for each features, by default c("gml_id", "gml_id")
# - areaCRS: a CRS object used as reference for area calculation. If no value is provided, take the current CRS. If NA is provided there no area computation
#
# Notes:
# - only supported for GML2 for now
#
getIntersection <- function(features1, features2, gmlIdAttributeName=c("gml_id", "gml_id"), areaCRS){
  
  #check GML id attribute
  if (length(gmlIdAttributeName) == 1L) {
    gmlIdAttributeName <- c(gmlIdAttributeName, gmlIdAttributeName)
  }
  
  #check CRS
  if (proj4string(features1) != proj4string(features2)) {
    print("CRS differ, try to project the second feature collection")
    features2 <- spTransform(features2, CRS(proj4string(features1)));  	
  }
  
  #compute the intersection possibility
  features.intersects <- gIntersects(features1, features2, byid=TRUE)
  #keep only relevant data
  features1 <- features1[apply(features.intersects, 2L, function(x) {sum(x)}) > 0, ]
  features2 <- features2[apply(features.intersects, 1L, function(x) {sum(x)}) > 0, ]
  #compute the intersection
  features.intersection <- gIntersection(features1, features2, byid=TRUE)
  
  features.intersection.structure <- data.frame(features1=colnames(features.intersects)[as.vector(col(features.intersects))], 
                                       features2=rownames(features.intersects)[as.vector(row(features.intersects))], 
                                       inter=c(features.intersects), 
                                       stringsAsFactors=FALSE)
  
  features.intersection.structure <- features.intersection.structure[features.intersection.structure$inter,]
  ID <- paste(features.intersection.structure$features1, "_x_", features.intersection.structure$features2, sep="")
  features.intersection <- spChFIDs(features.intersection, ID)
  
  merge.df <- merge(merge(features1@data, features.intersection.structure, by.x=gmlIdAttributeName[1L], by.y="features1"), features2@data, by.x="features2", by.y=gmlIdAttributeName[2L])
  rownames(merge.df) <- paste(merge.df[, gmlIdAttributeName[1L]], "_x_", merge.df$features2, sep="")
  merge.df$features2 <- NULL
  merge.df[, gmlIdAttributeName[1L]] <- NULL
  merge.df$inter <- NULL
  merge.df$ID <- ID
  
  #compute areas if no areaCRS is provided or if an valid areaCRS is provided. i.e. areas are not computed if areaCRS=NA
  withArea <- FALSE
  if (missing(areaCRS)) {
    area.df <- data.frame(area_custom=gArea(features.intersection, byid=TRUE))
    withArea <- TRUE
  } else {
    if (class(areaCRS) == "CRS") {
      area.df <- data.frame(area_custom=gArea(spTransform(features.intersection, areaCRS), byid=TRUE))
      withArea <- TRUE
    }
  }
  if (withArea) {
    merge.df <- merge(merge.df, area.df, by="row.names")
    rownames(merge.df) <- merge.df$Row.names
    merge.df$Row.names <- NULL
  }
  
  #build the result sp dataframe
  return(SpatialPolygonsDataFrame(Sr=features.intersection, data=merge.df, match.ID=TRUE))
}
