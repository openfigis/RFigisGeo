# Intersection.R
# Author: Emmanuel Blondel <emmanuel.blondel@fao.org>
# Author: Norbert Billet <norbert.billet@ird.fr>
#
# Description: computes an intersection
# Creation Date: 2013/11/14
# Revision Date: 2013/11/18
# Revision Date: 2013/12/05 : Improve performance
# Revision Date: 2014/05/23: Sanitize SpatialCollections & improve performance
#===============================================================================

# Computes an Intersection and returns a sp object
#
# Arguments:
# - features1: a first sp object
# - features2: a second sp object
# - gmlIdAttributeName: specific to GML, names of the ID attributes for each 
#                       features, by default c("gml_id", "gml_id")
# - areaCRS: a CRS object used as reference for area calculation. If no value is
#            provided, take the current CRS. If NA is provided there no area 
#            computation.
#
# Notes:
# - only supported for GML2 for now
#
getIntersection <- function(features1, features2,
                            gmlIdAttributeName=c("gml_id", "gml_id"), areaCRS){
  
  #check GML id attribute
  if (length(gmlIdAttributeName) == 1L) {
    gmlIdAttributeName <- rep(gmlIdAttributeName, 2)
  }
  
  #check CRS
  if (proj4string(features1) != proj4string(features2)) {
    print("CRS differ, try to project the second feature collection")
    features2 <- spTransform(features2, CRS(proj4string(features1)));  	
  }
  
  #target geometry obj
  #(used for sanitizing output with presence of SpatialCollections)
  spClasses <- c("SpatialPoints","SpatialLines","SpatialPolygons")
  refs <- as.data.frame(matrix(c("point","point","point","point","point",
                                 "line","point","line","poly"),ncol = 3))
  colnames(refs) <- spClasses
  row.names(refs) <- spClasses

  baseClass1 = class(features1)
  baseClass2 = class(features2)
  if(attr(regexpr("DataFrame", class(features1)),"match.length") > 0)
    baseClass1 = strsplit(baseClass1,"DataFrame")[[1]]
  if(attr(regexpr("DataFrame", class(features2)),"match.length") > 0)
    baseClass2 = strsplit(baseClass2,"DataFrame")[[1]]
  targetGeomObj <- refs[baseClass1, baseClass2]
  
  #compute intersection predicates
  features.intersects <- gIntersects(features1, features2, byid=TRUE)
  features1 <- features1[apply(features.intersects, 2L, function(x) {sum(x)}) > 0, ]
  features2 <- features2[apply(features.intersects, 1L, function(x) {sum(x)}) > 0, ]
  int <- gIntersects(features1, features2, byid=TRUE)
  
  #compute intersection geometries
  vec <- vector(mode="list", length=dim(int)[2])
  for (i in seq(along=vec)) {
    output <- try(gIntersection(features1[i,], features2[int[,i],], byid=TRUE))
    if(class(output) == "SpatialCollections"){
      output <- slot(output, paste(targetGeomObj, "obj", sep = ""))
    }
    if(!is.null(output)) vec[[i]] <- output 
  }
  int.features <- do.call("rbind",vec[sapply(vec, function(x) !inherits(x, "try-error"))])

  rn <- row.names(int.features)
  nrn <- do.call("rbind", strsplit(rn, " "))
  int.id <- paste(nrn[,1], nrn[,2], sep = "_x_")
  int.features.structure <- data.frame(ID = int.id,
                                       features1 = nrn[,1], features2 = nrn[,2],
                                       stringsAsFactors=FALSE)
  int.features <- spChFIDs(int.features, int.id)
  
  #append attributes
  merge.df <- merge(
    merge(features1@data, int.features.structure, by.x=gmlIdAttributeName[1L], by.y="features1"),
    features2@data,
    by.x="features2",
    by.y=gmlIdAttributeName[2L]
  )
  rownames(merge.df) <- paste(merge.df[, gmlIdAttributeName[1L]], merge.df$features2, sep="_x_")
  merge.df$features2 <- NULL
  merge.df[, gmlIdAttributeName[1L]] <- NULL
  merge.df$ID <- NULL

  #compute areas if no areaCRS is provided or if an valid areaCRS is provided.
  #(i.e. areas are not computed if areaCRS=NA)
  withArea <- FALSE
  if (missing(areaCRS)) {
    area.df <- data.frame(geo_area=gArea(int.features, byid=TRUE))
    withArea <- TRUE
  } else {
    if (class(areaCRS) == "CRS") {
      area.df <- data.frame(geo_area=gArea(spTransform(int.features, areaCRS), byid=TRUE))
      withArea <- TRUE
    }
  }
  if (withArea) {
    merge.df <- merge(merge.df, area.df, by="row.names")
    rownames(merge.df) <- merge.df$Row.names
    merge.df$Row.names <- NULL
  }
  
  #build the result sp dataframe
  return(SpatialPolygonsDataFrame(Sr = int.features, data = merge.df, match.ID=TRUE))
}
