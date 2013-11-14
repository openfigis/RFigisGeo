# Intersection.R
# Author: Emmanuel Blondel <emmanuel.blondel@fao.org>
#
# Description: computes an intersection
# Creation Date: 2013/11/14
# Revision Date: -
#=======================

# Computes an Intersection and returns a sp object
#
# Arguments:
# - features1: a first sp object
# - features2: a second sp object
# - gmlIdAttributeName: specific to GML, the name of the ID attribute, by default "gml_id"
#
# Notes:
# - only supported for GML2 for now
#
getIntersection <- function(features1, features2, gmlIdAttributeName="gml_id"){

	#check CRS
	if(proj4string(features1) != proj4string(features2)){
		print("CRS differ, try to project the second feature collection")
		features2 <- spTransform(features2, CRS(proj4string(features1)));		
	}
	
	#prepare schema (for attributes)
	dfText = "attrs <- data.frame("
	dfText <- paste(dfText, "ID=character(0)", sep="")
	data1 <- features1@data
	for(i in 1:length(colnames(data1))){
		if(colnames(data1)[i] != gmlIdAttributeName){
			clazz <- class(data1[,i])
			if(clazz != "factor"){
				clazz <- paste(clazz, "(0)", sep="")
			}else{
				clazz <- paste(clazz, "()", sep="")
			}
			dfText <-paste(dfText, ",", colnames(data1)[i], "=",clazz, sep="")
		}
	}
	data2 <- features2@data
	for(i in 1:length(colnames(data2))){
		if(colnames(data2)[i] != gmlIdAttributeName){
			clazz <- class(data1[,i])
			if(clazz != "factor"){
				clazz <- paste(clazz, "(0)", sep="")
			}else{
				clazz <- paste(clazz, "()", sep="")
			}
			dfText <-paste(dfText, ",", colnames(data2)[i], "=",clazz, sep="")
		}
	}
	dfText <- paste(dfText, ")", sep="")
	eval(parse(text=dfText))
	featureType <- gsub("\\.", "_", colnames(attrs))
	
	#compute the intersection
	intersects <- list()
	for (i in 1:nrow(features1)) {
		for (j in 1:nrow(features2)) {
			ID <- sprintf("%s_x_%s", features1@data[i,gmlIdAttributeName], features2@data[j,gmlIdAttributeName])
			intersection <- gIntersection(features1[i,], features2[j,])
			
			if(!is.null(intersection)){
				intersects[[ID]] <- Polygons(slot(slot(intersection, "polygons")[[1]], "Polygons"), ID = ID)
				drop <- c(gmlIdAttributeName)
				attrs <- rbind(attrs, cbind(ID, features1[i,]@data[, !(colnames(data1) %in% drop)], features2[j,]@data[, !(colnames(data2) %in% drop)]))
			}
		}
	}
	#encapsulate Polygons in a SpatialPolygons object
	sp <- SpatialPolygons(intersects, proj4string = CRS(proj4string(features1)))
	
	#prepare attributes data.frame
	colnames(attrs) <- featureType
	row.names(attrs) <- attrs$ID
	
	#join attributes to features
	id <- match(sapply(slot(sp, "polygons"), function(x) slot(x,"ID")), row.names(attrs))
    spdf <- SpatialPolygonsDataFrame(sp[!is.na(id),],attrs[id[!is.na(id)],],match.ID=TRUE)
	return(spdf);
}