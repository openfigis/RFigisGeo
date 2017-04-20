#' 
#' @name intersection
#' @title intersection
#' @description Computes an Intersection
#' 
#' @param features1 a first sp object
#' @param features2 a second sp object
#' @param gmlIdAttributeName specific to GML, names of the ID attributes for each 
#'        features, by default c("gml_id", "gml_id")
#' @param areaCRS a CRS object used as reference for area calculation. If no value 
#'        is provided, take the current CRS. If NA is provided there no area computation.
#' @return an object of class "Spatial"
#' 
#' @note only supported for GML 2
#' 
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#'         Norbert Billet \email{norbert.billet@@ird.fr}
#'
intersection <- function(features1, features2,
                            gmlIdAttributeName=c("gml_id", "gml_id"), areaCRS = NULL){
  
  #check GML id attribute
  if (length(gmlIdAttributeName) == 1L) {
    gmlIdAttributeName <- rep(gmlIdAttributeName, 2)
  }
  
  hasData1 <- (attr(regexpr("DataFrame", class(features1)),"match.length") > 0)
  hasData2 <- (attr(regexpr("DataFrame", class(features2)),"match.length") > 0)
  if(hasData1) {
  	if(!(gmlIdAttributeName[1L] %in% colnames(features1@data))){
  		features1@data$gml_id <- 1:nrow(features1@data)
  	}
  	row.names(features1) <- as.character(features1@data$gml_id)
  }
  if(hasData2) {
  	if(!(gmlIdAttributeName[2L] %in% colnames(features2@data))){
  		features2@data$gml_id <- 1:nrow(features2@data)
  	}
  	row.names(features2) <- as.character(features2@data$gml_id)
  }
  
  #check CRS
  targetCRS <- proj4string(features1)
  if (targetCRS != proj4string(features2)) {
    print("CRS differ, try to project the second feature collection")
    features2 <- spTransform(features2, CRS(targetCRS));  	
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
  if(hasData1)
    baseClass1 = strsplit(baseClass1,"DataFrame")[[1]]
  if(hasData2)
    baseClass2 = strsplit(baseClass2,"DataFrame")[[1]]
  trgGeomObj <- as.character(refs[baseClass1, baseClass2])
  trgGeomSlot <- switch(trgGeomObj,
    "point"= c(NA, "Points"),
    "line" = c("lines","Lines"),
    "poly" = c("polygons", "Polygons")
  )
  trgGeomClass <- paste("Spatial", trgGeomSlot[2], sep = "")
  
  #compute intersection predicates
  features.intersects <- NULL
  tryCatch({
  	features.intersects <- gIntersects(features1, features2, byid=TRUE)
  },error=function(error){
  	logger.error(paste(error,"\n"))
  })
  if(is.null(features.intersects)){
  	logger.warn("No intersect between the 2 source feature collections")
  	return(NULL)
  }
	
  #stop here if no intersects at all
  if(!any(feature.intersects)) return(NULL)
	
  features1 <- features1[apply(features.intersects, 2L, function(x) {sum(x)}) > 0, ]
  features2 <- features2[apply(features.intersects, 1L, function(x) {sum(x)}) > 0, ]
  int <- gIntersects(features1, features2, byid=TRUE)
  int.df <- data.frame(int)
  colnames(int.df) <- row.names(features1)
  int.df <- cbind(feature2 = row.names(int.df), int.df)
  int.df <- reshape(int.df, direction = "long",
                    varying = list(names(int.df)[2:ncol(int.df)]),
                    v.names = "intersect", idvar = "feature2",
                    timevar = "feature1", times = colnames(int.df)[2:ncol(int.df)])
  int.df <- int.df[int.df$intersect == TRUE,]
  
  #compute intersection geometries
  vec <- vector(mode="list", length = nrow(int.df))
  for (i in seq(along=vec)) {
    feat1 <- as.character(int.df[i,"feature1"])
    feat2 <- as.character(int.df[i,"feature2"])
    output <- try(
      gIntersection(
        features1[feat1,],
        features2[feat2,],
        byid = TRUE
      )
    )
    
    if(class(output) == "SpatialCollections"){
      
      spf <- slot(output, paste(trgGeomObj, "obj", sep = ""))
      sflist <- lapply(slot(spf, trgGeomSlot[1]), function(t){
        slot(t, trgGeomSlot[2])[[1]]
      })
      
      id <- paste(feat1, feat2, sep=" ")
      output <- switch(trgGeomObj,
        "line" = SpatialLines(list(Lines(sflist, ID = id)), proj4string = CRS(targetCRS)),
        "poly" = SpatialPolygons(list(Polygons(sflist, ID = id)), proj4string = CRS(targetCRS))
      )
      
      if(!is.null(output) & class(output) == trgGeomClass) vec[[i]] <- output
      
    }else{
      if(!is.null(output)  & class(output) == trgGeomClass) vec[[i]] <- output
    }
  }
  int.features <- do.call("rbind",vec[sapply(vec, function(x) !is.null(x) & !inherits(x, "try-error"))])
  
  #ids pairs
  rn <- row.names(int.features)
  nrn <- do.call("rbind", strsplit(rn, " "))
  int.id <- paste(nrn[,1], nrn[,2], sep = "_x_")
  int.features.structure <- data.frame(ID = int.id,
                                       features1 = nrn[,1], features2 = nrn[,2],
                                       stringsAsFactors=FALSE)
  if(regexpr("SpatialPoints", class(int.features)) == -1) int.features <- spChFIDs(int.features, int.id)
  
  #append attributes
  out <- int.features
  if(hasData1 || hasData2){
	  
	  #compute areas if no areaCRS is provided or if an valid areaCRS is provided.
	  #(i.e. areas are not computed if areaCRS=NA)
	  withArea <- FALSE
	  if(!missing(areaCRS) && !is.null(areaCRS)){
  		if(class(areaCRS) != "CRS") stop("Invalid areaCRS object. Must be an object of class 'CRS'")
  		if(class(int.features) == "SpatialPolygons") withArea <- TRUE
	  }
	  #compute source input areas
	  if(withArea){
  		features1@data$geo_area1 <- sapply(features1@polygons,function(x){
  			gArea(spTransform(SpatialPolygons(Srl =list(x), proj4string = CRS(proj4string(features1))), areaCRS))
  		})
  		features2@data$geo_area2 <- sapply(features2@polygons,function(x){
  			gArea(spTransform(SpatialPolygons(Srl =list(x), proj4string = CRS(proj4string(features2))), areaCRS))
  		})
	  }
	
    merge.df <- int.features.structure 
	  if(hasData1){
		  merge.df <- merge(features1@data, int.features.structure, by.x=gmlIdAttributeName[1L], by.y="features1")
	  }
	  if(hasData2){
		  merge.df <- merge(merge.df, features2@data, by.x="features2", by.y=gmlIdAttributeName[2L])
	  }
	  rownames(merge.df) <- paste(merge.df[, gmlIdAttributeName[1L]], merge.df$features2, sep="_x_")
	  merge.df$features2 <- NULL
	  merge.df[, gmlIdAttributeName[1L]] <- NULL
	  merge.df$ID <- NULL

	  if (withArea) {
  		#intersection area
  		area.df <- data.frame(geo_area=gArea(spTransform(int.features, areaCRS), byid=TRUE))
  		merge.df <- merge(merge.df, area.df, by="row.names")
  		
  		#add percentages of area
  		merge.df$per_area1 <- merge.df$geo_area / merge.df$geo_area1 * 100
  		merge.df$per_area2 <- merge.df$geo_area / merge.df$geo_area2 * 100
  		
    }
	  
	  merge.df$Row.names <- NULL	  
	  merge.df <- cbind(gml_id = row.names(int.features), merge.df, stringsAsFactors = FALSE)
	  rownames(merge.df) <- row.names(int.features)
	  
	  #build the result sp dataframe
	  out <- switch(class(int.features),
					"SpatialPoints" = SpatialPointsDataFrame(coords = int.features, data = merge.df, match.ID=TRUE),
					"SpatialLines" = SpatialLinesDataFrame(sl = int.features, data = merge.df, match.ID=TRUE),
					"SpatialPolygons" = SpatialPolygonsDataFrame(Sr = int.features, data = merge.df, match.ID=TRUE))
	}
	
	return(out)
}
