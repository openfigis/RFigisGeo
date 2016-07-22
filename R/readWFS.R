#' @name readWFS
#' @title readWFS
#' @description Read WFS & returns a sp object
#'
#' @param url the baseURL of the WFS GetFeature request
#' @param outputFormat the output format for the WFS GetFeature request, by default "GML"
#' @param p4s an optional proj4string, by default NULL (an attempt will be performed to get the projection from the data)
#' @param gmlIdAttributeName specific to GML, the name of the ID attribute, by default "gml_id"
#' @param verbose if log has to printed in the R console. TRUE by default
#' @return an object of class "Spatial"
#' 
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#'         Norbert Billet \email{norbert.billet@@ird.fr} 
#
readWFS <- function(url, outputFormat = "GML", p4s = NULL,
                    gmlIdAttributeName="gml_id", verbose = TRUE){
	
  	features <- NULL
  
	#request
	wfsRequest <- url
	if(outputFormat != "GML") {
		wfsRequest <- paste(url, "&outputFormat=", outputFormat, sep="")
	}
	
	if(outputFormat == "GML") {
		# download the data
		percentCodes <- "%20"
		names(percentCodes) <- "\x20"
		content <- getURL(curlPercentEncode(x = wfsRequest, codes = percentCodes))
		xmlfile <- xmlTreeParse(content, useInternalNodes = TRUE)
		
		#write the file to disk
		tempf = tempfile() 
		destfile = paste(tempf,".gml",sep='')
		saveXML(xmlfile, destfile)
		
		#download.file(wfsRequest, destfile, mode="wb")
		layername <- tryCatch(
		  ogrListLayers(destfile),
		  error = function(error) {
		    if(verbose){
		      message(error)
		    }
		  })
  
    if(is.null(layername) || length(layername) > 0) {
			 if(verbose){
        logger.warn("Unknown or Empty GIS web-resource")
			 }
      return(NULL)
		}
		
    #check if we have geometry
    propertyNames <- NULL
    properties <- unlist(strsplit(wfsRequest,"&propertyName="))
		if(length(properties) > 1){
		      propertyNames <- unlist(strsplit(properties[length(properties)], "&"))[1]
		      propertyNames <- unlist(strsplit(propertyNames, ","))
	    	}
		if(!is.null(propertyNames)){
      			hasGeometry <- any(c("GEOMETRY", "geometry", "THE_GEOM","the_geom") %in% propertyNames)
		}else{
		  hasGeometry = ((length(getNodeSet(xmlfile, "//gml:featureMember//gml:coordinates")) > 0)
		                 || (length(getNodeSet(xmlfile, "//gml:featureMember//gml:pos")) > 0)
		                 || (length(getNodeSet(xmlfile, "//gml:featureMember//gml:posList")) > 0))
		}
    
		 if(hasGeometry){
			
			# get the Spatial Reference System (SRS)
			srs <- NA
			#xmlfile<-xmlTreeParse(destfile, useInternalNodes = TRUE)
			fmXML <- getNodeSet(xmlfile, "//gml:featureMember")
			if(length(fmXML) > 0)
			  fmXML <- fmXML[[1]]
			  srsName <- getNodeSet(xmlDoc(fmXML), "//@srsName")
			if (length(srsName) >= 1) {
				srsName <- as.character(srsName[[1]])
			}	
			#srsName patterns matching		
			srsPattern = "http://www.opengis.net/gml/srs/epsg.xml#" #match case 1
			if(attr(regexpr(srsPattern, srsName, ignore.case = T),"match.length") > 0){
				epsg <- unlist(strsplit(srsName, srsPattern))[2]
				srs <- paste("+init=epsg:", epsg, sep="")
			}else{
				srsPattern = "urn:(x-)?ogc:def:crs:EPSG" #match case 2
				if(attr(regexpr(srsPattern, srsName, ignore.case = T),"match.length") > 0){
					srsStr <- unlist(strsplit(srsName, ":"))
					epsg <- srsStr[length(srsStr)]
					srs <- paste("+init=epsg:", epsg, sep="")
				}else{
					#search if srsName is a WKT PROJ name (PROJCS or GEOGCS)
					#if yes set srs with the corresponding proj4string
					#first search without any consideration of the ESRI representation
					srs <- findP4s(srsName, morphToESRI=FALSE)
					if (is.na(srs)) {
						#if not found search with consideration of the ESRI representation
						srs <- findP4s(srsName, morphToESRI=TRUE)
					}
					if (! is.na(srs) && ! length(srs) == 1) {
						srs <- NA
					}
				}
			}
			
			if(is.na(srs)){
				warning("Unable to convert GML srsName to a CRS object. CRS will be set to NA", call. = T)
			}
			
			if (missing(p4s)) p4s <- srs
			features = tryCatch(
				readOGR(destfile, layername, p4s = srs, disambiguateFIDs = TRUE, verbose = verbose),
                        	error = function(err){ if(verbose) message(paste0(err,"\n"))})
			if(!is.null(features)){
				if(regexpr("SpatialPoints", class(features)) == -1)
					features <- spChFIDs(features, as.character(features@data[,gmlIdAttributeName])) 
			}
			
		}else{
			membersContent <- sapply(getNodeSet(xmlfile, "//gml:featureMember"), function(x) xmlChildren(x))
			fid <- sapply(membersContent, function(x) xmlAttrs(x))
			membersAttributes <- xmlToDataFrame(
        			nodes = getNodeSet(xmlfile, "//gml:featureMember/*[@*]"),
        			stringsAsFactors = FALSE
      			)
			features <- cbind(fid, membersAttributes, stringsAsFactors = FALSE)
		}
		     	
	}else{
		stop("Unsupported WFS format")
	}
	return(features)
}
