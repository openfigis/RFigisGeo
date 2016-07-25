#' @name readWFS
#' @title readWFS
#' @description Read WFS & returns a sp object
#'
#' @param url the baseURL of the WFS GetFeature request
#' @param outputFormat the output format for the WFS GetFeature request, by default "GML"
#' @param p4s an optional proj4string, by default NULL (an attempt will be performed to get the projection from the data)
#' @param gmlIdAttributeName specific to GML, the name of the ID attribute, by default "gml_id"
#' @param target.dir a target directory where temporary GML files will be downloaded
#' @param verbose if log has to printed in the R console. TRUE by default
#' @return an object of class "Spatial"
#' 
#' @author Emmanuel Blondel \email{emmanuel.blondel1@@gmail.com}
#'         Norbert Billet \email{norbert.billet@@ird.fr} 
#
readWFS <- function(url, outputFormat = "GML", p4s = NULL,
                    gmlIdAttributeName="gml_id", target.dir = NULL,
                    verbose = TRUE){
  
  	features <- NULL
  
	#request
	wfsRequest <- url
	if(outputFormat != "GML") {
		wfsRequest <- paste(url, "&outputFormat=", outputFormat, sep="")
	}
	
  	if(verbose) logger.info(sprintf("Reading WFS request: %s \n",wfsRequest))
	if(outputFormat == "GML") {
		# download the data
		percentCodes <- "%20"
		names(percentCodes) <- "\x20"
		content <- getURL(curlPercentEncode(x = wfsRequest, codes = percentCodes))
		xmlfile <- xmlTreeParse(content, useInternalNodes = TRUE)
		
		if(!all(class(xmlfile) == c("XMLInternalDocument","XMLAbstractDocument"))){
			if(verbose) logger.error("Fetch data doesn't seem in XML format \n")
			return(NULL)
		}
    
		#write the file to disk
    		tempdir <- NULL
    		if(missing(target.dir) || is.null(target.dir)){
      			tempdir <- tempdir()
    		}else{
      			if(file.exists(target.dir)){ 
        			tempdir <- target.dir
      			}else{
        			if(verbose) logger.error(sprintf("Target directory %s doesn't exist \n",target.dir))
        			return(NULL)
      			}
    		}
    
    		if(verbose){
    			logger.info(sprintf("Writing temporary GML file to '%s' \n",tempdir))
    		}
		
		tempf = tempfile(tmpdir = tempdir) 
		destfile = paste(tempf,".gml",sep='')
		saveXML(xmlfile, destfile)
    
    		if(!file.exists(destfile)){
      			if(verbose) logger.error(sprintf("GML file %s cannot be found \n", destfile))
      			return(NULL)
    		}
    
    		deleteGML <- function(){
			if(verbose) logger.info(sprintf("Deleting temporary GML file '%s' \n",destfile))
      			unlink(destfile)
      			unlink(paste0(tempf,".gfs"))
    		}
		
		#download.file(wfsRequest, destfile, mode="wb")
		if(verbose) logger.info(sprintf("Fetching layer info from GML file '%s' using GDAL \n",destfile))
		layername <- tryCatch(
			ogrListLayers(destfile),
		  	error = function(error) {
		    		if(verbose){
		      			logger.error(error)
		      			deleteGML()
				}
		})
  
    		if(is.null(layername) || length(layername) == 0) {
			if(verbose) logger.error("Unknown or Empty GIS web-resource \n")
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
      			if(verbose) logger.info("Inheriting Spatial Reference System...\n")
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
					if(verbose) logger.info("Fetching SRS definition\n")
					srs <- findP4s(srsName, morphToESRI=FALSE)
					if (is.na(srs)){
						#if not found search with consideration of the ESRI representation
						srs <- findP4s(srsName, morphToESRI=TRUE)
					}
					if (! is.na(srs) && ! length(srs) == 1) {
						srs <- NA
					}
				}
			}
			
			if(is.na(srs)){
				if(verbose) logger.warn("Unable to convert GML srsName to a CRS object. CRS will be set to NA \n")
			}else{
        			if(verbose) logger.info(sprintf("SRS definition = '%s' \n",srs))
			}
			
			if (missing(p4s)) p4s <- srs
			if(verbose) logger.info(sprintf("Reading temporary GML file '%s' with GDAL \n",destfile))
			features = tryCatch(
				readOGR(destfile, layername, p4s = srs, disambiguateFIDs = TRUE, verbose = verbose),
                        	error = function(err){
                        		if(verbose){
                              			logger.error(err)
                        		 	deleteGML()
                            		}
                        })
			if(!is.null(features)){
				if(regexpr("SpatialPoints", class(features)) == -1){
					logger.info(sprint("Add feature identifiers for object of class '%s'", class(features)))
					features <- spChFIDs(features, as.character(features@data[,gmlIdAttributeName]))
				}
			}else{
				if(verbose) logger.warn(sprintf("Features returned by GDAL are null \n",destfile))	
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
		  
  		#unlink temporary files
  		deleteGML()
		     	
	}else{
		stop("Unsupported WFS format")
	}

  	if(verbose){
    		logger.info("WFS features successfully fetched! \n")
    		logger.info(sprintf("Number of features = %s \n", nrow(features)))
  	}
	return(features)
}
