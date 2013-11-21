# FigisGeoUtils.R
# Author: Emmanuel Blondel <emmanuel.blondel@fao.org>
#
# Description: provides a set of utility R functions
# Creation Date: 2013/11/14
# Revision Date: -
#=======================

# Find a proj4 definition from a srsName
#
# Arguments:
# - srsName: name of the spatial reference system
# - morphToESRI: use the ESRI WKT representation
# Value:
#  List of proj4 defnitions where the WKT GEOGCS attribute is equal to the required srsName
#
# Comment:
#  We actually except lat/long coordinates systems due to confusion in output of different WFS vendors
#
findP4s <- function(srsName, morphToESRI=FALSE) {
	
	if (missing(srsName)) {
		stop("please provide a spatial reference system name")
	}
	#we remove the latlong proj for compatibility with sp
	proj.lst <- as.character(projInfo("proj")$name)
	proj.lst <- proj.lst[proj.lst != "latlong" & proj.lst != "latlon"]
	#build combinations of know proj and datum
	proj.datum.grd <- expand.grid(proj=proj.lst, datum=as.character(projInfo("datum")$name), stringsAsFactors=FALSE)
	#function to ask WKT representation
	getShowWkt <- function(x) {
		res <- try(showWKT(paste("+proj=", x[1], " +datum=", x[2], sep=""), morphToESRI=morphToESRI), silent=TRUE)
		if (class(res) == "try-error") {
			return(NA)
		} else {
			return(res)
		}
	}
	#ask WKT for all projection
	GCS <- apply(proj.datum.grd, 1, FUN=getShowWkt)
	
	GCS.df <- data.frame(proj=proj.datum.grd$proj, datum=proj.datum.grd$datum, WKT=GCS, stringsAsFactors=FALSE)
	#keep only valids
	GCS.df <- GCS.df[! is.na(GCS.df$WKT),]
	#the pattern to find
	pattern <- paste("GEOGCS[\"", srsName, "\"", sep="")
	#search for pattern
	GCS.df <- GCS.df[substr(tolower(GCS.df$WKT), 1, nchar(pattern)) == tolower(pattern),]
	#keep only first SRS in case of identical WKT representation
	GCS.df <- GCS.df[!duplicated(GCS.df$WKT),]
	#return the proj4 definition
	return(paste("+proj=", GCS.df$proj, " +datum=", GCS.df$datum, sep=""))
}

# Read WFS & returns a sp object
#
# Arguments:
# - url: the baseURL of the WFS GetFeature request
# - outputFormat: the output format for the WFS GetFeature request, by default "GML"
# - p4s: an optional proj4string, by default NULL (an attempt will be performed to get the projection from the data)
# - gmlIdAttributeName: specific to GML, the name of the ID attribute, by default "gml_id"
#
#
readWFS <- function(url, outputFormat = "GML", p4s = NULL, gmlIdAttributeName="gml_id"){
	
	#request
	wfsRequest <- url
	if(outputFormat != "GML") {
		wfsRequest <- paste(url, "&outputFormat=", outputFormat, sep="")
	}
	
	if(outputFormat == "GML") {
		
		# download the data
		tempf = tempfile() 
		destfile = paste(tempf,".gml",sep='')
		download.file(wfsRequest, destfile, mode="wb")
		layername <- ogrListLayers(destfile)
		if (length(layername) != 1) {
			stop("Mistake with layers in the input dataset")
		}
		
		# get the Spatial Reference System (SRS)
		srs <- NA
		xmlfile<-xmlTreeParse(destfile, useInternalNodes = TRUE)
		srsName <- getNodeSet(xmlfile, "(//gml:featureMember//@srsName)[1]")
		
		if (length(srsName) == 1) {
			srsName <- as.character(srsName[[1]])
			
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
					if (length(srs) == 0) {
						#if not found search with consideration of the ESRI representation
						srs <- findP4s(srsName, morphToESRI=TRUE)
					}
					if (! length(srs) == 1) {
						srs <- NA
					}
				}
			}
		}
		
		if(is.na(srs)){
			warning("Unable to convert GML srsName to a CRS object. CRS will be set to NA", call. = T)
		}
		
		if (missing(p4s)){
			features = readOGR(destfile, layername, p4s = srs, disambiguateFIDs=TRUE)
		}else{
			features = readOGR(destfile, layername, p4s = p4s, disambiguateFIDs=TRUE)
		}
		features <- spChFIDs(features, as.character(features@data[,gmlIdAttributeName]))      	
	}else{
		stop("Unsupported WFS format")
	}
	return(features)
}


# Export sp object to well-known formats
#
# Arguments:
# - features: the sp object to export
# - outputFormat: the format of the output file, by default "SHAPE-ZIP" (a zipped shapefile)
# - file.path: the base path where to export the features
# - file.name: the name of the output file
#
# Notes:
# - only supported for GML and SHAPE-ZIP
#
exportFeatures <- function(features, outputFormat = "SHAPE-ZIP", file.path = NULL, file.name = NULL){
	
	ug <- uuid.gen()
	uuid<-ug()
	if(is.null(file.name)){
		file.name <- uuid
	}
	if(is.null(file.path)){
		file.path <- paste(tempdir(),"/",uuid,sep="")
	}
	
	#manage output formats
	if(outputFormat == "SHAPE-ZIP"){
		writeOGR(features, file.path, file.name, driver="ESRI Shapefile")
		zip_path<-paste(file.path,"/",file.name,".zip",sep="")
		shapefiles <- list.files(file.path, pattern = file.name, full.names=TRUE)
		zip(zipfile=zip_path, flags="-r9Xj", files=shapefiles) # requires R_ZIPCMD to be set in linux OS.
		
		if (! file.exists(zip_path)) {
			zip_path <- NA
			stop("Error when creating zip file")
		}
		outputFile <- paste(file.path, "/", file.name, ".zip", sep="")
		
	}else if(outputFormat == "GML"){
		writeOGR(rfeatures, file.path, file.name, driver="GML")
		outputFile <- paste(file.path, "/", file.name, ".gml", sep="")
	}else{
		stop("Unsupported output format")
	}
	return(outputFile)
}

