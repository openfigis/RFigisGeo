# FigisGeoUtils.R
# Author: Emmanuel Blondel <emmanuel.blondel@fao.org>
#
# Description: provides a set of utility R functions
# Creation Date: 2013/11/14
# Revision Date: -
#=======================

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
		xmlfile<-xmlTreeParse(destfile,useInternalNodes = TRUE)
		feat <-xmlChildren(getNodeSet(xmlfile,paste('//','gml:featureMember', sep=''))[[1]])[[1]]
		ns <-unlist(strsplit(xmlName(feat, full = T),':'))[1]
		node<-getNodeSet(xmlfile, paste("//", ns, ":the_geom/*",sep=""))
		if(length(node) == 0){
			node<-getNodeSet(xmlfile, paste("//", ns, ":THE_GEOM/*",sep="")) #try with uppercase geom name (case of Oracle datastore layers)
		}
		node <- xmlRoot(xmlDoc(node[[1]]))
		srsName <- xmlGetAttr(node, "srsName")
	
		#srsName patterns matching
		srs <- NA
		srsPattern = "http://www.opengis.net/gml/srs/epsg.xml#" #match case 1
		if(length(regexpr(srsPattern, srsName, ignore.case = T)) == 1){
			epsg <- unlist(strsplit(srsName, srsPattern))[2]
			srs <- paste("+init=epsg:", epsg, sep="")
		}else{
			srsPattern = "urn:EPSG" #match case 2
			if(length(regexpr(srsPattern, srsName, ignore.case = T)) == 1){
				srsStr <- unlist(strsplit(srsName, ":"))
				epsg <- srsStr[length(srsStr)]
				srs <- paste("+init=epsg:", epsg, sep="")
			}else{
				#TODO match case 3
				#search if srsName is a WKT PROJ name (PROJCS or GEOGCS)
				#if yes set srs with the corresponding proj4string

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

