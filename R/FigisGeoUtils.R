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
# - outputFormat: the output format for the WFS GetFeature request, by default "GML2"
# - p4s: an optional proj4string, by default NULL (an attempt will be performed to get the projection from the data)
# - gmlIdAttributeName: specific to GML, the name of the ID attribute, by default "gml_id"
#
# Notes:
# - only supported for GML2 for now
#
readWFS <- function(url, outputFormat = "GML2", p4s = NULL, gmlIdAttributeName="gml_id"){
	
	#request
	wfsRequest <- paste(url, "&outputFormat=", outputFormat, sep="")
	
	if(outputFormat == "GML2"){
		
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
		workspace <-unlist(strsplit(xmlName(xmlChildren(getNodeSet(xmlfile,paste('//','gml:featureMember', sep=''))[[1]])[[1]], full = T),':'))[1]
		node<-getNodeSet(xmlfile, paste("//", workspace, ":the_geom/*",sep=""))
		if(length(node) == 0){
			#try with uppercase geom name (case of Oracle datastore layers)
			node<-getNodeSet(xmlfile, paste("//", workspace, ":THE_GEOM/*",sep=""))
		}
		value<-sapply(node, function(x) xmlGetAttr(x, "srsName"))
		srsValue<-strsplit(value,"#")[[1]][2]
		projection<-paste("+init=epsg:",srsValue,sep="")
		
		if (missing(p4s)){
			try(crs<-CRS(projection), silent = T)
			if(!exists("crs")){
				stop("The code ", srsValue, " is an unknown EPSG code. Specify the p4s projection argument (PROJ4 format).")
			}else{
				features = readOGR(destfile, layername, p4s = projection, disambiguateFIDs=TRUE)
			} 
		}else{
			features = readOGR(destfile, layername, p4s = p4s, disambiguateFIDs=TRUE)
		}
		features <- spChFIDs(features, as.character(features@data[,gmlIdAttributeName]))      	
	}else{
		stop("Unsupported WFS format")
	}
	return(features)
}




