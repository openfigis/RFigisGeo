# test_Utils.R
# Authors:
#	Emmanuel Blondel <emmanuel.blondel@fao.org>
#
# Description: RUnit tests for FigisGeoUtils
# Creation Date: 2014/01/30
# Revision Date: -
#=======================

require(sp)

test_findP4s = function(){
	checkEquals(findP4s("GCS_WGS_1984", morphToESRI=TRUE), "+proj=longlat +datum=WGS84")
	checkEquals(findP4s("GCS_WGS_1984", morphToESRI=FALSE), NA)
}

test_readWFS = function(){
	wfsRequest = "http://www.fao.org/figis/geoserver/fifao/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=fifao:FAO_MAJOR"
	features = readWFS(wfsRequest)
	checkEquals(typeof(features), "S4")
	checkEquals(class(features)[1], "SpatialPolygonsDtFrame")
	checkEquals(length(features), 19L)
}

test_exportFeatures = function(){
	wfsRequest = "http://www.fao.org/figis/geoserver/fifao/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=fifao:FAO_MAJOR"
	features = readWFS(wfsRequest)
	out1 = exportFeatures(features, outputFormat="SHAPE-ZIP")
	checkTrue(file.exists(out1))
	out2 = exportFeatures(features, outputFormat="GML")
	checkTrue(file.exists(out2))
}


