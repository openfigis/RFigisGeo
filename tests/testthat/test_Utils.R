# test_Utils.R
# Authors:
#	Emmanuel Blondel <emmanuel.blondel@fao.org>
#
# Description: Unit tests for Utils functions
# Creation Date: 2014/01/30
# Revision Date: -
#=======================
require(RFigisGeo, quietly = TRUE)
require(testthat)
context("Utils")

test_that("findP4s",{
	p1 = findP4s("GCS_WGS_1984", morphToESRI=TRUE)
	p2 = findP4s("GCS_WGS_1984", morphToESRI=FALSE)
	expect_equal(p1, "+proj=longlat +datum=WGS84")
	expect_equal(p2, NA)
})

test_that("readWFS",{
	wfsRequest = "http://www.fao.org/figis/geoserver/fifao/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=fifao:FAO_MAJOR&outputFormat=text/xml; subtype=gml/2.1.2"
	features = readWFS(wfsRequest)
	expect_equal(typeof(features), "S4")
	expect_is(features, "SpatialPolygonsDataFrame")
	expect_equal(length(features), 19L)
})

test_that("readWFS - geometryless",{
	wfsRequest = "http://www.fao.org/figis/geoserver/GeoRelationship/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=GeoRelationship:FAO_AREAS_x_EEZ_HIGHSEAS&outputFormat=text/xml; subtype=gml/2.1.2&maxFeatures=50";
	df = readWFS(wfsRequest)
	expect_is(df, "data.frame")
	expect_equal(nrow(df), 50L)
})

test_that("exportFeatures",{
	wfsRequest = "http://www.fao.org/figis/geoserver/fifao/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=fifao:FAO_MAJOR"
	features = readWFS(wfsRequest)
	out1 = exportFeatures(features, outputFormat="SHP", tozip = TRUE)
	expect_true(file.exists(out1))
	out2 = exportFeatures(features, outputFormat="GML")
	expect_true(file.exists(out2))
})


