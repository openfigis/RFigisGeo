# test_Intersection.R
# Author: Emmanuel Blondel <emmanuel.blondel@fao.org>
#
# Description: Unit tests for Intersection process
# Creation Date: 2014/02/10
# Revision Date: -
#=======================
require(RFigisGeo, quietly = TRUE)
require(testthat)
context("Intersection")

test_that("Intersection",{
	features1 <- readWFS("http://www.fao.org/figis/geoserver/fifao/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=fifao:FAO_MAJOR")
	features2 <- readWFS("http://www.fao.org/figis/geoserver/species/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=species:SPECIES_DIST_OCC")
	intersection <- getIntersection(features1, features2)
	expect_is(intersection, "SpatialPolygonsDataFrame")
	expect_true(all(names(features1)[-1] %in% names(intersection)))
	expect_true(all(names(features2)[-1] %in% names(intersection)))
	expect_true("geo_area" %in% names(intersection))
})