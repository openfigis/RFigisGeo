# test_Reallocation.R
# Author: Emmanuel Blondel <emmanuel.blondel@fao.org>
#
# Description: Unit tests for FigisGeoUtils
# Creation Date: 2014/02/06
# Revision Date: -
#=======================
require(RFigisGeo, quietly = TRUE)
require(testthat)
context("Reallocation")

test_that("Reallocation",{
	
	#test stat data
	dfdata <- data.frame(cbind(c(rep("d1_v1",2),rep("d1_v2",3)), rep("d2_v1",5), c(rep(c("src1","src2"),2),"src3"),c(2500,1907,400,300,359)))
	colnames(dfdata) <- c("dim1","dim2","area","value")
			
	#test intersections data
	intersections <- data.frame(cbind(c(rep("src1",5),rep("src2",3)),c(c("trg1","trg2","trg3","trg4","trg5"),c("trg3","trg2","trg5")),c(c(45,367,123.45,789.34,34.2),c(23.56, 676.89, 345.34))))
	colnames(intersections) <- c("area1","area2","intersect")		
			
	#perform reallocation test
	spread <- reallocate(dfdata,intersections,"area","area1","value","intersect")
	expect_is(spread,"data.frame")
	sapply(unique(spread[,"value"]),
		function(x){
			expect_equal(x, sum(spread[spread$value == x,]$spread))
		}			
	)
})
