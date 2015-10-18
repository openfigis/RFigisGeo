# test_Reallocation.R
# Author: Emmanuel Blondel <emmanuel.blondel@fao.org>
#
# Description: Unit tests for SPREAD algorithm
# Creation Date: 2014/02/06
# Revision Date: -
#=======================
require(RFigisGeo, quietly = TRUE)
require(testthat)
context("Reallocation")

test_that("Areal Reallocation",{
	
	#test stat data
	dfdata <- data.frame(cbind(c(rep("d1_v1",2),rep("d1_v2",3)), rep("d2_v1",5), c(rep(c("src1","src2"),2),"src3"),c(2500,1907,400,300,359)))
	colnames(dfdata) <- c("dim1","dim2","area","value")
	
	#test intersections data
	intersections <- data.frame(
			cbind(
					c(rep("src1",5),rep("src2",3)),
					c(c("trg1","trg2","trg3","trg4","trg5"),c("trg3","trg2","trg5")),
					c(c(45,367,123.45,789.34,34.2),c(23.56, 676.89, 345.34))
			)
	)
	colnames(intersections) <- c("area1","area2","intersect")	
	#perform reallocation test
	spread <- reallocate(x = dfdata, y = intersections, area.x = "area", area.y = "area1", by.x = NULL, by.y = NULL,
						data = "value", warea = "intersect", wprob = NULL)
	expect_is(spread,"data.frame")
	sapply(unique(spread[,"value"]),
			function(x){
				expect_equal(x, sum(spread[spread$value == x,]$spread))
			}			
	)
			
})

test_that("Reallocation - with additional mappings",{
	
	#test stat data
	dfdata <- data.frame(cbind(c(rep("d1_v1",2),rep("d1_v2",3)), rep("d2_v1",5), c(rep(c("src1","src2"),2),"src3"),c(2500,1907,400,300,359)))
	colnames(dfdata) <- c("dim1","dim2","area","value")
			
	#test intersections data
	intersections <- data.frame(
						cbind(
							c(rep("src1",5),rep("src2",3)),
							c(c("trg1","trg2","trg3","trg4","trg5"),c("trg3","trg2","trg5")),
							c(rep("d1_v1",3),rep("d1_v2",2),rep("d1_v2", 3)),
							c(0.8, 0.56, 0.45, 0.2, 0.43, 0.89, 0.3, 0.98),
							c(c(45,367,123.45,789.34,34.2),c(23.56, 676.89, 345.34))
						)
					)
	colnames(intersections) <- c("area1","area2","dim1","d1_prob","intersect")		
			
	#perform reallocation test
	spread <- reallocate(x = dfdata, y = intersections, area.x = "area", area.y = "area1", by.x = "dim1", by.y = "dim1",
						data = "value", warea = "intersect", wprob = "d1_prob")
	expect_is(spread,"data.frame")
	sapply(unique(spread[,"value"]),
		function(x){
			expect_equal(x, sum(spread[spread$value == x,]$spread))
		}			
	)
})

test_that("Reallocation - with aggregates",{
			
	#test stat data
	dfdata <- data.frame(cbind(c(rep("d1_v1",2),rep("d1_v2",3)), rep("d2_v1",5), c(rep(c("src1","src2"),2),"src3"),c(2500,1907,400,300,359)))
	colnames(dfdata) <- c("dim1","dim2","area","value")
	
	#test intersections data
	intersections <- data.frame(
			cbind(
					c(rep("src1",5),rep("src2",3)),
					c(c("trg1","trg2","trg3","trg4","trg5"),c("trg3","trg2","trg5")),
					c(rep("d1_v1",3),rep("d1_v2",2),rep("d1_v2", 3)),
					c(0.8, 0.56, 0.45, 0.2, 0.43, 0.89, 0.3, 0.98),
					c(c(45,367,123.45,789.34,34.2),c(23.56, 676.89, 345.34))
			)
	)
	colnames(intersections) <- c("area1","area2","dim1","d1_prob","intersect")		
	
	#perform reallocation test
	spread <- reallocate(x = dfdata, y = intersections, area.x = "area", area.y = "area1", by.x = "dim1", by.y = "dim1",
			data = "value", warea = "intersect", wprob = "d1_prob", aggregates ="area2")
	expect_is(spread,"data.frame")
	expect_true(all(names(spread) %in% c("area1", "area2", "dim1", "dim2", "spreadValue")))
})
