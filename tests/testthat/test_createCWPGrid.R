# test_createCWPGrid.R
# Author: Emmanuel Blondel <emmanuel.blondel@fao.org>
#
# Description: Unit tests for CWP Grid creation
# Creation Date: 2017/11/13
# Revision Date: -
#=======================
require(RFigisGeo, quietly = TRUE)
require(testthat)
context("createCWPGrid")

test_that("CWP size 1 - 10' x 10'",{
  sp1 <- createCWPGrid(size = 1)
})

test_that("CWP size 2 - 20' x 20'",{
  sp2 <- createCWPGrid(size = 2)
})

test_that("CWP size 3 - 30' x 30'",{
  sp3 <- createCWPGrid(size = 3)
})

test_that("CWP size 4 - 30' x 1°",{
  sp4 <- createCWPGrid(size = 4)
})

test_that("CWP size 5 - 1° x 1°",{
  sp5 <- createCWPGrid(size = 5)
})

test_that("CWP size 6 - 5° x 5°",{
  sp6 <- createCWPGrid(size = 6)
})

test_that("CWP size 7 - 10° x 10°",{
  sp7 <- createCWPGrid(size = 7)
})

test_that("CWP size 8 - 20° x 20°",{
  sp8 <- createCWPGrid(size = 8)
})

test_that("CWP size 9 - 30° x 30°",{
  sp9 <- createCWPGrid(size = 9)
})