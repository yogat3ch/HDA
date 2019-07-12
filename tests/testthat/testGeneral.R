context("Test the general functions of the HDA package")
library(HDA)
options(scipen=12)
# Make reproducible
set.seed(1)
load(file = "../../etc/dat.RData")

test_that("Test that go functions as intended",
          {expect_false({x <- NaN
 go("x")})
            expect_false({x <- NA
 go("x")})
            expect_false({x <- NULL
 go("x")})
            expect_false({x <- numeric(0)
                         go("x")})
            expect_true({x <- {4/0}
 go("x")})
            expect_true(all({x <- dat
 go("x")}))})

test_that("Test that startPkgs is silent",
          expect_silent(startPkgs("magrittr")))

test_that("Test  that unloadPkgs is silent",
          expect_silent(unloadPkgs("magrittr")))

test_that("Test that Mode works for various data types",
          {expect_identical(Mode(dat$char),"ID")
            expect_identical(Mode(dat$fac),factor("a", levels = c("a","b","c")))
            expect_identical(Mode(c(5,4,23,5,6,7,3,2,5)),5)})

test_that("Test that find_peaks produces indexes appropriately based on varying m values",
          {expect_identical(find_peaks(dat$x),3)
            expect_identical(find_peaks(dat$x,m = .05),c(3,6,10))})

test_that("Test that findna produces consistent output",
          {tmp <- tempfile()
          expect_known_output(findna(dat), tmp, print = TRUE)
          expect_known_output(findna(dat), tmp, print = TRUE)})
test_that("rleIndex output and appropriate data.frame",
          {
            testthat::expect_identical(rleIndex(rle(c(rep(T,4),rep(F,4),rep(T,1), rep(F,1)))), data.frame(lengths = c(4L, 4L, 1L, 1L),values = c(TRUE, FALSE, TRUE, FALSE), start = c(1, 5, 9, 10), end = c(4L, 8L, 9L, 10L)))
          })
