context("Test the general functions of the HDA package")
library(HDA)
options(scipen=12)
# Make reproducible
set.seed(1)
dat <- data.frame(y = rnorm(15,0,1),x = {rnorm(15,0,1) + rnorm(15,0,.02)}, char = state.abb[sample(1:length(state.name), size = 15)], fac = factor(rep(c("a","b","c"), each = 5), levels = c("a","b","c")),  date = seq(lubridate::ymd("2018-12-12"), lubridate::ymd("2018-12-26"), 1), stringsAsFactors = F)

test_that("Test that %n% functions as intended",
          {expect_false(NaN %n% T)
            expect_false(NA %n% T)
            expect_false(NULL %n% T)
            expect_false(numeric(0) %n% T)
            expect_true({4/0} %n% T)
            expect_true(dat %n% T)})

test_that("Test that startPkgs is silent",
          expect_silent(startPkgs("magrittr")))

test_that("Test  that unloadPkgs is silent",
          expect_silent(unloadPkgs("magrittr")))

test_that("Test that Mode works for various data types",
          {expect_identical(Mode(dat$char),"ID")
            expect_identical(Mode(dat$fac),factor("a", levels = c("a","b","c")))
            expect_identical(Mode(c(5,4,23,5,6,7,3,2,5)),5)})

test_that("Test that find_peaks produces indexes appropriately based on varying m values",
          e{xpect_identical(find_peaks(dat$x),3)
            expect_identical(find_peaks(dat$x,m = .05),c(3,6,10))})

test_that("Test that findna produces consistent output",
          {tmp <- tempfile()
          expect_known_output(findna(dat), tmp, print = TRUE)
          expect_known_output(findna(dat), tmp, print = TRUE)})
