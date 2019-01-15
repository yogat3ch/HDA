context("Test that functions for Assumption Evaluation in Statistical Tests work properly.")
library(HDA)
options(scipen=12)
# Make reproducible
set.seed(1)
dat <- data.frame(y = rnorm(15,0,1),x = {rnorm(15,0,1) + rnorm(15,0,.02)}, char = state.abb[sample(1:length(state.name), size = 15)], fac = factor(rep(c("a","b","c"), each = 5), levels = c("a","b","c")),  date = seq(lubridate::ymd("2018-12-12"), lubridate::ymd("2018-12-26"), 1), stringsAsFactors = F)

test_that("visEDA produces the appropriate output types",
          {tmp <- tempfile()
          expect_true(all(unique(lapply(visEDA(dat),class)) %>% unlist == c("gtable","gTree","grob","gDesc")))
          expect_length(visEDA(dat), 2)
          expect_known_output(visEDA(dat), tmp, print = TRUE)
          expect_known_output(visEDA(dat), tmp, print = TRUE)})

test_that("homoVariance produces consistent output",
          {tmp <- tempfile()
          expect_known_output(out <- homoVariance(x ~ fac, dat), tmp, print = TRUE)
          expect_known_output(out <- homoVariance(x ~ fac, dat), tmp, print = TRUE)})

test_that("testTrans produces consistent output",
          {tmp <- tempfile()
          expect_known_output(out <- testTrans("y",c("x"), dat), tmp, print = TRUE)
          expect_known_output(out <- testTrans("y",c("x"), dat), tmp, print = TRUE)})
