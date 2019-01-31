context("Test that functions for Assumption Evaluation in Statistical Tests work properly.")
library(HDA)
options(scipen=12)
# Make reproducible
set.seed(1)
load(file = "../../etc/dat.RData")

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
