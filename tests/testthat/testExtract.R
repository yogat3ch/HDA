context("Test the general functions of the HDA package")
#library(HDA)
options(scipen=12)
# Make reproducible
set.seed(1)
testthat::test_that("Test that extractHighlights outputs as intended",
          {testthat::expect_message(val <- HDA::extractHighlights("../../etc/Test.pdf", cit = T),"Message: No author detected")
            testthat::expect_identical(val, "<div style='background-color:whitesmoke; border-left:groove Turquoise;padding:5px 5px 5px 10px;-webkit-box-shadow: 1px 1px 2px 1px rgba(0,0,0,0.75);-moz-box-shadow: 1px 1px 3px 1px rgba(0,0,0,0.75);box-shadow: 1px 1px 3px 1px rgba(0,0,0,0.75);'>Page.1: <strong>#Main</strong> <span style='color:#4054e9'>Test text</span>  This is a test pdf to test the functionality of <span style='color:Tomato;'>**HDA::extract Highlights**</span>. This box should have a *turquoise* left border. The keyword should be **bold**. *Test text* should be light blue. <em>Turquoise</em> should be underlined in both instances. The function name should be <strong>bold</strong> and tomato colored.  Page.1:  This should be an additional paragraph concatenated into a single highlight.(2019)</div>")})
