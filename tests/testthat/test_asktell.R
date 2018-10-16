library(quizme)
library(testthat)

context("output of ask, tell is correct")
quizme()
test_that("ask and tell functions return null",{
              expect_null(ask())
              expect_null(tell())
})
