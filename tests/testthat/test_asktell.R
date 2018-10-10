library(quizme)
library(testthat)

context("output of ask, tell and make_quiz is correct")

test_that("make_quiz returns a list",{
              expect_type(make_quiz(), "list")
})

test_that("ask and tell functions return null",{
              expect_null(ask())
              expect_null(tell())
})
