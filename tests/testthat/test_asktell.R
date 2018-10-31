library(quizme)
library(testthat)

context("output of ask, tell gives error with numeric argument")
quizme()
test_that("ask and tell functions return error with argument",{
              expect_error(ask(5))
              expect_error(tell(5))
})
