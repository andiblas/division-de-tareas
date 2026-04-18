# Unit tests for funcionesVarias.R
# Run with: Rscript test_funcionesVarias.R
# Requires: install.packages("testthat")

library(testthat)

setwd(dirname(sys.frame(1)$ofile))
source("funcionesVarias.R")

test_that("productoria multiplies all elements of a vector", {
  result <- productoria(c(2, 3, 4))
  expect_equal(result$producto, 24)
  expect_equal(result$productoSinCeros, 24)
})

test_that("productoria treats zeros differently in productoSinCeros", {
  result <- productoria(c(2, 0, 5))
  expect_equal(result$producto, 0)
  expect_equal(result$productoSinCeros, 10)
})

test_that("bienestarSocial sums the diagonal of a matrix", {
  m <- matrix(c(1, 2, 3,
                4, 5, 6,
                7, 8, 9), nrow = 3, byrow = TRUE)
  expect_equal(bienestarSocial(m), 1 + 5 + 9)
})

test_that("proporciones normalises each column to sum to 1", {
  m <- matrix(c(1, 1,
                3, 1,
                0, 2), nrow = 3, byrow = TRUE)
  props <- proporciones(m)
  expect_equal(colSums(props), c(1, 1))
  expect_equal(props[, 1], c(0.25, 0.75, 0))
})
