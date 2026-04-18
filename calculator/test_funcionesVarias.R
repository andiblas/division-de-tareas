# Unit tests for funcionesVarias.R
# Run with: Rscript test_funcionesVarias.R
# Requires: install.packages("testthat")

library(testthat)

setwd(dirname(sys.frame(1)$ofile))
source("funcionesVarias.R")

# ---- repartoTareasRoundRobin -------------------------------------------------
# Round-Robin: agents take turns (in the given order) picking the chore they
# dislike the least from those still available. Lower score = less disliked.
# dislikeMatrix: rows = chores, cols = agents.
# Returns: list(Art, llevan) following the repartoTareas convention, where
# Art[[i]] is the vector of 1-indexed chore indices assigned to agent i.

test_that("repartoTareasRoundRobin assigns each agent their least-disliked chore (one round)", {
  # 2 agents, 2 chores
  #         A1  A2
  # chore1   1  10
  # chore2  10   1
  m <- matrix(c(1, 10,
                10, 1), nrow = 2, byrow = TRUE)

  result <- repartoTareasRoundRobin(2, m)

  expect_setequal(result$Art[[1]], 1)  # A1 takes chore 1
  expect_setequal(result$Art[[2]], 2)  # A2 takes chore 2
})

test_that("repartoTareasRoundRobin cycles through agents over multiple rounds", {
  # 2 agents, 3 chores — one full round is not enough, so a 2nd round runs.
  #         A1  A2
  # chore1   1   5
  # chore2   2   6
  # chore3   3   7
  # Pick order: A1 -> chore1, A2 -> chore2, A1 -> chore3.
  m <- matrix(c(1, 5,
                2, 6,
                3, 7), nrow = 3, byrow = TRUE)

  result <- repartoTareasRoundRobin(2, m)

  expect_setequal(result$Art[[1]], c(1, 3))
  expect_setequal(result$Art[[2]], 2)
})
