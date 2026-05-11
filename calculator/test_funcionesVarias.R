# Unit tests for funcionesVarias.R
# Run with: Rscript test_funcionesVarias.R
# Requires: install.packages("testthat")

library(testthat)

# setwd(dirname(sys.frame(1)$ofile))
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

  result <- repartoTareasRoundRobin(m, c(1, 2))

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

  result <- repartoTareasRoundRobin(m, c(1, 2))

  expect_setequal(result$Art[[1]], c(1, 3))
  expect_setequal(result$Art[[2]], 2)
})


# ---- repartoTareasAllRoundRobins --------------------------------------------
# Runs repartoTareasRoundRobin for every permutation of the agents and returns
# a list of length n! with entries list(order, Art, llevan).

find_by_order <- function(results, ord) {
  for (r in results) {
    if (length(r$order) == length(ord) && all(r$order == ord)) return(r)
  }
  NULL
}

test_that("repartoTareasAllRoundRobins returns one result per permutation with correct allocations", {
  # 2 agents, 2 chores -> 2! = 2 permutations
  #         A1  A2
  # chore1   1   5
  # chore2   2   6
  m <- matrix(c(1, 5,
                2, 6), nrow = 2, byrow = TRUE)

  results <- repartoTareasAllRoundRobins(m)

  expect_length(results, 2)

  # order (1, 2): A1 picks chore 1, A2 picks chore 2
  r12 <- find_by_order(results, c(1, 2))
  expect_false(is.null(r12))
  expect_setequal(r12$Art[[1]], 1)
  expect_setequal(r12$Art[[2]], 2)

  # order (2, 1): A2 picks chore 1 (lower dislike), A1 picks chore 2
  r21 <- find_by_order(results, c(2, 1))
  expect_false(is.null(r21))
  expect_setequal(r21$Art[[1]], 2)
  expect_setequal(r21$Art[[2]], 1)
})


# ---- repartoTareasTopTrading ------------------------------------------------
# Top-Trading Envy-Cycle Elimination (Bhaskar, Sricharan, Vaish 2022 — Alg. 2)
# Assigns chores one at a time to a "sink" agent (one who envies nobody).
# If no sink exists, resolves a cycle in the top-trading envy graph first.
# Guarantees EF1 for additive valuations.

test_that("TopTrading assigns all chores and produces valid allocation", {
  # 3 agents, 4 chores
  m <- matrix(c(3, 1, 2,
                1, 3, 2,
                2, 2, 3,
                4, 1, 1), nrow = 4, byrow = TRUE)

  set.seed(1)
  result <- repartoTareasTopTrading(3, m)

  all_assigned <- sort(unlist(result$Art))
  expect_equal(all_assigned, 1:4)
  expect_length(result$Art, 3)
  expect_length(result$llevan, 3)
})

test_that("TopTrading returns EF1 allocation on paper Example 1", {
  # Example 1 from Bhaskar et al. (page 3): 3 agents, 6 chores
  # Paper uses negative values; we use absolute dislike costs.
  m <- matrix(c(
    1, 2, 1,
    4, 1, 3,
    2, 2, 1,
    3, 2, 1,
    0, 3, 3,
    1, 1, 10
  ), nrow = 6, byrow = TRUE)

  set.seed(42)
  result <- repartoTareasTopTrading(3, m)

  all_assigned <- sort(unlist(result$Art))
  expect_equal(all_assigned, 1:6)

  ef1_check <- EF1(result$Art, m)
  expect_equal(ef1_check$ef1, 1)
})

test_that("TopTrading is EF1 with 2 agents, equal preferences", {
  # Both agents dislike all chores equally — any complete allocation is EF1.
  m <- matrix(c(5, 5,
                5, 5,
                5, 5,
                5, 5), nrow = 4, byrow = TRUE)

  set.seed(7)
  result <- repartoTareasTopTrading(2, m)

  all_assigned <- sort(unlist(result$Art))
  expect_equal(all_assigned, 1:4)

  ef1_check <- EF1(result$Art, m)
  expect_equal(ef1_check$ef1, 1)
})

test_that("TopTrading handles more agents than chores", {
  # 3 agents, 2 chores — one agent should get nothing.
  m <- matrix(c(3, 1, 2,
                1, 3, 2), nrow = 2, byrow = TRUE)

  set.seed(10)
  result <- repartoTareasTopTrading(3, m)

  all_assigned <- sort(unlist(result$Art))
  expect_equal(all_assigned, 1:2)
  expect_length(result$Art, 3)

  empty_count <- sum(lengths(result$Art) == 0)
  expect_equal(empty_count, 1)
})

test_that("TopTrading handles single agent", {
  # 1 agent gets all chores.
  m <- matrix(c(3, 7, 2, 5), nrow = 4, ncol = 1)

  result <- repartoTareasTopTrading(1, m)

  expect_equal(sort(result$Art[[1]]), 1:4)
})

test_that("TopTrading is EF1 with asymmetric preferences", {
  # 3 agents, 6 chores with varied preferences to stress cycle resolution.
  m <- matrix(c(
    9, 1, 5,
    1, 8, 4,
    5, 5, 9,
    2, 7, 1,
    8, 2, 3,
    3, 6, 7
  ), nrow = 6, byrow = TRUE)

  set.seed(99)
  result <- repartoTareasTopTrading(3, m)

  all_assigned <- sort(unlist(result$Art))
  expect_equal(all_assigned, 1:6)

  ef1_check <- EF1(result$Art, m)
  expect_equal(ef1_check$ef1, 1)
})

test_that("TopTrading is EF1 across multiple random seeds", {
  m <- matrix(c(
    1, 2, 1,
    4, 1, 3,
    2, 2, 1,
    3, 2, 1,
    0, 3, 3,
    1, 1, 10
  ), nrow = 6, byrow = TRUE)

  for (s in 1:20) {
    set.seed(s)
    result <- repartoTareasTopTrading(3, m)
    all_assigned <- sort(unlist(result$Art))
    expect_equal(all_assigned, 1:6, info = paste("seed", s))
    ef1_check <- EF1(result$Art, m)
    expect_equal(ef1_check$ef1, 1, info = paste("seed", s))
  }
})
