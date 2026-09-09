# Unit tests for envidia2_tareas (repartoTareas.R)
# Run with: Rscript test_repartoTareas.R
# Requires: install.packages("testthat")

library(testthat)

source("repartoTareas.R")

# ---- envidia2_tareas --------------------------------------------------------
# Given a valuation matrix (rows = chores, cols = agents, values = dislike cost)
# and an allocation, it returns 6 fairness coefficients. For chores a value > 1
# means the fairness condition is violated, so the worst case is the MAXIMUM.
#
# All matrices below use columns that sum to 10, so proporciones() just divides
# by 10 and every expected value can be checked by hand.
#
# Notation used in the comments:
#   props[c, i] = what chore c costs agent i, as a fraction of their total
#   S[i, j]     = what agent j's bundle costs agent i (sum of props over bundle j)
#
# The 6 coefficients:
#   alfa_ef    = max over i!=j of  S[i,i] / S[i,j]
#   alfa_efx   = max over i!=j of (S[i,i] - lightest chore of i) / S[i,j]
#   alfa_ef1   = max over i!=j of (S[i,i] - heaviest chore of i) / S[i,j]
#   alfa_prop  = max over i of  n * S[i,i]
#   alfa_propx = max over i of  n * (S[i,i] - lightest chore of i)
#   alfa_prop1 = max over i of  n * (S[i,i] - heaviest chore of i)


test_that("envidia2_tareas on a symmetric 2-agent / 4-chore split", {
  #         A1  A2
  # chore1   1   4
  # chore2   2   3
  # chore3   3   2
  # chore4   4   1
  m <- matrix(c(1, 4,
                2, 3,
                3, 2,
                4, 1), nrow = 4, byrow = TRUE)

  # A1 takes chores 1 and 2, A2 takes chores 3 and 4.
  reparto <- list(c(1, 2), c(3, 4))

  # props (each column / 10):
  #   A1: .1 .2 .3 .4     A2: .4 .3 .2 .1
  # S[1,1] = .1+.2 = .3    S[1,2] = .3+.4 = .7
  # S[2,1] = .4+.3 = .7    S[2,2] = .2+.1 = .3
  res <- envidia2_tareas(m, reparto)

  # EF: both agents prefer their own bundle (0.3 < 0.7), so alfa_ef < 1.
  #   alfa_ef[1,2] = .3/.7 = 3/7      alfa_ef[2,1] = .3/.7 = 3/7
  expect_equal(res$alfa_ef, 3 / 7)

  # EFX: drop the lightest chore of each agent (0.1 for both).
  #   (.3 - .1)/.7 = 2/7  for both pairs
  expect_equal(res$alfa_efx, 2 / 7)

  # EF1: drop the heaviest chore of each agent (0.2 for both).
  #   (.3 - .2)/.7 = 1/7  for both pairs
  expect_equal(res$alfa_ef1, 1 / 7)

  # PROP: n * S[i,i] = 2 * .3 = .6 for both agents.
  expect_equal(res$alfa_prop, 0.6)

  # PROPx: 2 * (.3 - .1) = .4      PROP1: 2 * (.3 - .2) = .2
  expect_equal(res$alfa_propx, 0.4)
  expect_equal(res$alfa_prop1, 0.2)
})


test_that("envidia2_tareas detects envy in a 3-agent allocation (alfa_ef > 1)", {
  #         A1  A2  A3
  # chore1   6   1   1
  # chore2   3   6   3
  # chore3   1   3   6
  m <- matrix(c(6, 1, 1,
                3, 6, 3,
                1, 3, 6), nrow = 3, byrow = TRUE)

  # Worst possible split: every agent gets the chore they hate the most.
  reparto <- list(1, 2, 3)

  # props (each column / 10), and since bundle j = {chore j}: S[i,j] = props[j,i]
  # S = | .6  .3  .1 |
  #     | .1  .6  .3 |
  #     | .1  .3  .6 |
  res <- envidia2_tareas(m, reparto)

  # EF ratios (i != j): .6/.3 = 2 and .6/.1 = 6 → worst case is 6.
  expect_equal(res$alfa_ef, 6)

  # Each bundle has a single chore, so removing it (either as lightest or
  # heaviest) leaves 0 → both EFX and EF1 are trivially satisfied.
  expect_equal(res$alfa_efx, 0)
  expect_equal(res$alfa_ef1, 0)

  # PROP: 3 * .6 = 1.8 > 1 → not proportional.
  expect_equal(res$alfa_prop, 1.8)

  # PROPx / PROP1: 3 * (.6 - .6) = 0 for every agent.
  expect_equal(res$alfa_propx, 0)
  expect_equal(res$alfa_prop1, 0)
})


test_that("envidia2_tareas handles an agent with an empty bundle", {
  #         A1  A2
  # chore1   1   4
  # chore2   2   3
  # chore3   3   2
  # chore4   4   1
  m <- matrix(c(1, 4,
                2, 3,
                3, 2,
                4, 1), nrow = 4, byrow = TRUE)

  # A1 does everything, A2 does nothing.
  reparto <- list(1:4, integer(0))

  # S[1,1] = 1   S[1,2] = 0
  # S[2,1] = 1   S[2,2] = 0
  res <- envidia2_tareas(m, reparto)

  # EF: A1 divides by S[1,2] = 0 with a non-zero numerator → Inf (maximum envy).
  #     A2 has S[2,2] = 0, so their ratio is 0.
  expect_equal(res$alfa_ef, Inf)

  # EFX: A1 keeps 1 - .1 = .9 after dropping the lightest chore → .9/0 = Inf.
  #      A2 has an empty bundle → numerator forced to 0 → 0/1 = 0.
  expect_equal(res$alfa_efx, Inf)

  # EF1: A1 keeps 1 - .4 = .6 after dropping the heaviest chore → .6/0 = Inf.
  expect_equal(res$alfa_ef1, Inf)

  # PROP: max(2 * 1, 2 * 0) = 2
  expect_equal(res$alfa_prop, 2)

  # PROPx: A1 → 2 * (1 - .1) = 1.8 ; A2 (empty) → 0
  expect_equal(res$alfa_propx, 1.8)

  # PROP1: A1 → 2 * (1 - .4) = 1.2 ; A2 (empty) → 0
  expect_equal(res$alfa_prop1, 1.2)
})


test_that("envidia2_tareas returns 1 when both bundles cost an agent nothing", {
  # A2 is indifferent: every chore costs them 0 except one, which they keep.
  #         A1  A2
  # chore1   5   0
  # chore2   5  10
  m <- matrix(c(5, 0,
                5, 10), nrow = 2, byrow = TRUE)

  # A1 takes chore 1, A2 takes chore 2.
  reparto <- list(1, 2)

  # props:  A1: .5 .5     A2: 0 1
  # S[1,1] = .5   S[1,2] = .5
  # S[2,1] = 0    S[2,2] = 1
  res <- envidia2_tareas(m, reparto)

  # EF ratios: A1 → .5/.5 = 1 ; A2 → 1/0 = Inf (A2 would rather have A1's chore).
  expect_equal(res$alfa_ef, Inf)

  # EFX/EF1: single-chore bundles → numerator 0 for both agents.
  #   A1: 0/.5 = 0 ; A2: numerator 0 AND S[2,1] = 0 → the 0/0 branch returns 1.
  expect_equal(res$alfa_efx, 1)
  expect_equal(res$alfa_ef1, 1)

  # PROP: max(2 * .5, 2 * 1) = 2
  expect_equal(res$alfa_prop, 2)
  expect_equal(res$alfa_propx, 0)
  expect_equal(res$alfa_prop1, 0)
})


test_that("envidia2_tareas respects alfa_ef >= alfa_efx >= alfa_ef1", {
  # Removing the lightest chore always leaves at least as much burden as
  # removing the heaviest one, so the three coefficients must be ordered.
  # Same for the PROP family.
  m <- matrix(c(1, 4, 2,
                2, 3, 5,
                3, 2, 1,
                4, 1, 2), nrow = 4, byrow = TRUE)

  reparto <- list(c(1, 2), 3, 4)
  res <- envidia2_tareas(m, reparto)

  expect_true(res$alfa_ef >= res$alfa_efx)
  expect_true(res$alfa_efx >= res$alfa_ef1)

  expect_true(res$alfa_prop >= res$alfa_propx)
  expect_true(res$alfa_propx >= res$alfa_prop1)
})
