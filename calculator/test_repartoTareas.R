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


# ---- repartoExhaustivoTareas ------------------------------------------------
# Walks every possible allocation of the chores and keeps: the leximin champion,
# the max Nash welfare champion, and the best attainable value of each of the 6
# alfas taken independently (so those 6 can come from 6 different allocations).
#
# For chores a larger alfa is worse, so "best" means MINIMUM here.

# Independent brute force used as the oracle: enumerate the k^n assignments
# directly instead of going through sum.comb/setparts/perms.
enumerar_repartos_fuerza_bruta <- function(n_tareas, n_agentes) {
  grilla <- expand.grid(rep(list(seq_len(n_agentes)), n_tareas))
  lapply(seq_len(nrow(grilla)), function(r) {
    asignacion <- as.integer(grilla[r, ])
    lapply(seq_len(n_agentes), function(a) which(asignacion == a))
  })
}


test_that("repartoExhaustivoTareas visits exactly k^n allocations", {
  m2 <- matrix(runif(4 * 2, 1, 10), nrow = 4, ncol = 2)
  expect_equal(repartoExhaustivoTareas(m2)$formas, 2^4)

  # 3 agents / 3 chores forces size profiles with zeros (e.g. 0,0,3), which is
  # how empty bundles show up in the enumeration.
  m3 <- matrix(runif(3 * 3, 1, 10), nrow = 3, ncol = 3)
  expect_equal(repartoExhaustivoTareas(m3)$formas, 3^3)
})


test_that("repartoExhaustivoTareas matches a brute force enumeration", {
  #         A1  A2
  # chore1   1   4
  # chore2   2   3
  # chore3   3   2
  # chore4   4   1
  m <- matrix(c(1, 4,
                2, 3,
                3, 2,
                4, 1), nrow = 4, byrow = TRUE)

  res <- repartoExhaustivoTareas(m)

  todos <- enumerar_repartos_fuerza_bruta(4, 2)
  metricas <- lapply(todos, function(rep) envidia2_tareas(m, rep))

  for (alfa in c("alfa_ef", "alfa_ef1", "alfa_efx", "alfa_prop", "alfa_prop1", "alfa_propx")) {
    esperado <- min(vapply(metricas, function(x) x[[alfa]], numeric(1)))
    expect_equal(res[[paste0(alfa, "_min")]], esperado, info = alfa)
  }

  expect_equal(res$nash_max, max(vapply(metricas, function(x) x$bienestar_nash, numeric(1))))

  # The leximin champion must beat every other allocation head to head.
  for (rep in todos) {
    expect_equal(comparacion_leximin_pp_tareas(res$reparto_min_leximin, rep, m), 1)
  }
})


test_that("repartoExhaustivoTareas optima are at least as good as both champions", {
  m <- matrix(c(1, 4, 2,
                2, 3, 5,
                3, 2, 1,
                4, 1, 2,
                2, 2, 3), nrow = 5, byrow = TRUE)

  res <- repartoExhaustivoTareas(m)
  metricas_lex  <- envidia2_tareas(m, res$reparto_min_leximin)
  metricas_nash <- envidia2_tareas(m, res$reparto_max_nash)

  # By definition of a minimum over all allocations. This is the assertion that
  # catches a min/max inversion when porting the goods version.
  for (alfa in c("alfa_ef", "alfa_ef1", "alfa_efx", "alfa_prop", "alfa_prop1", "alfa_propx")) {
    expect_lte(res[[paste0(alfa, "_min")]], metricas_lex[[alfa]])
    expect_lte(res[[paste0(alfa, "_min")]], metricas_nash[[alfa]])
  }

  # Nash welfare of the Nash champion is the reported maximum.
  expect_equal(metricas_nash$bienestar_nash, res$nash_max)
})


test_that("repartoExhaustivoTareas returns the allocation that attains each optimum", {
  m <- matrix(c(1, 4, 2,
                2, 3, 5,
                3, 2, 1,
                4, 1, 2,
                2, 2, 3), nrow = 5, byrow = TRUE)

  res <- repartoExhaustivoTareas(m)

  for (alfa in c("alfa_ef", "alfa_ef1", "alfa_efx", "alfa_prop", "alfa_prop1", "alfa_propx")) {
    reparto <- res[[paste0("reparto_", alfa, "_min")]]
    expect_equal(envidia2_tareas(m, reparto)[[alfa]], res[[paste0(alfa, "_min")]], info = alfa)
  }
})


# ---- generarRepartos --------------------------------------------------------
# Same generation logic that repartoExhaustivoTareas runs at its start, pulled
# out so it can be inspected on its own. It takes no valuations: given n chores
# and k agents it must produce every one of the k^n ways of handing each chore
# to exactly one agent.
#
# A reparto is a list of k vectors: reparto[[i]] = the chores agent i does.
# Empty bundles are legal and show up as integer(0).
#
# To read the assertions below, each reparto is written as its "who does what"
# vector — one entry per chore, holding the agent doing it:
#   list(1:2, integer(0))  ->  "1-1"   (agent 1 does both chores)
#   list(2, 1)             ->  "2-1"   (agent 2 does chore 1, agent 1 chore 2)
firma <- function(reparto) {
  duenio <- integer(sum(lengths(reparto)))
  for (a in seq_along(reparto)) duenio[reparto[[a]]] <- a
  paste(duenio, collapse = "-")
}


test_that("generarRepartos lists the 4 allocations of 2 chores between 2 agents", {
  repartos <- generarRepartos(n_tareas = 2, n_agentes = 2)

  # The whole universe, written out by hand:
  #   "1-1"  agent 1 does both chores, agent 2 does nothing
  #   "1-2"  agent 1 does chore 1, agent 2 does chore 2
  #   "2-1"  agent 2 does chore 1, agent 1 does chore 2
  #   "2-2"  agent 2 does both chores, agent 1 does nothing
  expect_equal(sort(sapply(repartos, firma)), c("1-1", "1-2", "2-1", "2-2"))
})


test_that("generarRepartos produces exactly k^n allocations, all different", {
  for (params in list(c(n_tareas = 3, n_agentes = 2),
                      c(n_tareas = 2, n_agentes = 3),
                      c(n_tareas = 3, n_agentes = 3),
                      c(n_tareas = 4, n_agentes = 2))) {
    n <- params[["n_tareas"]]
    k <- params[["n_agentes"]]
    etiqueta <- paste(n, "chores /", k, "agents")

    repartos <- generarRepartos(n, k)

    # Each chore goes to one of k agents, independently → k^n allocations.
    expect_equal(length(repartos), k^n, info = etiqueta)

    # And no allocation is generated twice. This half is the interesting one:
    # size profiles with two or more zeros (e.g. 0,0,3) can easily be counted
    # more than once, since permuting two idle agents changes nothing.
    expect_equal(length(unique(sapply(repartos, firma))), k^n, info = etiqueta)
  }
})


test_that("generarRepartos includes allocations that leave agents with no chores", {
  # 2 chores among 3 agents: at least one agent is always idle.
  repartos <- generarRepartos(n_tareas = 2, n_agentes = 3)
  firmas <- sapply(repartos, firma)

  # "Agent 1 does both chores, agents 2 and 3 idle" must be there, exactly once.
  expect_equal(sum(firmas == "1-1"), 1)

  # Same for each of the other two agents taking everything.
  expect_equal(sum(firmas == "2-2"), 1)
  expect_equal(sum(firmas == "3-3"), 1)

  # Every reparto has one slot per agent, and with only 2 chores for 3 agents
  # at least one of those slots is empty.
  for (reparto in repartos) {
    expect_equal(length(reparto), 3)
    expect_true(any(lengths(reparto) == 0))
  }
})


test_that("generarRepartos assigns every chore exactly once", {
  repartos <- generarRepartos(n_tareas = 4, n_agentes = 3)

  for (reparto in repartos) {
    expect_equal(length(reparto), 3)
    # Pooling the three bundles must give back the 4 chores: none lost, none
    # handed to two agents at the same time.
    expect_equal(sort(unlist(reparto)), 1:4)
  }
})
