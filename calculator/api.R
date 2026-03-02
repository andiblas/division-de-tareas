# Allocation Calculator API
# A Plumber API that will compute fair chore allocations

library(plumber)

source("funcionesVarias.R")

#* @apiTitle Allocation Calculator API
#* @apiDescription Computes fair allocation of chores among agents

#* Calculate chore allocation
#* @param agents List of agent names
#* @param chores List of chore names
#* @param cost_values Nested list of cost values per agent/chore
#* @post /allocate
function(req) {
  # Parse the JSON body
  body <- req$body
  agents <- body$agents
  chores <- body$chores
  cost_values <- body$cost_values

  # Log received parameters
  message("=== Allocation Request Received ===")
  message("Agents: ", paste(agents, collapse = ", "))
  message("Chores: ", paste(chores, collapse = ", "))
  message("cost values: ", jsonlite::toJSON(cost_values, auto_unbox = TRUE))
  message("===================================")

  n_trab   <- length(agents)
  n_tareas <- length(chores)

  # Build matriz_valoracion: rows = chores, cols = agents, values = dislike scores
  m <- matrix(NA_real_, nrow = n_tareas, ncol = n_trab,
              dimnames = list(chores, agents))

  for (ai in seq_along(agents)) {
    for (ci in seq_along(chores)) {
      m[ci, ai] <- as.numeric(cost_values[[agents[ai]]][[chores[ci]]])
    }
  }

  message("Valuation matrix:")
  message(paste(capture.output(print(m)), collapse = "\n"))

  # Run the allocation algorithm
  result <- repartoTareas(n_trab, m)

  # Map chore indices (1-indexed) back to chore names, per agent
  allocation_named <- list()
  for (i in seq_along(agents)) {
    agent_name <- agents[i]
    chore_indices <- result$Art[[i]]
    allocation_named[[agent_name]] <- if (length(chore_indices) == 0) character(0) else chores[chore_indices]
  }

  # Map burden values to agent names (round to 4 decimal places for readability)
  burden_named <- setNames(lapply(round(result$llevan, 4), jsonlite::unbox), agents)

  message("Allocation result: ", jsonlite::toJSON(allocation_named, auto_unbox = FALSE))
  message("Burden: ", jsonlite::toJSON(burden_named, auto_unbox = TRUE))

  list(
    status     = "success",
    message    = "Allocation computed successfully",
    allocation = allocation_named,
    burden     = burden_named
  )
}

#* Health check endpoint
#* @get /health
function() {
  list(status = "ok")
}
