# Allocation Calculator API
# A Plumber API that will compute fair chore allocations

library(plumber)

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

  # For now, just echo back the parameters
  # TODO: Implement actual allocation algorithm using cost values
  list(
    status = "success",
    message = "Stub response - allocation algorithm not yet implemented",
    received = list(
      agents = agents,
      chores = chores,
      agents_count = length(agents),
      chores_count = length(chores),
      cost_values = cost_values
    )
  )
}

#* Health check endpoint
#* @get /health
function() {
  list(status = "ok")
}
