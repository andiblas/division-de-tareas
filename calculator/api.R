# Allocation Calculator API
# A Plumber API that will compute fair chore allocations

library(plumber)

#* @apiTitle Allocation Calculator API
#* @apiDescription Computes fair allocation of chores among agents

#* Calculate chore allocation
#* @param agents List of agent names
#* @param chores List of chore names
#* @post /allocate
function(req) {
  # Parse the JSON body
  body <- req$body
  agents <- body$agents
  chores <- body$chores

  # Log received parameters
  message("=== Allocation Request Received ===")
  message("Agents: ", paste(agents, collapse = ", "))
  message("Chores: ", paste(chores, collapse = ", "))
  message("===================================")

  # For now, just echo back the parameters
  # TODO: Implement actual allocation algorithm
  list(
    status = "success",
    message = "Stub response - allocation algorithm not yet implemented",
    received = list(
      agents = agents,
      chores = chores,
      agents_count = length(agents),
      chores_count = length(chores)
    )
  )
}

#* Health check endpoint
#* @get /health
function() {
  list(status = "ok")
}
