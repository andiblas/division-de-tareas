# Run the Plumber API server
library(plumber)

# Load and run the API
pr <- plumb("api.R")
pr$run(host = "0.0.0.0", port = 8000)
