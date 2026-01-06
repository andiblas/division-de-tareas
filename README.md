# Allocation of chores platform
The allocation of chores platform will be in charge of finding the best allocation of chores for the given agents. We understand as best allocation as the most fair allocation for all agents.

# Architecture
```
Web app (Flask) <-----> Allocation calculator (R + Plumber)
```

### Web app
This is the presentation layer that will be in charge of gathering the input for an instance.
Instance: is a set of variables that represent a problem. Agents, chores and their value functions.
The Web APP will be in charge of receiving the input from the presentation layer and processing the problem.
The main tasks on this layer will be:
1. Receiving the input and determine which is the best allocation algorithm to pick depending on simple criteria; input's size.
2. Parsing that input and send it over to the Allocation calculator layer. This will be done via HTTP.
3. Waiting for the Allocation calculator's response, parse it and render the results.
Programmed in Python, with Flask.

### Allocation calculator
This layer is responsible of the actual allocation calculator. Receives the input data from the Web API layer.
Programmed in R, the statistical computing language. This layer holds different strategies to calculate the allocation.
Web API chooses which strategy to use, and this layer uses that strategy to calculate. 
There's a thin layer at this level that will use [Plumber](https://github.com/rstudio/plumber), a HTTP API tool that expose
the main functions to the upper layer.
