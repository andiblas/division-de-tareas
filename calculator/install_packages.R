# One-time setup script: install all R packages required by funcionesVarias.R
# Run once per machine: Rscript install_packages.R

options(repos = c(CRAN = "https://cloud.r-project.org"))

install.packages("igraph")
install.packages("isoband")
install.packages("sandwich")
install.packages("DirichletReg")
install.packages("shiny")
install.packages("ggplot2")
install.packages("plotly")
install.packages("gridExtra")
install.packages("partitions")
install.packages("combinat")
install.packages("testthat")
