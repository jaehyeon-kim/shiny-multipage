#install.packages(c("Rcpp", "PKI"))
#devtools::install_github("trestletech/shinyStore")

#library(Rcpp)
#library(shinyStore)
library(shiny)
library(shinythemes)

source("ui.R")
source("server.R")
shinyApp(ui, server)
