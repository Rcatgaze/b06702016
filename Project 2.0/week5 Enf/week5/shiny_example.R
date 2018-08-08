library(rsconnect)
library(shiny)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
options(shiny.usecairo = FALSE)
runApp()

#deployApp()

