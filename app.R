# app.R
library(shiny)

# load your ui and server definitions
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
