library(shiny)

# TO START THE APP
# Set your working directory to the folder containing app.R
# run this command in the console: shiny::runApp()


source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)


