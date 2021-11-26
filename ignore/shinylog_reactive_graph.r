library(reactlog)
# tell shiny to log all reactivity
reactlog::reactlog_enable()

# # run a shiny app
# app <- system.file("examples/01_hello", package = "shiny")
# runApp(app)
cheem::run_app()

# once app has closed, display reactlog from shiny
shiny::reactlogShow()
