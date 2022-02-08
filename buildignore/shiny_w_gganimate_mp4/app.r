library(shiny)
library(gapminder)
library(ggplot2)
library(gganimate)

p <- ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  facet_wrap(~continent, scales = "free") +
  scale_x_log10() +
  transition_manual(year)

ui <- shinyUI(basicPage(
  imageOutput("gif"), ## Works
  imageOutput("mp4")  ## doesn't work
))

server <- shinyServer(function(input, output) {
  
  output$gif <- renderImage({
    outfile <- tempfile(fileext='.gif')
    anim_gif <- animate(p)
    anim_save("outfile.gif", anim_gif)
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif'
         #width = 800, height = 800, alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  output$mp4 <- renderImage({
    outfile <- tempfile(fileext='.mp4')
    anim_mp4 <- animate(p, renderer = av_renderer())
    anim_save("outfile.mp4", anim_mp4)
    
    # Return a list containing the filename
    list(src = "outfile.mp4",
         contentType = 'video/mp4'
         #width = 800, height = 800, alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
})

shinyApp(ui = ui, server = server)