library(plotly)
## TAKEAWAYS: ----
#- can use subplot with highlight_key
#- can use gganmiate on a highlight_key'd ggplot
#- can have different axis titles with subplots

### dev use case -----
economics_long <- tibble::tibble(economics_long, id=1:nrow(economics_long))
hk <- plotly::highlight_key(economics_long, ~id)
gg1 <- ggplot(hk, aes(date, value)) + 
  geom_point() 
ggp1 <- gg1 %>% 
  #facet_wrap(~variable, scales = "free_y", ncol = 1) +
  #labs(x= "X1", y = "Y1") %>%
  ggplotly() %>% 
  layout(xaxis = list(title = 'X1 title', showgrid = FALSE), yaxis = list(title = 'Y1 title', showgrid = FALSE))

gg2 <- ggplot(hk, aes(factor(1), value)) +
  geom_point() +
  #facet_wrap(~variable, scales = "free_y", ncol = 1) + 
  theme(axis.text = element_blank(), axis.ticks = element_blank())
ggp2 <-   ggplotly(gg2) %>% 
  layout(xaxis = list(title = 'X2 title', showgrid = FALSE), yaxis = list(title = 'Y2 title', showgrid = FALSE))
(sp <- subplot(ggp1, ggp2, shareY = FALSE, titleX = TRUE))

sp %>% 
  plotly::layout(dragmode = "select", showlegend = FALSE) %>% ## Set drag left mouse
  plotly::event_register("plotly_selected") %>%               ## Reflect "selected", on release of the mouse button.
  plotly::highlight(on = "plotly_selected", off = "plotly_deselect")

### gganimte with highlight_key is a no-go -----
mt <- tibble::tibble(mtcars, id=1:nrow(mtcars))
anim <- ggplot(plotly::highlight_key(mt, ~id), aes(mpg, disp)) +
  geom_point() +
  transition_states(gear, transition_length = 2, state_length = 1) +
  enter_fade() +
  exit_fade()

## Not run: 
# Explicitly animate using default (same as just printing the animation)
animate(anim) ### Renders, but doesn't display geom


