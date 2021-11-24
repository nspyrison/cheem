## Testing plotly existance issues, esp with frame cor2
## Setup ----
require(spinifex)
require(cheem)
#?cheem::radial_cheem_tour()

sub <- amesHousing2018_NorthAmes[1:200, ]
X <- sub[, 1:9]
Y <- log(sub$SalePrice)
clas <- sub$SubclassMS

## Does adding Sys.sleep make RStudio lesss likely to crash?
rf_fit  <- default_rf(X, Y); Sys.sleep(.01);
shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE); Sys.sleep(.01);
this_ls <- cheem_ls(X, Y, class = clas,
                    model = rf_fit,
                    attr_df = shap_df); Sys.sleep(.01);

bas <- basis_attr_df(shap_df, rownum = 1)
# Error in paste(group, frame, sep = "-") : object 'group' not found
#debugonce(radial_cheem_tour)
ggt <- radial_cheem_tour(
  this_ls, basis = bas, manip_var = 1, angle = .5)

## Error in the regression case ----
animate_plotly(ggt)
## Error:
# Error in paste(group, frame, sep = "-") : object 'group' not found 
animate_gganimate(ggt) ## toggles back and forth (with frame cor2)
## Error:
# Error in `$<-.data.frame`(`*tmp*`, "group", value = "") : 
#   replacement has 1 row, data has 0 

## Trouble shooting radial_cheem_tour errors:
#debugonce(radial_cheem_tour)
ggt <- radial_cheem_tour( 
  this_ls, basis = bas, manip_var = 1, angle = .5)



## from primatives ----
?facet_wrap_tour

library(cheem)
library(spinifex)
dat     <- scale_sd(flea[, 1:6])
clas    <- flea$species
bas     <- basis_pca(dat)
mv      <- manip_var_of(bas)
mt_path <- manual_tour(bas, manip_var = mv)

### Facet and density -----
ggt <- ggtour(mt_path, dat, angle = .3) +
  facet_wrap_tour(facet_var = clas, ncol = 2, nrow = 2) +
  proto_density(aes_args = list(color = clas, fill = clas, shape = clas),
                rug_shape = NULL) +
  proto_basis1d("floor1d") +
  proto_origin1d()

animate_plotly(ggt) ## removing rugs helped in this case.
animate_gganimate(ggt, render = gganimate::ffmpeg_renderer())

### facet and append y -----
dummy_y <- scale_sd(as.integer(clas) + rnorm(nrow(dat), 0, .5))

ggt <- ggtour(mt_path, dat, angle = .3) +
  facet_wrap_tour(facet_var = clas, ncol = 2, nrow = 2) +
  append_fixed_y(fixed_y = dummy_y) +
  proto_point(list(fill = clas, color = clas)) +
  proto_basis1d(position = "floor1d") +
  proto_origin()

message("removing rugs helped in this case.")
animate_plotly(ggt)
animate_gganimate(ggt, render = gganimate::ffmpeg_renderer())

### facet, append y, and frame_cor -----
ggt <- ggtour(mt_path, dat, angle = .3) +
  facet_wrap_tour(facet_var = clas, ncol = 2, nrow = 2) +
  append_fixed_y(fixed_y = dummy_y) +
  proto_frame_cor2(position = c(.5, 1.1)) +
  proto_basis1d(position = "floor1d") +
  proto_origin() +
  proto_point(list(fill = clas, color = clas))
  #proto_frame_cor2(position = c(.5, 1.1))

message("order of frame cor is an issue; if we remove facet does that help?")
animate_plotly(ggt)
animate_plotly2(ggt)
animate_gganimate(ggt, render = gganimate::ffmpeg_renderer())

### append y, and frame_cor -----
ggt <- ggtour(mt_path, dat, angle = .3) +
  #facet_wrap_tour(facet_var = clas, ncol = 2, nrow = 2) +
  append_fixed_y(fixed_y = dummy_y) +
  proto_frame_cor2(position = c(.5, 1.1)) +
  proto_point(list(fill = clas, color = clas)) +
  proto_basis1d(position = "floor1d") +
  proto_origin()
  

animate_plotly(ggt)
animate_plotly2(ggt)
animate_gganimate(ggt, render = gganimate::ffmpeg_renderer())


## Changes redraw to false.
animate_plotly2 <- function(
  ggtour,
  fps = 8,
  ... ## Passed to plotly::layout().
){
  ## Frame asymmetry issue: https://github.com/ropensci/plotly/issues/1696
  #### Adding many protos is liable to break plotly animations, see above url.
  ## Assumptions
  if(length(ggtour$layers) == 0L) stop("No layers found, did you forget to add a proto_*?")
  n_frames <- length(unique(last_ggtour()$df_basis$frame))
  ## 1 Frame only:
  if(n_frames == 1L){
    warning("ggtour df_basis only has 1 frame, applying just plotly::ggplotly instead.")
    anim <- plotly::ggplotly(p = ggtour, tooltip = "tooltip") %>%
      ## Remove button bar and zoom box
      plotly::config(displayModeBar = FALSE) %>%
      ## Remove legends and axis lines
      plotly::layout(showlegend = FALSE, dragmode = FALSE,
                     #, fixedrange = TRUE ## This is a curse, do not use.
                     yaxis = list(showgrid = FALSE, showline = FALSE),
                     xaxis = list(showgrid = FALSE, showline = FALSE,
                                  scaleanchor = "y", scalaratio = 1L),
                     ...)
  }else{
    ## More than 1 frame:
    
    ## Block plotly.js warning: lack of support for horizontal legend;
    #### https://github.com/plotly/plotly.js/issues/53
    anim <- suppressWarnings(
      plotly::ggplotly(p = ggtour, tooltip = "tooltip") %>%
        plotly::animation_opts(frame = 1L / fps * 1000L,
                               transition = 0L, redraw = FALSE) %>%
        plotly::animation_slider(
          active = 0L, ## 0 indexed first frame
          currentvalue = list(prefix = "Frame: ", font = list(color = "black"))
        ) %>%
        ## Remove button bar and zoom box
        plotly::config(displayModeBar = FALSE,
                       modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d"))) %>%
      ## Remove legends and axis lines
      plotly::layout(showlegend = FALSE, dragmode = FALSE,
                     #, fixedrange = TRUE ## This is a curse, do not use.
                     yaxis = list(showgrid = FALSE, showline = FALSE),
                     xaxis = list(showgrid = FALSE, showline = FALSE,
                                  scaleanchor = "y", scalaratio = 1L),
                     ...)
  }
  
  ## Clean up
  .set_last_ggtour(NULL) ## Clears last tour
  ## This should prevent some errors from not running ggtour() right before animating it.
  .m <- gc() ## Mute garbage collection
  
  return(anim)
}
  