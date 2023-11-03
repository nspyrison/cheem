# Create some output for Readme ----

library(cheem)
library(spinifex)

if(F) ## Attempting to parallelize, small test but didn't look 11x.
  browseURL(
    "https://stackoverflow.com/questions/67321487/how-to-use-multiple-cores-to-make-gganimate-faster")
n_cores <- future::availableCores() ## 12 on Acer laptop
future::plan("multiprocess", workers = n_cores - 1)

## Classification:
X    <- penguins_na.rm[, 1:4]
clas <- penguins_na.rm$species
Y    <- as.integer(clas)
colnames(X) <- c("bl", "bd", "fl", "bm")

## Cheem
peng_chm <- cheem_ls(X, Y, penguin_xgb_shap, penguin_xgb_pred, clas,
                     label = "Penguins, xgb, shapviz")

bas <- sug_basis(penguin_xgb_shap, rownum = 1)
mv  <- which(colnames(X) == "fl")
ggt <- radial_cheem_tour(peng_chm, basis = bas, manip_var = mv,
                         primary_obs = 243, comparison_obs = 169, angle = .10)

prim <- 243
comp <- 256
global_view(peng_chm, primary_obs = prim, comparison_obs = comp)
bas <- sug_basis(penguin_xgb_shap, prim)
mv  <- sug_manip_var(penguin_xgb_shap, primary_obs = prim, comparison_obs = comp)
ggt <- radial_cheem_tour(peng_chm, basis = bas, manip_var = mv, prim, comp)
animate_plotly(ggt)
if(interactive()){
  ## Render gif
  gif <- animate_gganimate(
    ggt, height = 2, width = 6, units = "in", res = 150) #, render = gganimate::av_renderer())
  ## Save gif
  gganimate::anim_save("tour_penguins.gif", animation = gif, path = "./buildignore")
  beepr::beep()
  
  ## Render mp4
  # mp4 <- animate_gganimate(
  #   ggt, height = 2, width = 4.5, units = "in", res = 150,
  #   render = gganimate::av_renderer())
  ## Save mp4
  # gganimate::anim_save("tour_penguins.mp4", animation = mp4, path = "./ignore")
  # beepr::beep()
}
tictoc::toc()
## , YMMV
