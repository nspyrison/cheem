# Create some output for Readme ----
library(cheem)
library(spinifex)

## Classification:
X    <- penguins_na.rm[, 1:4]
clas <- penguins_na.rm$species
Y    <- as.integer(clas)

## Cheem
peng_chm <- cheem_ls(X, Y, penguin_xgb_shap, penguin_xgb_pred, clas,
                     label = "Penguins, xgb, shapviz")

prim <- 243
comp <- 169
bas  <- sug_basis(penguin_xgb_shap, rownum = prim)
mv   <- sug_manip_var(penguin_xgb_shap, primary_obs = prim, comp)
ggt  <- radial_cheem_tour(peng_chm, basis = bas, manip_var = mv,
                          primary_obs = prim, comparison_obs = comp, angle = .10)

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

