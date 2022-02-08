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
colnames(X) <- c("b_l", "b_d", "f_l", "b_m")

rf_fit  <- default_rf(X, Y)
## Long runtime for full datasets or complex models:
shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
this_ls <- cheem_ls(X, Y, class = clas,
                    model = rf_fit,
                    attr_df = shap_df)

bas <- basis_attr_df(shap_df, rownum = 1)
mv  <- which(colnames(penguins_ls$attr_df) == "f_l")
ggt <- radial_cheem_tour(this_ls, basis = bas, manip_var = mv,
                         primary_obs = 243, comparison_obs = 169, angle = .25)
if(interactive()){
  ## Render gif
  gif <- animate_gganimate(
    ggt, height = 2, width = 4.5, units = "in", res = 150) #, render = gganimate::av_renderer())
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
