# TAKE AWAYS ----
## - profvis: 8.2-10 sec, tictoc: ~13 (inside the profvis).
## - gc() is called about 10 times, each taking 170-330 ms each! remove or gut from spinifex. 
## -- removing gc() from spinifex: 6 sec (profvis), 9.5 (tictoc)


library(profvis) ## Line profile, the radial_cheem_tour examples
library(cheem)
library(spinifex)

## Classification:
X    <- penguins_na.rm[, 1:4]
clas <- penguins_na.rm$species
Y    <- as.integer(clas)

## start prof

profvis(
  {
    tictoc::tic("inside profvis")
    rf_fit  <- default_rf(X, Y)
    ## Long runtime for full datasets or complex models:
    shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
    this_ls <- cheem_ls(X, Y, class = clas,
                        model = rf_fit,
                        attr_df = shap_df)
    
    bas <- basis_attr_df(shap_df, rownum = 1)
    ggt <- radial_cheem_tour(this_ls, basis = bas, manip_var = 1,
                             primary_obs = 1, comparison_obs = 2)
    animate_plotly(ggt)
    if(FALSE) ## or animate with gganimate
      animate_gganimate(ggt) #, render = gganimate::av_renderer())
    
    ## Regression:
    dat  <- amesHousing2018_NorthAmes
    X    <- dat[, 1:9]
    Y    <- log(dat$SalePrice)
    clas <- dat$SubclassMS
    
    rf_fit  <- default_rf(X, Y)
    ## Long runtime for full datasets or complex models:
    shap_df <- attr_df_treeshap(rf_fit, X, noisy = FALSE)
    this_ls <- cheem_ls(X, Y, class = clas,
                        model = rf_fit,
                        attr_df = shap_df)
    
    bas <- basis_attr_df(shap_df, rownum = 1)
    ggt <- radial_cheem_tour(this_ls, basis = bas, manip_var = 1, angle = .25)
    
    animate_plotly(ggt)
    if(FALSE){ ## or animate with gganimate
      gif <- animate_gganimate(ggt)
      beepr::beep()
      mp4 <- animate_gganimate(ggt, renderer = gganimate::av_renderer())
      beepr::beep()
    }
    tictoc::toc()
  }
)

if(F){
  ## Save gif
  gganimate::anim_save("tour_penguins.gif", animation = gif, path = "./buildignore")
  ## Save mp4
  gganimate::anim_save("tour_penguins.mp4", animation = mp4, path = "./buildignore")
  
}
