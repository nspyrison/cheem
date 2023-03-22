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

bas <- sug_basis(shap_df, rownum = 1)
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
                rug_shape = 3) +
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
animate_gganimate(ggt, render = gganimate::ffmpeg_renderer())

