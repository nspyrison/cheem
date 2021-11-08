# FIFA regression, save layer_ls for app -----
## Dependencies ------
require("cheem")
require("YaleToolkit")
help(package = "YaleToolkit") ## No new haven data 
data() ## No new haven data 

# ## Setup ------
# .raw <- DALEX::apartments ## 1000 x 6. Y | 4 Xs | 1 class
# Y <- .raw[,1]
# X <- .raw[,2:5]
# clas <- .raw[,6]
# #hist(Y) ## Slight right skew
# 
# ## Create an ordered level of district; its rank of increasing sd.
# require(dplyr)
# .agg <- .raw %>% 
#   group_by(district) %>%
#   dplyr::summarise(mean_price = mean(m2.price),
#                    sd_price = sd(m2.price),
#                    median_price = median(m2.price)) %>%
#   ungroup()
# require(ggplot2)
# ## ~ 4 levels of sd, 3 levels of price.
# if(F)
#   ggplot(.agg, aes(mean_price, sd_price)) + geom_point()
# .agg <- .agg %>%
#   arrange(sd_price) %>%
#   mutate(district_sd_rank = 1:n())
# lj <- left_join(.raw, .agg, by = "district")
# X <- lj[, c(2:5, 10)]
# 
# ## cheem_ls -----
# ## Error in solve.default(cov, ...) :
# # not singular for pca or something?
# cheem_ls <- cheem_ls(
#   x = X, y = Y, basis_type = "pca", class = clas)
# names(layer_ls)
# 
# 
# ## EXPORT OBJECTS ----
# if(interactive()){
#   setwd("~/R/cheem")
#   save(cheem_ls,
#        file = "./inst/shiny_apps/cheem_initial/data/4preprocess_apartments.RData")
# }
# if(F) ## Not run, load cheem_ls
#   load("./inst/shiny_apps/cheem_initial/data/4preprocess_apartments.RData")
# 
