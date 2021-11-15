## Dependencies ------
## Local files
require("cheem")

{
  d <- ggplot2::diamonds
  s <- d[d$clarity == "VS1",] ## 8171
  s <- s[s$color == "J",] ##542
  s <- s[, c(7, 1, 5, 6, 8:10, 2)]
  diamonds_sub <- data.frame(log_price = log(s$price), s[,-1])
}
## Visualize
if(F)
  GGally::ggpairs(diamonds_sub, mapping = aes(color = cut))

Y <- diamonds_sub$log_price
X <- diamonds_sub[,2:7]
clas <- diamonds_sub$cut

## cheem_ls -----
# ERROR HERE: object 'y.1' not found. Rstudio won't let me select anything....
debugonce(cheem_ls)
cheem_ls <- cheem_ls(
  x = X, y = Y, basis_type = "pca", class = clas)
names(cheem_ls)

## EXPORT OBJECTS ----
if(interactive() == TRUE){
  setwd("~/R/cheem")
  save(cheem_ls,
       file = "./inst/shiny_apps/cheem_initial/data/5preprocess_diamond_subset.RData")
}
if(F) ## Not run, load cheem_ls
  load("./inst/shiny_apps/cheem_initial/data/5preprocess_diamond_subset.RData")

