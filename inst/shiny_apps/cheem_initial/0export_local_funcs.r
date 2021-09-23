setwd("~/R/trees_of_cheem")
source("./apps/trees_of_cheem.r") ## Local functions, esp. for basis_cheem() and view_cheem()
source("./apps/cobs_n_plot_funcs.r") ## COBS func, and plotting functions for shiny

if(interactive()){
  save(proto_basis1d_distribution, ## from: file.edit("./apps/trees_of_cheem.r")
       basis_local_attribution,    ## from: file.edit("./apps/trees_of_cheem.r")
       radial_cheem_ggtour,         ## from: file.edit("./apps/cobs_n_plot_funcs.r")
       linked_plotly_func,         ## from: file.edit("./apps/cobs_n_plot_funcs.r")
       file = "0local_funcs.RData")
  file.copy("./0local_funcs.RData", to = "./apps/cheem_app/data/0local_funcs.RData", overwrite = TRUE)
  file.remove("./0local_funcs.RData")
}
