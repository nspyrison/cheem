### Preprocessing data for shiny app:
{
  tictoc::tic("Preprocess all")
  wd <- getwd()
  fp <- "./inst/shiny_apps/cheem_initial/preprocessing/"
  if(substr(wd, nchar(wd) - 4, nchar(wd)) != "cheem")
    warning("work directory is not set to cheem!")
  if(F) 
    dir("./inst/shiny_apps/cheem_initial/preprocessing")
  
  ## Classification:
  source(paste0(fp, "preprocess_toy_classification.r"))
  source(paste0(fp, "preprocess_chocolates.r"))
  source(paste0(fp, "preprocess_penguins.r"))
  ## Regression:
  source(paste0(fp, "preprocess_toy_quad_regression.r"))
  source(paste0(fp, "preprocess_toy_trig_regression.r"))
  source(paste0(fp, "preprocess_toy_mixture_regression.r"))
  source(paste0(fp, "preprocess_fifa.r"))
  source(paste0(fp, "preprocess_ames2018.r"))
  rm("this_ls")
  
  if(F){ ## Open individual files.
    file.edit(paste0(fp, "preprocess_penguins.r"))
    file.edit(paste0(fp, "preprocess_toy_classification.r"))
    file.edit(paste0(fp, "preprocess_fifa.r"))
    file.edit(paste0(fp, "preprocess_ames2018.r"))
    file.edit(paste0(fp, "preprocess_toy_quad_regression.r"))
    file.edit(paste0(fp, "preprocess_toy_trig_regression.r"))
    file.edit(paste0(fp, "preprocess_chocolates.r"))
  }
  tictoc::toc()
  beepr::beep(4)
}
