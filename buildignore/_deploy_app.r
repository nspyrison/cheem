## from: https://github.com/rstudio/rsconnect/issues/88, It seems like  
## deployment needs some sort of artifact from the remotes::install_github()
## in order to work, that compiling locally does not leave so....

#1 Force reinstall spinifex and cheem from github -----
#, so that shiny sees the hidden artifact
print("first restart session to detach packages from session")
if(F){
  ## Need to give rsconnect a trail to install from rather than local installations
  remotes::install_github("nspyrison/spinifex", force = TRUE)
  remotes::install_github("nspyrison/cheem",    force = TRUE)
}

#2 Deploy app ----
if(F)
  ?rsconnect::deployApp("")
## ehh, not playing ball, do it manually:
#### opening app.r, top right button (blue circle arrows) of the file panel.
file.edit("./inst/shiny_apps/cheem_initial/app.r")
