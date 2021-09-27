## from: https://github.com/rstudio/rsconnect/issues/88, It seems like  
## deployment needs some sort of artifact from the remotes::install_github()
## in order to work, that compiling locally does not leave so....

# #1 Delete spinifex and cheem folders ----
# if(F){ ## nvm, do it manually...
#   file.remove("../win-library/4.1/spinifex")
#   file.remove("../win-library/4.1/cheem")
# }

#2 Install spinifex and cheem from github -----
#, so that shiny sees the hidden artifact
if(F){
  remotes::install_github(
    "nspyrison/spinifex",     force = TRUE, dependencies = TRUE)
  remotes::install_github(
    "nspyrison/cheem",        force = TRUE, dependencies = TRUE)
  remotes::install_github(
    "ModelOriented/treeshap", force = TRUE, dependencies = TRUE)
}

#3 Deploy app ----
?rsconnect::deployApp("")
## ehh, whatever do it manually by opening app.r, 
#### and top right button of the file panel.