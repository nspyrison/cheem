# {cheem}, R package

Interactively explore data- and local explanation- spaces side-by-side. Further interrogate observation explanations with a radial tour.

## Context

_Local explanations_ are an approximation of instance(observation) level variable importance to a model. That is, a point-measure of each variable's importance to the model at the particular location in the $x$-space.

`{cheem}` extracts the local explanation of every observation in a dataset, given a model. Given a model, extract the local explanation of every observation in a data set. View the data- and explanation-spaces side-by-side in an interactive shiny application. Further explored a selected point against a comparison by using its explanation as a 1D projection basis. The structure of explanation projection is then explored by a radial tour.

## Getting started

```
## Github dependancies:
remotes::install_github('ModelOriented/treeshap')
remotes::install_github("nspyrison/spinifex")

## Install cheem development version & its CRAN dependancies.
remotes::install_github("nspyrison/cheem", dependencies = TRUE)
## Run the {cheem} app including 3 preprocessed datasets
cheem::run_app()

## Preprocess your own dataset:
## Follow along with the cheem_ls() exampes:
?cheem_ls
```

<!-- Alternatively, try out hosted shiny app at [ebsmonash.shinyapps.io/cheem_initial](https://ebsmonash.shinyapps.io/cheem_initial/). -->

### Original application

We started by looking at the model-agnostic local explanation _SHAP_ as applied to random forests. We made this choice out of concern for runtime (`{treeshap}` uses an  alternative algorithm with reduced computational complexity and thus achieves much faster run time extracting the full SHAP matrix during the preprocessing step). The namesake, Cheem, stems from the original application to tree-based models. The [Cheem](https://tardis.fandom.com/wiki/Tree_of_Cheem) are a fictional race of tree-based humanoids for consistency with the Dr. who/Dr. why theme of the {DALEX} ecosystem.

<!---
### Extensions

18 Sept, 2021, Generalizing the code-base will likely take the order of:

1. Extend the scope of random forest models; from only {randomForest} to all RF models handled by {treeshap}.\
2. Extend the scope of local explanations; from {treeshap} SHAP values to all local explanations handled by {DALEX}.\
--->


#### Sources

[Explanatory Model Analysis (ebook)](https://ema.drwhy.ai/shapley.html#SHAPRcode) \
[DALEX CRAN page](https://CRAN.R-project.org/package=DALEX) \
[spinifex CRAN page](https://cran.r-project.org/package=spinifex) \
[treeshap GitHub page](https://github.com/ModelOriented/treeshap) 
