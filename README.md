# cheem

Interactively explore data- and local explanation- spaces side-by-side. Further interrogate observation explanations with a radial tour.

## Context

_Local explanations_ are an approximation of instance(observation) level variable importance to a model. That is, a point-measure of each variable's importance to the model at the particular location in the $x$-space.

`{cheem}` extracts the local explanation of every observation in a dataset, given a model. Given a model, extract the local explanation of every observation in a data set. View the data- and explanation-spaces side-by-side in an interactive shiny application. Further explored a selected point against a comparison by using its explanation as a 1D projection basis. The structure of explanation projection is then explored by a radial tour.

## Getting started

```
## Install development version from GitHub & its dependancies.
remotes::install_github("nspyrison/cheem", dependencies = TRUE)
## Run the {cheem} app including 3 preprocessed datasets
cheem::run_app()
```
### Original application

We started by looking at the model-agnostic local explantion _SHAP_ as applied to random forests. We made this choice our of concern for runtime (`{treeshap}` uses an an alternative algorithm with reduced computational complexity and thus achieves much faster run time in the _preprocessing_ step). The namesake, Cheem, stemmed from this tree-based approach. The [Cheem](https://tardis.fandom.com/wiki/Tree_of_Cheem) are a fictional race of tree-based humanoids for consistency with the Dr. who/Dr. why theme of the {DALEX} ecosystem.

### Extensions

18 Sept, 2021, Generalizing the code-base will likely take the order of:

1. Extend the scope of random forest models; from only {randomForest} to all RF models handled by {treeshap}.\
2. Extend the scope of local explanations; from {treeshap} SHAP values to all local explanation handled by {DALEX}.\

Preprocessing step and {shiny} are maturing relatively quickly.

#### Sources

[Explanatory Model Analysis (ebook)](https://ema.drwhy.ai/shapley.html#SHAPRcode) \
[DALEX CRAN page](https://CRAN.R-project.org/package=DALEX) \
[spinifex CRAN page](https://cran.r-project.org/package=spinifex) \
[treeshap GitHub page](https://github.com/ModelOriented/treeshap) \
