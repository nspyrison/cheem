# __cheem__

Interactively explore data- and local explanation- spaces side-by-side. Further interrogate observation explanations with a radial tour.

## Context

_Local explanations_ are an approximation of instance(observation) level variable importance to a model. That is, a point-measure of each variable's importance to the model at the particular location in the _x_-space.

`{cheem}` extracts the local explanation of every observation in a dataset, given a model. Given a model, extract the local explanation of every observation in a data set. View the data- and explanation-spaces side-by-side in an interactive shiny application. Further explored a selected point against a comparison by using its explanation as a 1D projection basis. The structure of explanation projection is then explored by a radial tour.

## Getting started

```
## Download the package
install.packages("cheem", dependencies = TRUE)
## Restart the R session so the IDE has the correct directory structure
restartSession()
## Load cheem into session
library("cheem")
## Try the app
run_app()

# Processing your data
## Install treeshap from github, to use as a local explainer
remotes::install_github('ModelOriented/treeshap') ## Local 
## Follow the examples in cheem_ls()
?cheem_ls
```

## Global view

The global view show data-, attribution-spaces, and model fit side-by-side with linked brushing and hover tooltip.

![](https://github.com/nspyrison/cheem/blob/main/ignore/global_view_toy_class.PNG?raw=true)

Through exploration of the global view, identify a primary and comparison observation to compare. For the classification task, typically a misclassified point is selected against a near-by correctly classified one. In regression we can compare a point with an extreme residual with a nearby point that is more accurately predicted.


## Radial cheem tour

The attribution of the primary observation become the 1D basis for the tour. The variable with the largest difference between the primary and comparison point's bases is selected as the manipulation variable. That is the variable whose contribution change drives the change in the projection basis.

![](https://github.com/nspyrison/cheem/blob/main/ignore/radial_cheem_tour_toy_class.gif?raw=true)

By doing this we are testing the local explanation. By testing the variable sensitivity to the structure identified in the local explanation we can better evaluate how good of an explanation it is; how sensitive it's prediction is to a change in the variable contributions.


### Original application

We started by looking at the model-agnostic local explanation _SHAP_ as applied to random forests. We made this choice out of concern for runtime ({treeshap} uses an  alternative algorithm with reduced computational complexity and thus achieves much faster run time extracting the full SHAP matrix during the preprocessing step). The namesake, Cheem, stems from the original application to tree-based models. The [Cheem](https://tardis.fandom.com/wiki/Tree_of_Cheem) are a fictional race of tree-based humanoids for consistency with the Dr. who/Dr. why theme of the {DALEX} ecosystem.

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
