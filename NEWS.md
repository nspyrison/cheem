
# cheem v0.2.0 (CRAN)

## App related changes

- Added higher-level model performance to shiny app.
- In `global_view()`, added yhaty panel (residual plot/confusion matrix).
- In `global_view()`, added color options: log_maha.data and cor_attr.y.
- In `cheem_radial_tour()`, added regression case panel with additional fixed y of residual.
- In app radial tour inputs, added inclusion variable, subsetting variables used in radial tour.
- `plotly::subplot()` variants of `global_view()` & `cheem_radial_tour()`. 
- Added AmesHousing data, chocolates, and new toy simulated datasets (shiny app only).
- Reduced shiny app wording.

## Interal & util

- Major rebase of `cheem_ls()`.
- Added `linear/logistic_tform()` to suggest an alpha as a function of the number of observations.


# cheem v0.1.0 (GitHub only, commit 283da4)

## Primary Preprocessing functions

- `default_rf()` create a `randomForest::randomForest()` with more conservative defaults.
- `attr_df_treeshap()` create `treeshap::treeshap()` local explanations of each observation.
- `cheem_ls()` create a cheem list of prepared tables for use in `run_app()`. 

## Primary visual functions

- `run_app()` which is a shiny app consuming the following two:
- `global_view()` linked 'plotly' of approximations of data- and attribution-spaces with model information. 
- `cheem_radial_tour()` create `spinifex::ggtour` of the specified radial tour. Consumed by animate_plotly, animate_gganimate, or filmstrip. 
