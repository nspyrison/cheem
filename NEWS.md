
# cheem v0.2.0 (CRAN)

## run_app changes:

- Added AmesHousing data, chocolates, and new toy simulated datasets
- Added inclusion variable to app
- Greatly reduced wording
- Added global_view yhaty plot
- Added global_view color options: log_maha.data and cor_attr.y
- Added global_view basis maps
- Added cheem_radial_tour add facet with additional fixed y of residual

## Util & other

- Major rebase of cheem_ls()
- Added linear/logistic_tforms to suggest an alpha


# cheem v0.1.0 (GitHub only, commit 283da4)

## Primary Preprocessing functions

- `default_rf()` create a `randomForest::randomForest()` with more conservative defaults.
- `attr_df_treeshap()` create `treeshap::treeshap()` local explainations of each observation.
- `cheem_ls()` create a cheem list of prepared tables for use in `run_app()`. 

## Primary visual functions

- `run_app()` which is a shiny app consuming the following two:
- `global_view()` linked 'plotly' of approximations of data- and attribution-spaces with model information. 
- `cheem_radial_tour()` create `spinifex::ggtour` of the specified radial tour. Consumed by animate_plotly, animate_gganimate, or filmstrip. 
