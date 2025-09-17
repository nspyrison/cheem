# cheem v0.4.2

- Removed the function `rnorm_from` and lqmm from suggests; lqmm (matrix math) was removed from CRAN, would need to introduce 2 new packages to keep unused utility function.
- ggplot2 (v4.0.0) pushed breaking changes as they transition to S7 objects; changed tests from expect_equal(class(x), c("gg", "ggplot")) to expect_true(inherits(x, c("ggplot", "ggplot2::ggplot"))) according to tidyverse/ggplot2#6498.
- Fixed a 4x more visualization tests.


# cheem v0.4.1

- ggplot2 is pushing breaking changes as they transition to S7 objects; changed tests from expect_equal(class(x), c("gg", "ggplot")) to expect_true(inherits(x, c("ggplot", "ggplot2::ggplot"))) according to tidyverse/ggplot2#6498.
- Updated two ames housing urls as jse.amstat.org urls moved to https.

# cheem v0.4.0

- Repaired packagedown site!
- Fixed news on packagedown site.
- Shiny app has go buttons rather than waiting after every input change.
- Shiny app text, plot dimensions, and text cleaned up.
- Classification tour now uses a horizontal layout.
- Cleaned up the text on the facet panels for `global_tour()` and `radial_cheem_tour()`.
- Removed support for the `basis_type` argument. support for alternative bases types is really an extension of the analysis.
- Recreate the saved classification model, they fit too well to work as illustrations.
- Set seed more consistently. All model and attribution shifted a bit, but will be more replicable going forward.
- Minor documentation and code clean up and clarifications.


# cheem v0.3.0 -- Generalized for any attribution

- Rebase all functions from expecting a unified `treeshap::shap()` to generalized
data frame or matrix format for arbitrary attribution spaces.
- Rework vignette and examples to reflect this change.
- Added precomputed predictions and attributions for the Ames, Chocolates, and Penguins datasets. This allows users to run attribution-agnostic functions without dependencies.
- Add `subset_cheem()`, a convenience function for subsetting cheem lists after construction.
- Removed plotly subplot variations of visuals: `global_view_subplots()`, `radial_cheem_tour_subplots()`. These were development variations never used in the shiny app.
- Minor function renames for parsimony and consistency.


# cheem v0.2.0 (CRAN releases here on out)

## App related changes

- Added vignette: _Getting started with cheem_.
- Added __pkgdown__ site: https://nspyrison.github.io/cheem/.
- Added global model performance metrics to shiny app.
- In `global_view()`, added yhaty panel (residual plot/confusion matrix).
- In `global_view()`, added color options: log_maha.data and cor_attr.y.
- In `cheem_radial_tour()`, added regression case panel with additional fixed y of residual.
- In app radial tour inputs, added inclusion variable, subsetting variables used in radial tour.
- `plotly::subplot()` variants of `global_view()` & `cheem_radial_tour()`. 
- Added AmesHousing data, chocolates, and new toy simulated datasets (shiny app only).
- Reduced shiny app wording.

## Internal & utilities

- Major rebase of `cheem_ls()`.
- Added `linear/logistic_tform()` to suggest an alpha as a function of the number of observations.


## Sourcing __treeshap__

- __drat__ repository hosting __treeshap__ did not work with debian and window rhub platforms;
- Minimally ported functions and cpp source files with @author & @source. Changed examples for consistency and smaller code base support.
- as of v0.3.0, cheem was generalized to all local variable attributions, so this is not a concern.


# cheem v0.1.0 (GitHub only, commit 283da4)

## Primary preprocessing functions

- `default_rf()` create a `randomForest::randomForest()` with more conservative defaults.
- `attr_df_treeshap()` create `treeshap::treeshap()` local explanations of each observation.
- `cheem_ls()` create a cheem list of prepared tables for use in `run_app()`.

## Primary visual functions

- `run_app()` which is a shiny app consuming the following two:
- `global_view()` linked 'plotly' of approximations of data- and attribution-spaces with model information. 
- `cheem_radial_tour()` create `spinifex::ggtour` of the specified radial tour. Consumed by animate_plotly, animate_gganimate, or filmstrip.
