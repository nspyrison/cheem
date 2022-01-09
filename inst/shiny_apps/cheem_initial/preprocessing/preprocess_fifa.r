## Dependencies ------
{
  require(cheem)
  s <- function(sec = .01)Sys.sleep(sec)
  
  ## Setup ------
  .raw <- DALEX::fifa
  .dat_less_ys <- .raw %>%
    dplyr::select(
      -c(`nationality`, ## useless class
         ## potential target vars:
         `overall`, `potential`, `value_eur`, `wage_eur`)) %>%
    as.data.frame()
  
  if(F) ## Don't run, View corrplot
    corrplot::corrplot(cor(.dat_less_ys),
                       method = "circle", ## geom
                       type = "upper", ## only upper triangle
                       diag = F, ## remove auto correlation
                       order = "FPC", ## First principal component
                       tl.col = "black", tl.srt = 90, ## Text label color and rotation
                       tl.pos = "td")
  
  ## Munging aspects -----
  #### Agg some highly correlated vars.
  dat <- .dat_less_ys %>%
    dplyr::mutate(
      .keep = "none",
      bdy = (weight_kg+(height_cm/100L)^2L)/2L, ## bmi wasn't working well after 01 scaling.
      age = age,
      react = movement_reactions,
      off = (attacking_finishing+skill_long_passing+attacking_volleys+
               power_long_shots+skill_curve+mentality_positioning+attacking_crossing+
               attacking_short_passing+skill_dribbling+skill_ball_control)/10L,
      def = (defending_sliding_tackle+mentality_interceptions+
               defending_standing_tackle+defending_marking+mentality_aggression)/5L,
      acc = (attacking_heading_accuracy+power_shot_power)/2L,
      mvm = (movement_sprint_speed+movement_balance+movement_acceleration+
               mentality_vision+mentality_composure+movement_agility+
               mentality_penalties+skill_fk_accuracy+power_stamina+movement_reactions)/10L,
      pwr = (power_strength+power_jumping)/2L,
      gk = (goalkeeping_diving+goalkeeping_positioning+goalkeeping_reflexes+
              goalkeeping_handling+goalkeeping_kicking)/5L
    )
  ## Class for the position of the player, either a "fielder" or "goalkeeper"
  position <- clas <- dplyr::case_when(
    dat$gk <= 40L ~ "fielder",
    dat$gk >  40L ~ "goalkeeper") %>%
    factor(levels = c("fielder", "goalkeeper"))
}

## Starting with 42 variables, we remove `nationality`, and some potential Y vars,
#### and aggregate into 9 aggregate 'aspect' dimensions based on var correlation 
X <- dat ## 9 aspects of the X's
Y <- .raw$wage_eur ## wages in Euros, assumed 2020 valuation.
## removing log, model and explanation should be scale invariant
if(F)
  hist(Y)

rf_fit  <- default_rf(X, Y, verbose = TRUE); s();
shap_df <- attr_df_treeshap(rf_fit, X, verbose = TRUE); s();
this_ls <- cheem_ls(X, Y, class = clas,
                    model = rf_fit,
                    attr_df = shap_df, verbose = TRUE)
names(this_ls)

## Thin data, after model/layer_ls ----
## V2 is observed maha
{
  .orig_ls <- this_ls ## backup
  ## THIN: just first 500 (top skill overall/potential) (as messi and van Dijk are 1 & 8)
  .rownums_to_keep <- 1:500
  ## Order of maha numbers is not correct atm
  this_ls$global_view_df <-
    this_ls$global_view_df[this_ls$global_view_df$rownum %in% .rownums_to_keep,]
  this_ls$decode_df <-
    this_ls$decode_df[this_ls$decode_df$rownum %in% .rownums_to_keep,]
  this_ls$attr_df <- this_ls$attr_df[.rownums_to_keep,]
  length(.rownums_to_keep)
}

## EXPORT OBJECTS ----
saveRDS(this_ls, file = "~/R/cheem/inst/shiny_apps/cheem_initial/data/preprocess_fifa.rds")
cat("Saved.\n")
if(F) ## Not run, load this_ls
  this_ls <- readRDS("./inst/shiny_apps/cheem_initial/data/preprocess_fifa.rds")

