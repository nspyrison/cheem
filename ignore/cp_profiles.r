## Example of CP profiles:
if(F)
  browseURL("https://ema.drwhy.ai/ceterisParibus.html")

titanic_imputed <- archivist::aread("pbiecek/models/27e5c")
titanic_lmr <- archivist::aread("pbiecek/models/58b24")
titanic_rf <- archivist::aread("pbiecek/models/4e0fc")
(henry <- archivist::aread("pbiecek/models/a6538"))

library("DALEX")
explain_lmr <- explain(model = titanic_lmr, 
                       data  = titanic_imputed[, -9],
                       y     = titanic_imputed$survived == "yes",
                       type = "classification",
                       label = "Logistic Regression")

library("randomForest")
explain_rf <- DALEX::explain(model = titanic_rf,
                             data  = titanic_imputed[, -9],
                             y     = titanic_imputed$survived == "yes",
                             label = "Random Forest")[-1] ## drop model; 93% of weight.

## predict a cp profile
cp_titanic_rf <- predict_profile(explainer = explain_rf, 
                                 new_observation = henry)
cp_titanic_rf

## Plot
library("ggplot2")
plot(cp_titanic_rf, variables = c("age", "fare")) +
  ggtitle("Ceteris-paribus profile", "") + ylim(0, 0.8)
