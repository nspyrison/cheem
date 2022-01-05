library(treeshap)


##randomForest.unify ----
?randomForest.unify
library(randomForest)
data_fifa <- fifa20$data[!colnames(fifa20$data) %in%
                           c('work_rate', 'value_eur', 'gk_diving', 'gk_handling',
                             'gk_kicking', 'gk_reflexes', 'gk_speed', 'gk_positioning')]
data <- na.omit(cbind(data_fifa, target = fifa20$target))

rf <- randomForest::randomForest(target~., data = data, maxnodes = 10, ntree = 10)
unified_model <- randomForest.unify(rf, data)
# shaps <- treeshap(unified_model, data[1:2,])
# plot_contribution(shaps, obs = 1)

## ranger.unify -----
?ranger.unify
library(ranger)
data_fifa <- fifa20$data[!colnames(fifa20$data) %in%
                           c('work_rate', 'value_eur', 'gk_diving', 'gk_handling',
                             'gk_kicking', 'gk_reflexes', 'gk_speed', 'gk_positioning')]
data <- na.omit(cbind(data_fifa, target = fifa20$target))

rf <- ranger::ranger(target~., data = data, max.depth = 10, num.trees = 10)
unified_model2 <- ranger.unify(rf, data)
# shaps <- treeshap(unified_model, data[1:2,])
# plot_contribution(shaps, obs = 1)

## Comparison of unifed objects
str(unified_model)
.m <- sapply(seq_along(unified_model), function(i){
  names_eq <- all(names(unified_model[[i]]) == names(unified_model2[[i]]))
  dims_eq  <- all(ncol( unified_model[[i]]) == ncol( unified_model2[[i]])) ## note rows not equal.
  print(paste0("i=", i,", names equal: ", names_eq, ", ncol equal: ", dims_eq))
})
