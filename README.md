# Work In Progress

## Caution: Not yet handle missing value as lgb.model.dt.tree

## lightgbmExplainer
An R package that makes LightGBM models fully interpretable

### Example 
```
# Train a model
library(lightgbm)
data(agaricus.train, package = "lightgbm")
lgb.train <- agaricus.train
lgb.dtrain <- lgb.Dataset(lgb.train$data, label = lgb.train$label)
lgb.params = list(objective = "binary")
lgb.model <- lgb.train(lgb.params, lgb.dtrain, 3)
lgb.trees <- lgb.model.dt.tree(lgb.model)

# Explain
library(lightgbmExplainer)
explainer = buildExplainer(lgb.model,lgb.train$data, type="binary", base_score = 0.5)
pred.breakdown = explainPredictions(lgb.model, explainer, lgb.train$data)
predict(lgb.model,lgb.train$data)[8]
predict(lgb.model,lgb.train$data, rawscore = T)[8]
showWaterfall(lgb.model, explainer, lgb.dtrain, lgb.train$data,  2, type = "binary")
showWaterfall(lgb.model, explainer, lgb.dtrain, lgb.train$data,  8, type = "binary")
```

tested with lightgbm v2.0.7

Take reference from [xgboostExplainer](https://github.com/AppliedDataSciencePartners/xgboostExplainer) and credit to David Foster.

TODO: Handle <= > == != (labeled by decision instead of value)
TODO: Handle Missing Value in lgb.model.dt.tree
