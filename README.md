## lightgbmExplainer
An R package that makes LightGBM models fully interpretable

### Example 
```
library(lightgbm) # v2.1.0 or above
library(lightgbmExplainer)

# Load Data
data(agaricus.train, package = "lightgbm")
# Train a model
lgb.dtrain <- lgb.Dataset(agaricus.train$data, label = agaricus.train$label)
lgb.params <- list(objective = "binary")
lgb.model <- lgb.train(lgb.params, lgb.dtrain, 5)
# Build Explainer
lgb.trees <- lgb.model.dt.tree(lgb.model) # First get a lgb tree
explainer <- buildExplainer(lgb.trees)
# compute contribution for each data point
pred.breakdown <- explainPredictions(lgb.model, explainer, agaricus.train$data)
# Show waterfall for the 8th observation
showWaterfall(lgb.model, explainer, lgb.dtrain, agaricus.train$data,  8, type = "binary")
```

Take reference from [xgboostExplainer](https://github.com/AppliedDataSciencePartners/xgboostExplainer) and credit to David Foster.

Note: LightGBM provides similar function *lgb.interprete* and *lgb.plot.interpretation*. *lgb.interprete* could be faster if you only want to interprete a few data point, but it could be much slower if you want to interprete many data point.
