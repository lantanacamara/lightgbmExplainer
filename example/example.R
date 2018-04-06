###### LightGBM ###########
# Train a model
library(lightgbm)
data(agaricus.train, package = "lightgbm")
lgb.train <- agaricus.train
# Test NA
data_with_na <- lgb.train$data
data_with_na[2,3] <- NA
data_with_na[2,56] <- NA
lgb.dtrain <- lgb.Dataset(data_with_na, label = lgb.train$label)
# lgb.dtrain <- lgb.Dataset(lgb.train$data, label = lgb.train$label)
lgb.params = list(objective = "binary")
lgb.model <- lgb.train(lgb.params, lgb.dtrain, 3)
lgb.trees <- lgb.model.dt.tree(lgb.model)
head(lgb.trees)

library(lightgbmExplainer)
explainer = buildExplainer(lgb.trees, colnames(lgb.train$data))
pred.breakdown = explainPredictions(lgb.model, explainer, lgb.train$data)
predict(lgb.model,data_with_na)[2]
predict(lgb.model,data_with_na, rawscore = T)[2]
predict(lgb.model,data_with_na, predleaf = T)[2,]
showWaterfall(lgb.model, explainer, lgb.dtrain, data_with_na,  2, type = "binary")
showWaterfall(lgb.model, explainer, lgb.dtrain, lgb.train$data,  8, type = "binary")


##### Xgboost ######
library(xgboost)

data(agaricus.train, package='xgboost')
xgb.train <- agaricus.train

xgb.train.data <- xgb.DMatrix(as.matrix(xgb.train$data), label = xgb.train$label)

xgb.param <- list(objective = "binary:logistic")
xgb.model <- xgboost(param =xgb.param,  data = xgb.train.data, nrounds=3)
xgb.col_names = colnames(xgb.train$data)

xgb.trees = xgb.model.dt.tree(xgb.col_names, model = xgb.model)

#### The XGBoost Explainer example
library(xgboostExplainer)
xgb.explainer = buildExplainer(xgb.model,xgb.train.data, type="binary", base_score = 0.5)
xgb.pred.breakdown = explainPredictions(xgb.model, xgb.explainer, xgb.train.data)

showWaterfall(xgb.model, xgb.explainer, xgb.train.data, as.matrix(xgb.train$data),  2, type = "binary")
showWaterfall(xgb.model, xgb.explainer, xgb.train.data, as.matrix(xgb.train$data),  8, type = "binary")
predict(xgb.model, as.matrix(xgb.train$data))[8]
