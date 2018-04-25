library(data.table)

################## lightgbmExplainer Exaxmple ##################
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
# Check if the result is correct
compare <- merge(
  melt(pred.breakdown[8], measure.vars = colnames(pred.breakdown),
     variable.name = "Feature", value.name="Explainer"),
  lgb.interprete(lgb.model,agaricus.train$data,8),
  sort = F)
all(with(compare, Explainer == Contribution))

################## XGBoost Explainer Exaxmple ##################

library(xgboost)
library(xgboostExplainer)

xgb.train <- agaricus.train
xgb.train.data <- xgb.DMatrix(as.matrix(xgb.train$data), label = xgb.train$label)
xgb.param <- list(objective = "binary:logistic")
xgb.model <- xgboost(param =xgb.param,  data = xgb.train.data, nrounds=5)
xgb.col_names = colnames(xgb.train$data)
xgb.trees = xgb.model.dt.tree(xgb.col_names, model = xgb.model)
#### The XGBoost Explainer example
xgb.explainer = buildExplainer(xgb.model,xgb.train.data, type="binary", base_score = 0.5)
xgb.pred.breakdown = explainPredictions(xgb.model, xgb.explainer, xgb.train.data)
showWaterfall(xgb.model, xgb.explainer, xgb.train.data, as.matrix(xgb.train$data),  8, type = "binary")

################## Profiling with custom data (Don't Run) ##################
# Profiling
library(readr)
target_data <- read_rds("../../temp/lightgbmexplainer/2018-04-25/target_data.rds")
lgb.model <- lgb.load("../../temp/lightgbmexplainer/2018-04-25/lightgbm_v2_4_2_2.txt")
library(profvis)
# 8040ms/654.7MB
profvis(lgb.trees <- lgb.model.dt.tree(lgb.model))
# 23380ms/3136.3MB (v0.1 benchmark - 142150ms/8134.5MB (lgb.model.dt.tree excluded))
profvis(explainer <- buildExplainer(lgb.trees))
# 14160ms / 388.0MB
profvis(pred.breakdown <- explainPredictions(lgb.model, explainer, as.matrix(target_data$data)))
# 16580ms/1048.0MB
profvis(temp <- lgb.interprete(lgb.model,as.matrix(target_data$data),2))
# Extremely Long
profvis(temp2 <- lgb.interprete(lgb.model,as.matrix(target_data$data),1:94))

