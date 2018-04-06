#' Step 1: Build an lightgbmExplainer
#'
#' This function outputs an lightgbmExplainer (a data table that stores the feature impact breakdown for each leaf of each tree in an lightgbm model). It is required as input into the explainPredictions and showWaterfall functions.
#' @param lgb.model A trained lightgbm model
#' @param trainingData A lgb.Dataset of data used to train the model
#' @param type The objective function of the model - either "binary" (for binary:logistic) or "regression" (for reg:linear)
#' @param base_score Default 0.5. The base_score variable of the lightgbm model.
#' @return The lightgbm Explainer for the model. This is a data table where each row is a leaf of a tree in the lightgbm model
#'  and each column is the impact of each feature on the prediction at the leaf.
#'
#'  The leaf and tree columns uniquely identify the node.
#'
#'  The sum of the other columns equals the prediction at the leaf (log-odds if binary response).
#'
#'  The 'intercept' column is identical for all rows and is analogous to the intercept term in a linear / logistic regression.
#'
#' @export
#' @import data.table
#' @import lightgbm
#' @examples
#' library(lightgbm)
#' library(lightgbmExplainer)
#'
#' lgb.train <- agaricus.train
#' lgb.dtrain <- lgb.Dataset(lgb.train$data, label = lgb.train$label)
#' lgb.params = list(objective = "binary")
#' lgb.model <- lgb.train(lgb.params, lgb.dtrain, 3)
#' lgb.trees <- lgb.model.dt.tree(lgb.model)
#' lgb.trees
#' library(lightgbmExplainer)
#' explainer = buildExplainer(lgb.model,lgb.train.data, type="binary", base_score = 0.5)
#' pred.breakdown = explainPredictions(lgb.model, explainer, lgb.test.data)
#'
#' showWaterfall(lgb.model, explainer, lgb.test.data, test.data,  2, type = "binary")
#' showWaterfall(lgb.model, explainer, lgb.test.data, test.data,  8, type = "binary")


buildExplainer = function(lgb.model, col_names, type = "binary", base_score = 0.5){

  if(lgb.model$best_iter < 0){
    best_iter <- NULL
  }else{
    best_iter <- lgb.model$best_iter
  }

  cat('\nCreating the trees of the lightgbm model...')
  trees = lgb.model.dt.tree(model = lgb.model, num_iteration = best_iter)
  cat('\nGetting the leaf nodes for the training set observations...')
  nodes.train = predict(lgb.model,trainingData,predleaf =TRUE)

  cat('\nBuilding the Explainer...')
  cat('\nSTEP 1 of 2')
  tree_list = getStatsForTrees(trees)
  cat('\n\nSTEP 2 of 2')
  explainer = buildExplainerFromTreeList(tree_list,col_names)

  cat('\n\nDONE!\n')

  return (explainer)
}
