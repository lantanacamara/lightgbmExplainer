#' Step 2: Get multiple prediction breakdowns from a trained lightgbm model
#'
#' This function outputs the feature impact breakdown of a set of predictions made using an lightgbm model.
#' @param lgb.model A trained lightgbm model
#' @param explainer The output from the buildExplainer function, for this model
#' @param data A DMatrix of data to be explained
#' @return A data table where each row is an observation in the data and each column is the impact of each feature on the prediction.
#'
#' The sum of the row equals the prediction of the lightgbm model for this observation (log-odds if binary response).
#'
#' @export
#' @import data.table
#' @import lightgbm
#' @examples
#' library(lightgbm) # v2.1.0 or above
#' library(lightgbmExplainer)
#'
#' # Load Data
#' data(agaricus.train, package = "lightgbm")
#' # Train a model
#' lgb.dtrain <- lgb.Dataset(agaricus.train$data, label = agaricus.train$label)
#' lgb.params <- list(objective = "binary")
#' lgb.model <- lgb.train(lgb.params, lgb.dtrain, 5)
#' # Build Explainer
#' lgb.trees <- lgb.model.dt.tree(lgb.model) # First get a lgb tree
#' explainer <- buildExplainer(lgb.trees)
#' # compute contribution for each data point
#' pred.breakdown <- explainPredictions(lgb.model, explainer, agaricus.train$data)
#' # Show waterfall for the 8th observation
#' showWaterfall(lgb.model, explainer, lgb.dtrain, agaricus.train$data,  8, type = "binary")

explainPredictions = function(lgb.model, explainer ,data){

  #Accepts data table of the breakdown for each leaf of each tree and the node matrix
  #Returns the breakdown for each prediction as a data table

  nodes = predict(lgb.model,data,predleaf =TRUE)

  colnames = names(explainer)[1:(ncol(explainer)-2)]

  preds_breakdown = data.table(matrix(0,nrow = nrow(nodes), ncol = length(colnames)))
  setnames(preds_breakdown, colnames)

  num_trees = ncol(nodes)

  cat('\n\nExtracting the breakdown of each prediction...\n')
  pb <- txtProgressBar(style=3)
  for (x in 1:num_trees){
    nodes_for_tree = nodes[,x]
    tree_breakdown = explainer[tree==x-1]

    preds_breakdown_for_tree = tree_breakdown[match(nodes_for_tree, tree_breakdown$leaf),]
    preds_breakdown = preds_breakdown + preds_breakdown_for_tree[,colnames,with=FALSE]

    setTxtProgressBar(pb, x / num_trees)
  }

  cat('\n\nDONE!\n')

  return (preds_breakdown)

}
