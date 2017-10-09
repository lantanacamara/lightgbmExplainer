
#' @import data.table
#' @import lightgbm
#' @import purrr
getStatsForTrees = function(trees, nodes.train, type = "binary", base_score = 0.5){
  #Accepts data table of tree (the output of lgb.model.dt.tree)
  #Returns a list of tree, with the stats filled in (uplift_weight and previous feature, most importantly)
  #weight equal internal_value in lgb tree

  # tree_list = split(trees, as.factor(trees$tree_index)) # Why as.factor?
  tree_list = split(trees, trees$tree_index)
  walk(tree_list, ~.x[,parent := ifelse(is.na(node_parent), leaf_parent, node_parent)])
  walk(tree_list, ~.x[,weight := ifelse(is.na(internal_value), leaf_value, internal_value)])

  weight_list = map(tree_list, ~.x[!is.na(split_index)][order(split_index)]$weight)
  feature_list = map(tree_list, ~.x[!is.na(split_index)][order(split_index)]$split_feature)

  walk2(tree_list, weight_list, ~.x[,previous_weight := ifelse(is.na(parent), 0, .y[parent+1])])
  walk(tree_list, ~.x[,uplift_weight := weight - previous_weight])

  walk2(tree_list, feature_list, ~.x[,previous_feature := ifelse(is.na(parent), 0, .y[parent+1])])

  return (tree_list)
}
