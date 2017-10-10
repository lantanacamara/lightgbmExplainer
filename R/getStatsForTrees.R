
#' @import data.table
#' @import lightgbm
#' @importFrom purrr walk map walk2
getStatsForTrees = function(trees){
  #Accepts data table of tree (the output of lgb.model.dt.tree)
  #Returns a list of tree, with the stats filled in
  #weight equal internal_value in lgb tree
  #Assumption: the trees are ordered from left to right

  # tree_list = split(trees, as.factor(trees$tree_index)) # Why as.factor?
  tree_list = split(trees, trees$tree_index)
  walk(tree_list, ~.x[,index := ifelse(is.na(leaf_index), split_index, -leaf_index-1)]) # using native index representation
  walk(tree_list, ~.x[,parent := ifelse(is.na(node_parent), leaf_parent, node_parent)])
  walk(tree_list, ~.x[,weight := ifelse(is.na(internal_value), leaf_value, internal_value)])
  # 0 - left child, 1 - right child
  walk(tree_list, ~.x[,child_type := 0:(.N-1L), by = .(parent)])

  split_list = map(tree_list, ~.x[!is.na(split_index)][order(split_index)])

  walk2(tree_list, split_list, ~.x[,previous_weight := ifelse(is.na(parent), 0, .y$weight[parent+1])])
  walk2(tree_list, split_list, ~.x[,previous_feature := ifelse(is.na(parent), 0, .y$split_feature[parent+1])])
  walk2(tree_list, split_list, ~.x[,previous_threshold := ifelse(is.na(parent), 0, .y$threshold[parent+1])])
  walk2(tree_list, split_list, ~.x[,previous_decision_type := ifelse(is.na(parent), NA, .y$decision_type[parent+1])])
  walk2(tree_list, split_list, ~.x[,previous_decision_type := ifelse(child_type == 0,
                                                                     previous_decision_type,
                                                                     ifelse(previous_decision_type == "<=",
                                                                            ">", "!="))])
  walk(tree_list, ~.x[,uplift_weight := weight - previous_weight])
  return (tree_list)
}
