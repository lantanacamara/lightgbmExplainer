
#' @import data.table
#' @import lightgbm
#' @importFrom purrr walk map walk2
getStatsForTrees = function(lgb_tree){
  #Accepts data table of tree (the output of lgb.model.dt.tree)
  #Returns a list of tree, with the stats filled in
  #weight equal internal_value in lgb tree
  #Assumption: the trees are ordered from left to right

  lgb_tree_with_stat = copy(lgb_tree)
  #  using native index representation
  lgb_tree_with_stat[, index := ifelse(is.na(leaf_index), split_index, -leaf_index-1)]
  lgb_tree_with_stat[, parent := ifelse(is.na(node_parent), leaf_parent, node_parent)]
  lgb_tree_with_stat[, weight := ifelse(is.na(internal_value), leaf_value, internal_value)]

  # 0 - left child, 1 - right child
  # Must not sort at this step
  # TODO: handle default_left
  lgb_tree_with_stat[,child_type := 0:(.N-1L), by = .(tree_index, parent)]

  lgb_tree_with_stat <- merge(lgb_tree_with_stat,
        lgb_tree_with_stat[!is.na(split_index)][,.(tree_index,
                                          parent = split_index,
                                          previous_weight = weight,
                                          previous_feature = split_feature,
                                          previous_threshold = threshold,
                                          previous_decision_type = decision_type)],
        all.x = T, sort = F)
  lgb_tree_with_stat[,previous_decision_type := ifelse(child_type == 0,
                                     previous_decision_type,
                                     ifelse(previous_decision_type == "<=",
                                            ">", "!="))]
  lgb_tree_with_stat[,uplift_weight := weight - previous_weight]

  return (lgb_tree_with_stat)
}
