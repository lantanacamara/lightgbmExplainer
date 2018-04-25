
#' @import data.table
#' @import lightgbm

buildExplainerFromTree = function(lgb_tree_with_stat){

  ####accepts a list of trees and column names
  ####outputs a data table, of the impact of each variable + intercept, for each leaf
  col_names <- purrr::discard(unique(lgb_tree_with_stat$split_feature), is.na)

  lgb_tree_with_stat_breakdown <-
    setNames(data.table(matrix(nrow = 0, ncol = length(col_names) + 3)),
           c(col_names,'intercept', 'leaf','tree'))

  num_trees = length(unique(lgb_tree_with_stat$tree_index))

  cat('\n\nGetting breakdown for each leaf of each tree...\n')
  pb <- txtProgressBar(style=3)

  for (x in 0:(num_trees-1)){
    tree = lgb_tree_with_stat[tree_index == x]
    tree_breakdown = getTreeBreakdown(tree, col_names)
    tree_breakdown$tree = x
    lgb_tree_with_stat_breakdown = rbindlist(append(list(lgb_tree_with_stat_breakdown),list(tree_breakdown)))
    setTxtProgressBar(pb, (x+1) / num_trees)
  }
  # replace NA with 0
  lgb_tree_with_stat_breakdown[is.na(lgb_tree_with_stat_breakdown)] <- 0
  return (lgb_tree_with_stat_breakdown)

}
