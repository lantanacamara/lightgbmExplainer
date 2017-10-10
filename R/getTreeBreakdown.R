
#' @import data.table
#' @import xgboost
getTreeBreakdown = function(tree, col_names){

  ####accepts a tree (data table), and column names
  ####outputs a data table, of the impact of each variable + intercept, for each leaf



  tree_breakdown <- vector("list", length(col_names)  + 2)
  names(tree_breakdown) = c(col_names,'intercept','leaf')

  leaves = tree[index<0, index]

  for (leaf in sort(leaves, decreasing = T)){

    leaf_breakdown = getLeafBreakdown(tree,leaf,col_names)
    leaf_breakdown$leaf = -leaf-1 # from native represnetation back to lgb.dt.tree
    tree_breakdown = rbindlist(append(list(tree_breakdown),list(leaf_breakdown)))
  }

  return (tree_breakdown)
}
