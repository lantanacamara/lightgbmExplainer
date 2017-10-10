
#' @import data.table
#' @import lightgbm
getLeafBreakdown = function(tree,leaf,col_names){

  ####accepts a tree, the leaf id to breakdown and column names
  ####outputs a list of the impact of each variable + intercept

  impacts = as.list(rep(0,length(col_names)))
  names(impacts) = col_names

  path = findPath(tree,leaf)
  reduced_tree = tree[index %in% path,.(previous_feature,uplift_weight)]

  impacts$intercept=reduced_tree[1,uplift_weight]

  tmp = reduced_tree[-1,.(sum=sum(uplift_weight)),by=previous_feature]
  # tmp = tmp[-nrow(tmp)]
  impacts[tmp[,previous_feature]]=tmp[,sum]

  return (impacts)
}


