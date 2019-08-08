
#' @import data.table
#' @import lightgbm
getTreeBreakdown = function(tree, col_names){

  ####accepts a tree (data table), and column names
  ####outputs a data table, of the impact of each variable + intercept, for each leaf

  tree_breakdown <-
    setNames(data.table(matrix(nrow = 0, ncol = length(col_names) + 2)),
             c(col_names,'intercept', 'leaf'))

  temp <- copy(tree)
  temp[,path:=purrr::map(index, findPath, index, parent)]
  temp <- data.table(merge(tidyr::unnest(temp[index <0, .(leaf = -index-1, path)]),
        temp[, .(path = index, previous_feature,uplift_weight)],
        all.x = T, sort = F))
  temp <- temp[!is.na(previous_feature),
       .(uplift_weight = sum(uplift_weight)),
       by =.(leaf, previous_feature)]
  temp <- dcast(temp, formula = leaf ~ previous_feature,
        value.var = "uplift_weight", fill = 0)

  tree_breakdown = rbindlist(list(tree_breakdown, temp), use.names = T, fill = TRUE)

  return (tree_breakdown)
}
