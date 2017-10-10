
#' @import data.table
#' @import xgboost
findPath = function(tree, currentnode, path = c()){

  #accepts a tree data table, and the node to reach
  #path is used in the recursive function - do not set this

  while(currentnode!=0){
    path = c(currentnode, path)
    currentnode = tree[index==currentnode,parent]
  }
  return(c(0,path))

}

