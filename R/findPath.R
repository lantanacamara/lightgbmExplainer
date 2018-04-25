
#' @import data.table
#' @import lightgbm
findPath = function(currentnode, index, parent, path = c()){
  # print(currentnode)
  # print(index)
  # print(parent)
  #accepts a tree data table, and the node to reach
  #path is used in the recursive function - do not set this

  while(currentnode!=0){
    path = c(currentnode, path)
    currentnode = parent[index==currentnode]
  }
  # print(c(0,path))
  return(c(0,path))

}

