#' Calculates the empirical proportion of misclustered nodes.
#'
#' @param clusters A vector of node cluster memberships
#' @param nMembers The number of nodes in each block
#'
#' @export
#' @return The proportion of misclustered nodes up to identifiability.
#'
#' @keywords clustering
misClustRate <- function(clusters, nMembers) {

  nNodes = sum(nMembers)
  nBlocks = length(nMembers)
  nMisClustNodes = 0
  uniqueClusters = unique(clusters)
  clusterCounts = matrix(0, ncol = nBlocks, nrow = nBlocks)
  clusterLabels = rep(0, nBlocks)
  
  #get label counts for each cluster
  for(i in 1:nBlocks) {
    
    clustStart = sum(nMembers[1:i]) - sum(nMembers[i]) + 1
    clustEnd = sum(nMembers[1:i])
    
    for(j in uniqueClusters) {
      # true label is i, classified as 1,2, 3, ... , K
      clusterCounts[j,i] = sum(j == clusters[clustStart:clustEnd])  
    }                                                               
  }
  
  #determine cluster label based on counts, .
  if( nBlocks >=5) {
      #greedy approach, most matched, the most matched in the rest etc
      clusterCountsTemp = clusterCounts
      for(i in 1:nBlocks) {
          
          maxCoor = t(which(clusterCountsTemp ==
                                max(clusterCountsTemp), arr.ind = T))   ## arr.ind =T, array indices be returned when x is an array?
          maxCoor = matrix(maxCoor[1,])               ## take first one when there are multiple maximizer
          clusterLabels[maxCoor[2]] = maxCoor[1]
          clusterCountsTemp[maxCoor[1], ] = rep(-1, nBlocks)
          clusterCountsTemp[, maxCoor[2]] = rep(-1, nBlocks)
          
      }
  } else{
      nCorrectNodes = 0
      ClusterAssignment <- permutation(seq = 1:nBlocks)
      for ( i in 1:nrow(ClusterAssignment)){
          row = ClusterAssignment[i,]
          col = 1:nBlocks
          sum_tmp <- sum(diag(clusterCounts[row,col]))
          if(sum_tmp>nCorrectNodes) nCorrectNodes=sum_tmp
      }
      nMisClustNodes = nNodes - nCorrectNodes
  }
  
  
  for(i in 1:nBlocks) {
    nMisClustNodes = nMisClustNodes + sum(clusterCounts[-clusterLabels[i],i])
  }  
  
  
  return( nMisClustNodes/nNodes )
}
