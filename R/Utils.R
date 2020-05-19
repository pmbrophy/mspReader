#' Construct group index
#'
#' @param breaks Vector of integers specifying the index location where a group terminates
#'
#' @return a vector of index values the same length as the vector used to generate breaks
#'

.groupIndex <- function(breaks){
  nBreaks <- length(breaks)

  #Generate integer for each group
  groups <- c(1:nBreaks)

  #Calculate number of members in each group assuming last line is empty
  nMembers <- breaks[2:nBreaks] - breaks[1:(nBreaks-1)]
  nMembers <- c(breaks[1], nMembers)

  #Construct vector of index values
  index <- mapply(FUN = rep, x = groups, times = nMembers, SIMPLIFY = FALSE)
  index <- unlist(index)

  index
}

#' Split a vector into groups
#'
#' @param index the vector of index values to be split into groups
#' @param nGroups number of groups to be generated
#' @param randomize should the groups contain a random or ordered sampling from
#'   the index vector
#'
#' @return returns a list containing `groups` and `groupIndex` both of which are
#'   lists of length nGroups
#'
#' @examples
#' index <- c(c(1:10), c(30:40))
#' groupedIndex <- splitIndex(nGroups = 3, index = index, randomize = TRUE)
#'
#' groups <- unlist(groupedIndex$groups)
#' groups_i <- unlist(groupedIndex$groupIndex)
#'
#' all(index %in% groups)
#'
#' #Reorder the radomized groups by groups_i
#' groups_reorder <- vector(mode = "numeric", length = length(groups))
#' groups_reorder[groups_i] <- groups
#' groups_reorder
#'
.splitIndex <- function(index, nGroups, randomize = FALSE){
  indexLength <- length(index)

  #randomized index_i for the vector index
  index_i <- c(1:indexLength)
  if(randomize){
    index_i <- sample(index_i)
  }

  #Calcualte the groups
  groupSize <- indexLength %/% nGroups
  groupStarts <- seq(from = 1, to = groupSize * nGroups, by = groupSize)
  groupStops <- groupStarts + (groupSize-1)
  groupStops[nGroups] <- groupStops[nGroups] + (length(index) %% nGroups)

  groups <- mapply(FUN = seq, from = groupStarts, to = groupStops, by = 1, SIMPLIFY = FALSE)

  #randomized group indexs
  groups_i <- mapply(FUN = .indexFromVector, indexLocs = groups, MoreArgs = list(index_i), SIMPLIFY = FALSE)

  if(randomize){
    #Using randomized group indexs, get the actual index values
    groups <- mapply(FUN = .indexFromVector, indexLocs = groups_i, MoreArgs = list(index), SIMPLIFY = FALSE)
  }else{
    #Ordered implementation
    groups <- mapply(FUN = .indexFromVector, indexLocs = groups, MoreArgs = list(index), SIMPLIFY = FALSE)
  }

  list(groups = groups, groupIndex = groups_i)
}

#extract index locations from a vector
.indexFromVector <- function(indexLocs, vec){
  vec[indexLocs]
}
