#' Import a .msp file into R
#'
#' @param path system path to a .msp file containing mass spectral data
#'
#' @return
#' @export
#'
#' @examples
#'

readMsp <- function(path, parallel = FALSE){

  #Read text data and index
  print("reading text into memory")
  lns <- readLines(con = path)
  nLines <- length(lns)

  #Locate blank lines and build index
  nchar_lns <- nchar(lns)
  blankLines <- which(nchar_lns == 0)

  #Build group index for lns
  lastLineNotBlank <- nchar(lns[nLines]) != 0
  if(lastLineNotBlank){
    stop("last line is not blank, indexing will be corrupt")
  }
  group_i <- .groupIndex(breaks = blankLines)

  #Find header locations
  headerLocs <- grep(pattern = ":", x = lns, perl = TRUE)

  #Extract each header as its own group
  headers <- split(x = lns[headerLocs], f = group_i[headerLocs])

  #Remove header information and blank lines from lns and update group index
  print("processing data chunks")

  lns <- lns[-c(headerLocs, blankLines)]
  group_i <- group_i[-c(headerLocs, blankLines)]
  lns <- data.table::fread(text = lns, sep = "\t")
  lns[, index := group_i]

  #Process Header
  if(parallel){
    #TODO: Split up headers, wrap .processHeader into a processHeaders function using lapply
    i <- splitIndex()
    headers <- headers[i]
    print("processing header using multiple cores")
    header <- BiocParallel::bplapply(X = headers, FUN = function(X){library(mspReader); .processHeader(X)})
  }else{
    print("processing header")
    header <- lapply(X = headers, FUN = .processHeader)
  }

  remove(headers)
  header <- data.table::rbindlist(header, idcol = TRUE, fill = TRUE, use.names = TRUE)

  list(ions = lns, info = header)
}

#' Process the Comment field in .msp
#'
#' @description Process the text contained in the comment field in a .msp
#'   header. Comment field is expected to adhear to structure where each
#'   `name=value` pair is designated with an `=` and the `name=value` pair is
#'   isolated on either side with a `[:blank:]` character. These pairs are
#'   parsed and seperated to provide a charactor vector of named values.
#'
#' @param comment A text vector containing the comment field of an .msp file
#'
#' @return a named character vector
#'

.commentParser <- function(comment){
  commentLength <- nchar(comment)

  #Locate "[[:blank:]][[:alnum:]]+="
  commentSubLocs <- gregexpr(pattern = "[[:blank:]][[:graph:]]+=", text = comment, perl = TRUE)

  #Start locs of actual string are at +1 location
  subLocs_start <- commentSubLocs[[1]] + 1
  numSubGroups <- length(subLocs_start)

  #Stop Locations
  subLocs_stop <- subLocs_start[2:numSubGroups] - 2
  subLocs_stop <- c(subLocs_stop, commentLength)

  #Split into sub groups ("name"="value)
  subGroups <- substring(text = comment, first = subLocs_start, last = subLocs_stop)
  numNameValuePairs <- sapply(X = gregexpr(pattern = "=", text = subGroups, perl = TRUE), FUN = length)

  #Split groups with only one "=" into name/value pairs
  nameValuePairs <- which(numNameValuePairs == 1)
  subGroups_nameValuePairs <- strsplit(x = subGroups[nameValuePairs], split = "=", perl = TRUE)

  #Extract and name the name-value pairs
  subGroup_names <- sapply(X = subGroups_nameValuePairs, "[[", 1)
  subGroup_values <- sapply(X = subGroups_nameValuePairs, "[[", 2)
  names(subGroup_values) <- subGroup_names

  #Extract remaining subGroups that don't follow name/value pair
  numUnknownGroups <- numSubGroups - length(nameValuePairs)
  unknowGroups <- subGroups[-nameValuePairs]
  names(unknowGroups) <- paste0("unknown_", c(1:numUnknownGroups))

  #Combine all named subgroups and return
  c(subGroup_values, unknowGroups)
}

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

#' Process the header from an msp file
#'
#' @param header character vector containing the header
#'
#' @return a named character vector
#'

.processHeader <- function(header){
  #Find and process comment
  commentLoc <- grepl(pattern = "Comment:", x = header)
  comment <- .commentParser(comment = header[commentLoc])

  #Process other header fields
  header <- strsplit(x = header[-commentLoc], split = ": ", perl = TRUE)
  headerNames <- sapply(header, "[[", 1)
  header <- sapply(header, "[[", 2)
  names(header) <- headerNames

  #Combine
  header <- data.table::data.table(t(c(header, comment)))

  header
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
#' @export
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
