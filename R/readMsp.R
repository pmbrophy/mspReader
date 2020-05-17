#' Import a .msp file into R
#'
#' @param path system path to a .msp file containing mass spectral data
#'
#' @return
#' @export
#'
#' @examples
#'

readMsp <- function(path){

  #Read text data and index
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
  lns <- lns[-c(headerLocs, blankLines)]
  group_i <- group_i[-c(headerLocs, blankLines)]

  #Process Header
  header <- BiocParallel::bplapply(X = headers,
                                   FUN = function(X){library(mspReader); .processHeader(X)})
  remove(headers)
  header <- data.table::rbindlist(header, idcol = TRUE, fill = TRUE, use.names = TRUE)

  #Process Data
  #TODO: Parallize this chunk
  lns <- strsplit(x = lns, split = "\t", perl = TRUE) #memory hog
  lns <- do.call(what = rbind, args = lns)
  #maybe faster to : as.numeric(sapply(lns, "[[", 1))?
  lns <- data.table::data.table(index = group_i,
                                mz = as.numeric(lns[,1]),
                                intensity = as.numeric(lns[,2]),
                                annotation = lns[,3])

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
