#' Parse vector of comment strings
#'
#' @param comments a character vector containing the comment fields
#' @param indexs the group index of each comment
#' @param commentType either `NIST` or `MoNA` - used for comment parsing
#'
#' @return a data.table
#'

.parseCommentVector <- function(comments, indexs, commentType){
  #Parse each comment using appropraite method
  if(commentType == "NIST"){
    comments <- mapply(FUN = .commentParser_NIST,
                       comment = comments,
                       USE.NAMES = FALSE,
                       SIMPLIFY = FALSE)
  }else if(commentType == "MoNA"){
    comments <- mapply(FUN = .commentParser_MoNA,
                       comment = comments,
                       USE.NAMES = FALSE,
                       SIMPLIFY = FALSE)
  }else{
    stop("Comment type must be either NIST or MoNA")
  }

  #Get names of comment fields and index
  commentNames <- lapply(X = comments, FUN = names)
  nCommentsPerGroup <- sapply(X = comments, FUN = length)
  index <- mapply(FUN = rep, x = indexs, times = nCommentsPerGroup, SIMPLIFY = FALSE)

  #Get into data.table
  dt <- data.table::data.table(index = unlist(index), variable = unlist(commentNames), values = unlist(comments))

  #Reformat
  dt <- data.table::dcast(data = dt, formula = index ~ variable, value.var = "values", fill = TRUE)

  dt
}

#' Process the Comment field in a NIST .msp
#'
#' @description Process the text contained in the comment field in a .msp
#'   header from NIST. Comment field is expected to adhear to structure where each
#'   `name=value` pair is designated with an `=` and the `name=value` pair is
#'   isolated on either side with a `[:blank:]` character. These pairs are
#'   parsed and seperated to provide a charactor vector of named values.
#'
#' @param comment A text vector containing the comment field of an .msp file
#'
#' @return a named character vector
#'

.commentParser_NIST <- function(comment){
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

  #Process name value pairs
  parsedComment <- splitByEquals(nameValuePairs = subGroups)

  parsedComment
}

#' Process the Comment field in a MoNA .msp
#'
#' @description Process the text contained in the comment field in a .msp header
#'   from MoNA. Comment field is expected to adhear to structure where each
#'   `name=value` pair is designated with an `=` and the `name=value` pair is
#'   encapsulated in quotes and isolated on either side with a `[:blank:]`
#'   character. These pairs are parsed and seperated to provide a charactor
#'   vector of named values.
#'
#' @param comment A text vector containing the comment field of an .msp file
#'
#' @return a named character vector
#'

.commentParser_MoNA <- function(comment){
  #Remove Comments:
  comment <- gsub(pattern = "Comments: \"", x = comment, replacement = "")

  #Split into name-value pairs
  comments <- unlist(strsplit(split = "\" \"", x = comment))

  #Remove trailing quote from last item
  numComments <- length(comments)
  comments[numComments] <- gsub(x = comments[numComments], pattern = "\"", replacement = "")

  #Process name value pairs
  parsedComment <- .splitByEquals(nameValuePairs = comments)

  parsedComment
}


#' Split name-value pairs by an equal sign
#'
#' @param nameValuePairs a list of vectors containing name value pairs
#'
#' @return returns a named vector
#'

.splitByEquals <- function(nameValuePairs){
  #Check for number of "=" signs
  numNameValuePairs <- sapply(X = gregexpr(pattern = "=", text = nameValuePairs, perl = TRUE), FUN = length)

  #Split groups with only one "=" into name/value pairs
  nameValuePairLocs <- which(numNameValuePairs == 1)

  ##################################################################### #
  ###################    SINGLE NAME VALUE PAIR    #################### #
  ##################################################################### #
  subGroups_nameValuePairs <- strsplit(x = nameValuePairs[nameValuePairLocs], split = "=", perl = TRUE)

  #Extract and name the name-value pairs
  subGroup_names <- sapply(X = subGroups_nameValuePairs, "[[", 1)
  subGroup_values <- sapply(X = subGroups_nameValuePairs, "[[", 2)
  names(subGroup_values) <- subGroup_names

  ##################################################################### #
  ###################    UNKNOWN NAME VALUE PAIR    ################### #
  ##################################################################### #
  unknowGroups <- nameValuePairs[-nameValuePairLocs]
  if(length(unknowGroups) > 0){
    names(unknowGroups) <- paste0("unknown_", c(1:length(unknowGroups)))
  }

  #Combine all named subgroups and return
  c(subGroup_values, unknowGroups)
}
