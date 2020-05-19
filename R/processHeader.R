#' Process the header from an msp file
#'
#' @description Top level header processor for msp files. Provide a character
#'   vector containing the header lines and corrisponding groupIndex.
#'
#' @param header character vector containing the header lines only
#' @param groupIndex an integer vector the same length as `header` providing a
#'   index value that links the `header` information to data chunks (spectral
#'   data)
#' @param commentType either `NIST` or `MoNA` - used for comment parsing
#'
#' @return a fully formatted data.table
#'

.processHeader <- function(header, groupIndex, commentType = commentType){
  print("processing header chunks")
  #Split header field by ":" expected to produce two elements per line
  headers <- strsplit(x = header, split = ": ", perl = TRUE)

  #extract names from each item in list
  headerNames <- sapply(headers, "[[", 1)
  #eactract values from each item in list
  headerValues <- sapply(headers, "[[", 2)

  #Convert header data to a data.table
  headerDt <- data.table::data.table(index = groupIndex, variable = headerNames, value = headerValues)
  headerDt <- data.table::dcast(data = headerDt, formula = index ~ variable, value.var = "value")

  #Parse comment in the header and remove
  print("Processing header comment")
  headerDt <- .parseHeaderDt(headerDt = headerDt, commentType = commentType)

  headerDt
}

#' Process header data.table containing comment string
#'
#' @description Wrapper function that takes a data.table containing header
#' information and a `Comment` column full of text. The `Comment` column is
#' parsed and a data.table with additional columns is returned. The original
#' `Comment` column is removed.
#'
#' @param dt a data.table returned from .processHeader()
#' @param commentType either `NIST` or `MoNA` - used for comment parsing
#'
#' @return a data.table with additional parsed fields from the comment comment
#'   strings
#'

.parseHeaderDt <- function(headerDt, commentType = commentType){
  #Parse comment
  commentDt <- .parseCommentVector(comments = headerDt$Comment, indexs = headerDt$index, commentType = commentType)

  headerDt[, Comment := NULL]

  #Merge data.tables and return
  headerDt <- data.table::merge.data.table(x = headerDt, y = commentDt, by = "index")

  headerDt
}
