#' Import a .msp file into R
#'
#' @description Read an .msp file into R from a system path. The data and header
#' for each block are converted and assigned index numbers. Two data.tables are
#' returned to save memeory. These can be combined by merging on the `index`
#'
#' @param path system path to a .msp file containing mass spectral data
#'
#' @return a list containing two data.tables
#' @export
#'

readMsp <- function(path, commentType){

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

  ###################################################################################################### #
  ###########################################   HEADER    ############################################## #
  ###################################################################################################### #
  #Find header locations
  headerLocs <- grep(pattern = ": ", x = lns, perl = TRUE)

  #Process headers
  headers <- .processHeader(header = lns[headerLocs],
                            groupIndex = group_i[headerLocs],
                            commentType = commentType)

  #################################################################################################### #
  ###########################################   Data    ############################################## #
  #################################################################################################### #
  #Process data
  data <- .processDataChunks(data = lns[-c(headerLocs, blankLines)],
                             groupIndex = group_i[-c(headerLocs, blankLines)])

  list(data = data, info = headers)
}
