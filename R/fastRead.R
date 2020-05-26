#Taken from kevinushey/Kmisc "read.R"
#https://github.com/kevinushey/Kmisc/blob/master/src/read.cpp

##' Read a File
##'
##' Read a file into memory. We memory map the file for fast I/O.
##' The file is read in as a character vector of length = 1. This doesn't appear
##' to scale. Large files do not read into memory faster
##'
##' @param file Path to a file.
##'
##' @return character vector of length 1
##' @export
##'

f_read <- function(file) {
  file <- normalizePath( as.character(file) )
  if (!file.exists(file)) {
    stop("No file at file location '", file, "'.")
  }
  .Call('_mspReader_read', PACKAGE = 'mspReader', file, FALSE)
}


##' Read lines from a file
##'
##' Read a file into memory. We memory map the file for fast I/O. The file is
##' read in as a character vector. This doesn't appear to scale. Large files do
##' not read into memory faster
##'
##' @param file Path to a file.
##'
##' @return character vector
##' @export
##'
##' @example p <- file.path( R.home(), "NEWS" ) if (file.exists(p)) stopifnot(
##' identical( readLines(p), readlines(p) ) )
##'

f_readlines <- function(file) {
  file <- normalizePath( as.character(file) )
  if (!file.exists(file)) {
    stop("No file at file location '", file, "'.")
  }
  .Call('_mspReader_read', PACKAGE = 'mspReader', file, TRUE)
}
