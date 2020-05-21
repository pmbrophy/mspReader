#' Process spectral data
#'
#' @param data character vector containing only data chunks
#' @param groupIndex a numeric vector the same length as data providing index
#'   values for each line in data.
#'
#' @return a data.table containing `mz`, `intensity`, and `annotation`
#'
#'

.processDataChunks <- function(data, groupIndex){
  print("processing data chunks")

  #Needed for passing CMD Check
  ':=' <- NULL
  index <- NULL

  #Split and bind
  data <- strsplit(x = data, split = "[[:blank:]]", perl = TRUE)
  data <- do.call(rbind, data)

  #data.table
  data <- data.table::as.data.table(data)
  newNames <- c("mz", "intensity", "annotation")[c(1:ncol(data))]
  data.table::setnames(x = data, new = newNames, old = names(data))
  data[, index := groupIndex]

  data
}
