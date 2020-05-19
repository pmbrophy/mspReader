#' Process spectral data
#'
#' @param data character vector containing only data chunks
#'
#' @return a data.table containing `mz`, `intensity`, and `annotation`
#'

.processDataChunks <- function(data, groupIndex){
  print("processing data chunks")

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
