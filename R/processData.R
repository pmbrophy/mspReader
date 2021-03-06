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
  index <- NULL

  #Split and bind
  data <- strsplit(x = data, split = "[[:blank:]]", perl = TRUE)
  data <- do.call(rbind, data)

  nCols <- ncol(data)
  #Ensure numeric data
  if(!is.numeric(data)){
    if(nCols == 2){
      data <- data.table::data.table(mz = as.numeric(data[,1]),
                                     intensity = as.numeric(data[,2]))
    }else if(nCols == 3){
      data <- data.table::data.table(mz = as.numeric(data[,1]),
                                     intensity = as.numeric(data[,2]),
                                     annotation = data[,3])
    }else{
      stop(paste("number of columns in data chunks:", nCols))
    }
  }else{
    #Data is already numeric
    data <- data.table::data.table(data)
    newNames <- c("mz", "intensity", "annotation")[c(1:nCols)]
    data.table::setnames(x = data, new = newNames, old = names(data))
  }

  data[, index := groupIndex]
}
