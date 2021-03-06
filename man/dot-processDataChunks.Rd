% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/processData.R
\name{.processDataChunks}
\alias{.processDataChunks}
\title{Process spectral data}
\usage{
.processDataChunks(data, groupIndex)
}
\arguments{
\item{data}{character vector containing only data chunks}

\item{groupIndex}{a numeric vector the same length as data providing index
values for each line in data.}
}
\value{
a data.table containing `mz`, `intensity`, and `annotation`
}
\description{
Process spectral data
}
