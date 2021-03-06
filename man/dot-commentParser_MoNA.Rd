% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/commentParser.R
\name{.commentParser_MoNA}
\alias{.commentParser_MoNA}
\title{Process the Comment field in a MoNA .msp}
\usage{
.commentParser_MoNA(comment)
}
\arguments{
\item{comment}{A text vector containing the comment field of an .msp file}
}
\value{
a named character vector
}
\description{
Process the text contained in the comment field in a .msp header
  from MoNA. Comment field is expected to adhear to structure where each
  `name=value` pair is designated with an `=` and the `name=value` pair is
  encapsulated in quotes and isolated on either side with a `[:blank:]`
  character. These pairs are parsed and seperated to provide a charactor
  vector of named values.
}
