#' @title processLists - Process list environments
#'
#' @description Process \code{itemize} and \code{enumerate} list
#' environments in the tex document.
#'
#' @param tex a vector of Latex file lines
#'
#' @details Only \code{itemize} and \code{enumerate} lists are
#' processed.  \code{itemize} lists turn into bullet markdown lists,
#' and \code{enumerate} lists turn into 1., 2., ... lists.
#'
#' @return a processed vector of Latex file lines with lists
#' formated in R Markdown.
#'
#' @export
#'
processLists <- function(tex){

  tex <- processListTypes(tex, c("itemize", "enumerate"))
  tex

}

