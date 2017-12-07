#' @title readBibliography
#'
#' @description Reads the .bbl file from a tex document and
#' formats the bibliography for insertion into a .Rmd document
#'
#' @param bblFile The bbl file to read and format
#'
#' @return A data frame
#'
#' @export
readBibliography <- function(bblFile){

  bbl <- readLines(bblFile)
  bibLines <- c(grep("\\\\bibitem",bbl),length(bbl))

  bib <- NULL
  for( i in 1:(length(bibLines)-1)){
    curItem <- bbl[bibLines[i]:(bibLines[i+1]-1)]

    curItem <- paste(curItem, collapse = " ")
    curItem <- parseBib(curItem)

    bib <- c(bib,curItem)
  }

  bib
}
