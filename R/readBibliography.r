#' @title readBibliography
#'
#' @description Reads the .bbl file from a tex document and
#' formats the bibliography for insertion into a .Rmd document
#'
#' @param bblFile The bbl file to read and format
#'
#' @param style A text string specifying the style of the bibliography.
#' Only difference this makes is that for style == "plain", the '[n]'
#' numbers are added at the beginning.
#'
#' @return A vector of character strings containing the
#' formatted bibliography
#'
#' @export
readBibliography <- function(bblFile,style){

  bbl <- readLines(bblFile)
  bibLines <- c(grep("\\\\bibitem",bbl),length(bbl))

  bib <- NULL
  for( i in 1:(length(bibLines)-1)){
    curItem <- bbl[bibLines[i]:(bibLines[i+1]-1)]

    curItem <- paste(curItem, collapse = " ")
    curItem <- parseBib(curItem)

    if(style == "plain"){
      curItem <- paste0("[",i,"] ",curItem)
    }
    bib <- c(bib,curItem)
  }

  bib
}
