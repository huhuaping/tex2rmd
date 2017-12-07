#' @title Process the display equations in the tex file
#'
#' @description Processes all the display equations, converting them
#' into a markdown format.
#'
#' @param tex An array of latex lines
#'
#' @return A \code{tex} array, just like the input, with
#' display equations formated so they work in markdown.
#'
#' @export
processDisplayEqns <- function(tex){

  eqnPosBegin <- regexpr( "\\\\begin\\{(equation|equation\\*|gather)\\}", tex)
  eqnPosEnd <- regexpr( "\\\\end\\{(equation|equation\\*|gather)\\}", tex)

  eqnPosBegin <- which(eqnPosBegin>0)
  eqnPosEnd <- which(eqnPosEnd>0)

  for( i in 1:length(eqnPosBegin)){
    eqn <- tex[eqnPosBegin[i]:eqnPosEnd[i]]
    eqn <- gsub("\\\\(begin|end)\\{(equation|equation\\*|gather)\\}","",eqn)
    eqn <- gsub("\\\\label\\{[^\\}]*\\}","",eqn)
    eqn <- gsub("\\\\nonumber","",eqn)
    eqn[1] <- paste0("$$", eqn[1])
    eqn[length(eqn)] <- paste0(eqn[length(eqn)], "$$")
    eqn <- gsub("\\\\\\\\","\n$$\n$$",eqn)
    eqn <- eqn[nchar(eqn)>0]

    tex <- c(tex[1:(eqnPosBegin[i]-1)], eqn, tex[(eqnPosEnd[i]+1):length(tex)])
  }

  tex
}
