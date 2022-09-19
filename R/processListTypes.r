#' @title processOneListType - Process one type of list environment
#'
#' @description Process one \code{itemize} or \code{enumerate} list
#' environment.
#'
#' @param tex a vector of Latex file lines
#'
#' @param listTypes A vector containing the types lists to process.
#' For example, c("itemize", "enumerate").  Note that all list types
#' must be process-able in function \code{processOneList}.
#'
#' @return a processed vector of Latex file lines with lists
#' formated in R Markdown.
#'
#'
#' @export

processListTypes <- function(tex, listTypes){

  listTypes <- paste0("(",paste0(listTypes,collapse = "|"),")")
  beginList <- grep(paste0("\\\\begin\\{",listTypes,"\\}"), tex)
  if( length(beginList) == 0 ){
    return(tex)
  }

  # could have embedded lists, so use repeat statement

  endList <- grep(paste0("\\\\end\\{",listTypes,"\\}"), tex)

  for( i in 1:length(endList)){
    stop <- endList[i]
    beginList <- grep(paste0("\\\\begin\\{",listTypes,"\\}"), tex)
    start <- max(beginList[beginList <= stop])
    listType <- regexpr(listTypes, tex[stop])
    listType <- substring(tex[stop], listType, listType + attr(listType,"match.length") - 1)
    listLevel <- length(grep(paste0("\\\\begin\\{",listType,"\\}"), tex[1:(start-1)]))

    tex <- processOneList(tex, start, stop, listType, listLevel)
  }

  tex
}



