#' @title processOneList - Process one list environment
#'
#' @description Process one \code{itemize} or \code{enumerate} list
#' environment.
#'
#' @param tex a vector of Latex file lines
#'
#' @param start line number where list starts (begin statement)
#'
#' @param end line number where list ends (end statement)
#'
#' @param listType The type of list to process. Currently, only "itemize"
#' or "enumerate".
#'
#' @param listLevel the level of the list.  0 is top level, 1 is for
#' lists embedded in a 0 level list (i.e., 2nd level), etc.
#'
#' @return a processed vector of Latex file lines with lists
#' formated in R Markdown.
#'
#' @export
processOneList <- function(tex, start, end, listType, listLevel){


  # Not sure true always, but assume begin statement is on line by itself.

  # remove begin and end statements
  tex[start] <- sub( paste0("\\\\begin\\{",listType,"\\}"), "", tex[start])
  tex[end] <- sub( paste0("\\\\end\\{",listType,"\\}"), "", tex[end])

    itemLocs <- gregexpr("\\\\item", tex[start:end])
    maxItems <- length(unlist(lapply(itemLocs, function(x){if(x>0){x} else {NULL}})))
    if( listType == "itemize"){
      itemMark <- "*"
    } else {
      itemNum <- 1
      itemMark <- paste0(itemNum,".")
    }
    for( i in 1:length(itemLocs) ){
      if( itemLocs[[i]] > 0 ){
        for( j in 1:length(itemLocs[[i]])){
            lnMatch <- attr(itemLocs[[i]], "match.length")[j]

            # NOTE: anything between start of line and \item is lost!!
            # the following code perserves it
            # if(itemLocs[[i]][j] > 1){
            #   part1 <- substring(tex[start+i-1],1,itemLocs[[i]][j]-1)
            # } else {
            #   part1 <- ""
            # }
            part1 <- paste0(rep("    ", listLevel), collapse = "")
            if(itemLocs[[i]][j]+lnMatch < nchar(tex[start+i-1])){
              part3 <- substring(tex[start+i-1], itemLocs[[i]][j]+lnMatch)
            } else {
              part3 <- ""
            }
            tex[start+i-1] <- paste0(part1, itemMark, " ", part3)

            if( listType == "itemize"){
              itemMark <- "*"
            } else {
              itemNum <- itemNum + 1
              itemMark <- paste0(itemNum,".")
            }
        }
      }
  }

  tex
}



