#' @title processFigures
#'
#' @description Process the figures in a Latex file vector and
#' put into Rmd format
#'
#' @param x A vector of lines from the Latex file
#'
#' @return A vector of lines with latex figures replace by something
#' that will render a Rmd Figure
#'
#' @export
processFigures <- function(x){

  # First process any includegraphics in Figure environments
  figNum <- 1
  repeat{
    beginFigure <- grep("\\\\begin\\{figure\\}", x)
    endFigure <- grep("\\\\end\\{figure\\}", x)

    if(length(beginFigure)==0){
      break
    }

    rmdFigure <- processOneFigure(x, beginFigure[1], endFigure[1], figNum)
    x <- c( x[1:(beginFigure[1]-1)], rmdFigure, x[(endFigure[1]+1):length(x)])
    figNum <- figNum + 1
  }

  # Now process any tabular environments in text by themselves
  # repeat{
  #   beginFigure <- which(regexpr("\\\\begin\\{tabular\\}", x)>0)
  #   endFigure <- which(regexpr("\\\\end\\{tabular\\}", x)>0)
  #
  #   beginFig <- which(regexpr("\\\\begin\\{figure\\}", x)>0)
  #   endFig <- which(regexpr("\\\\end\\{figure\\}", x)>0)
  #
  #   if(length(beginFig)>0){
  #     repeat{
  #       if( length(beginFigure) > 0){
  #         inAFig <- (beginFig < beginFigure[1]) & (endFigure[1] < endFig)
  #         if( any(inAFig) ){
  #           # tabular is imbedded in a figure, do nothing
  #           beginFigure <- beginFigure[-1]
  #           endFigure <- endFigure[-1]
  #         } else {
  #           break
  #         }
  #       } else {
  #         break
  #       }
  #     }
  #   }
  #
  #   if(length(beginFigure)==0){
  #     break
  #   }
  #
  #   rmdFigure <- processOneFigure(x, beginFigure[1], endFigure[1], NA)
  #   x <- c( x[1:(beginFigure[1]-1)], rmdFigure, x[(endFigure[1]+1):length(x)])
  # }

  x
}
