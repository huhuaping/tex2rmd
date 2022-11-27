#' @title processTables
#'
#' @description Process the tables in a Latex file vector and
#' put into Rmd format
#'
#' @param x A vector of lines from the Latex file
#' @param tabInbed logical if a tabular within other environments, 
#' with default value "tabInbed = TRUE"
#'
#' @return A vector of lines with latex tables replace by something
#' that will render a Rmd table
#'
#' @export
processTabulars <- function(x, tabInbed = TRUE){

  # First process any tables in table environments
  tabNum <- 1
  repeat{
    beginTable <- regexpr("\\\\begin\\{tabular\\}", x)
    endTable <- regexpr("\\\\end\\{tabular\\}", x)

    beginTable <- which(beginTable > 0)
    endTable   <- which(endTable > 0)

    if(length(beginTable)==0){
      break
    }

    rmdTable <- processOneTabular(x, begin = beginTable[1], 
                                end = endTable[1], 
                                tabNum = tabNum)
    # now replace the rows of tabular' with Rmd table'
    x <- c( x[1:(beginTable[1]-1)], 
            rmdTable, 
            x[(endTable[1]+1):length(x)])
    cat(paste0("table ",tabNum, " was converted successfully! \n"))
    tabNum <- tabNum + 1
  }

  # Now process any tabular environments in text by themselves
  ## option if a tabular within other environments
  if (isTRUE(tabInbed)){  # default "tabInbed = TRUE"
    repeat{
      beginTable <- which(regexpr("\\\\begin\\{tabular\\}", x)>0)
      endTable <- which(regexpr("\\\\end\\{tabular\\}", x)>0)
      
      beginFig <- which(regexpr("\\\\begin\\{figure\\}", x)>0)
      endFig <- which(regexpr("\\\\end\\{figure\\}", x)>0)
      
      if(length(beginFig)>0){
        repeat{
          if( length(beginTable) > 0){
            inAFig <- (beginFig < beginTable[1]) & (endTable[1] < endFig)
            if( any(inAFig) ){
              # tabular is imbedded in a figure, do nothing
              beginTable <- beginTable[-1]
              endTable <- endTable[-1]
            } else {
              break
            }
          } else {
            break
          }
        }
      }
      
      if(length(beginTable)==0){
        break
      }
      
      rmdTable <- processOneTabular(x, beginTable[1], endTable[1], NA)
      x <- c( x[1:(beginTable[1]-1)], rmdTable, x[(endTable[1]+1):length(x)])
    }
  }
  
  return(x)
}
