#' @title processOneFigure
#'
#' @description Process one latex Figure into Rmd format
#'
#' @param x A vector of latex lines
#'
#' @param begin The beginning line number in the overall Latex
#' file of the figure to process
#'
#' @param end The ending line number in the overall Latex file
#' of the figure to process
#'
#' @details The \\begin{Figure} and \\end{Figure} command must be
#' on lines by themselves.
#'
#'
#' @export
processOneFigure <- function(x, begin, end, figNum){

  # cat(paste("in processOneFigure.r",begin,end,"\n"))

  x <- x[begin:end]
  x <- paste(x, collapse = " ")

  cap <- returnCommandArg(x, "caption")
  if(nchar(cap)>0){
    cap <- paste0("**Figure ", figNum, "**: ",cap)
    # take out label because something dropps \\ in the sub at the bottom.
    cap <- sub("\\\\label\\{[^\\}]+\\}", "", cap)
  }

  out <- NULL

  repeat{
    figFile <- returnCommandArg(x, "includegraphics")

    if( nchar(figFile) == 0 ){
      break
    }

    # Must add the extension
    path <- regexpr("^.*[/|\\]", figFile)
    if(path < 0){
      path <- "."
    } else {
      path <- substring(path, 1, path)
    }
    figFileList <- list.files(path, full.names=TRUE)

    pos <- regexpr("[\\w\\.]+$", figFile)  # just file name with maybe extension
    figFile <- substring(figFile, pos, nchar(figFile))
    figFile <- figFileList[grep(figFile, figFileList)[1]]  # takes first if multiple


    figSize <- returnCommandArg(x, "includegraphics", optArg = T)
    figSize <- gsub(","," ",figSize)

    if(nchar(figSize)>0){
      out <- c(out, paste0("![](", figFile,"){", figSize, "}"))
    } else {
      out <- c(out, paste0("![](", figFile,")"))
    }

    # Remove include graphics command, to stop repeat loop
    x <- sub("\\\\includegraphics","", x) # only the first


  }

  # put caption in last one, if multiple
  figOut <- out[length(out)]
  figOut <- sub("\\[\\]", paste("\\[", cap, "\\]"), figOut)
  out[length(out)] <- figOut
  out <- c(out[-length(out)], " ", out[length(out)])

  out
}
