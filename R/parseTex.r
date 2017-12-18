#' @title parseTex
#'
#' @description Parse a Tex command and return a specified argument.
#'
#' @param x A latex command, like "\\bibcite{text}{text}"
#'
#' @param argNum The number of the argument desired.
#'
#' @param delims The delimiters to use.  This must be a single
#' string, and only the first two characters are used.  First character
#' is the opening delimiter, second character is closing delimiter.
#'
#' @param warn Logical scaler.  If TRUE, issue a warning if the
#' requested argument is not found. Careful here: if \code{warn} is FALSE,
#' the calling procedure will not be able to distinguish between
#' an argument not found and the empty argumen (i.e., "{}").
#'
#' @return A string, the desired argument. If the argument is not found,
#' the empty string is returned and a warning is fired if \code{warn==TRUE}.
#'
#' @author Trent McDonald
#'
#' @examples
#' tex <- "command{1}{1984}{{Albert and Anderson}}{{}}"
#' parseTex(tex, 1)
#' parseTex(tex, 3)
#'
#' @export

parseTex <- function(x, argNum, delims="{}", warn=FALSE){

  delims <- substring(delims, 1:2, 1:2)
  openDelim <- delims[1]
  endDelim <- delims[2]

  # Make sure openDelim is first character of string
  if( substring(x,1,1) != openDelim){
    # remove begining white space
    #x <- sub("^\\s+","",x)
    # Get rid of everything up to first openDelim
    RegE <- paste0("^[^\\",openDelim, "]+")
    x <- sub(RegE,"",x, perl = TRUE)
  }

  # Make sure endDelim is last character of string
  if( substring(x, nchar(x), nchar(x)) != endDelim){
    RegE <- paste0("[^\\",endDelim,"]+$")
    x <- sub(RegE, "", x, perl=TRUE)
  }

  # burst string, one char per vector element
  xchars <- substring(x, 1:nchar(x), 1:nchar(x))

  nbrace <- 0
  narg <- 1
  pos <- 1
  start <- end <- 1
  repeat{
    if(nbrace == 0 & xchars[pos] == openDelim){
      start <- pos + 1
    }

    if( xchars[pos] == openDelim ){
      nbrace <- nbrace + 1
    } else if( xchars[pos] == endDelim){
      nbrace <- nbrace - 1
    }

    if(nbrace == 0 & xchars[pos] == endDelim){
      end <- pos - 1
    }

    # cat(paste(xchars[pos],'nbrace=',nbrace, "narg=", narg, "argNum=",argNum, "\n"))

    if( nbrace == 0 & narg == argNum ){
      break
    }
    if( pos >= length(xchars)){
      warning(paste0("Argument ", argNum, " not found in tex string"))
      start <- end <- -1
      break
    }
    if( nbrace == 0 ){
      narg <- narg + 1
    }
    pos <- pos + 1
  }

  substring(x, start, end)

}

# tmp2 <- aux[ grep("\\\\bibcite", aux)][1]
# tmp3 <- parseTex( tmp2, 2 )
# print(c(tmp3=tmp3))
#
# tmp4 <- parseTex( tmp3, 2)
# print(c(tmp4=tmp4))
# tmp2 <- paste0(tmp2,"[ab{c}d[eeee]]")
# tmp3 <- parseTex( tmp2, 1, delims = "[]" )
# print(c(tmp3=tmp3))
