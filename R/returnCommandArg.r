#' @title returnCommandArg
#'
#' @description Return an argument of a Tex command.
#'
#' @details Tex command is something like "\\comand[opt1]{arg1}{arg2}",
#' and this routine returns one of those arguments, either optional or required.
#'
#' @param x An array of character strings (lines from a Latex file)
#' to process.
#'
#' @param cmd A character string naming the command to look for in \code{x}.
#' This is the command, without the "\\".  \code{cmd} is simply pasted to the
#' end of two slashes "\\" and the results becomves a regular expression.
#' Hence, regular expressions like "(ref|label)" will return arguments
#' for the first \\ref or \\label command in \code{x}.  \code{cmd} = "[0-9]"
#' will return arguments of any command with a number in it. Etc.
#'
#' @param argNum The number of the argument to \code{cmd} that is desired.
#'
#' @param optArg True or false depending on whether one of the optional arguments
#' or one of the required arguments is desired.
#'
#' @return The desired argument as a single text string. If the command
#' does not exist in \code{x}, a string of length 0 (i.e., "") is returned.
#' If the requested argument does not exist (e.g., \code{argNum} too big),
#' a string of length 0 (i.e., "") is returned.
#'
#' @examples
#'
#' x <- c("kdkdk", "kdkd  \\title{My Title", "is Sir} dkdk")
#' returnCommandArg(x, "title")
#'
#' x <- c("\\cmd[opt1][opt2]{arg1}{arg2}")
#' returnCommandArg(x, "cmd", 1)  # "arg1"
#' returnCommandArg(x, "cmd", 2)  # "arg2"
#' returnCommandArg(x, "cmd", 1, TRUE)  # "opt1"
#' returnCommandArg(x, "cmd", 2, TRUE)  # "opt2"
#'
#' @export
#'
returnCommandArg <- function(x, cmd, argNum=1, optArg=FALSE){

  # Make one long string
  x <- paste(x,collapse = " ")

  # Find the command
  cmd <- paste0("\\\\",cmd)
  pos <- regexpr(cmd, x)
  if( pos < 0 ){
    return("")
  }

  # remove stuff before command
  if( pos > 1){
    x <- substring(x, pos, nchar(x))
  }

  # Return argument
  if( optArg ){
    ans <- parseTex(x,argNum,delims="[]")
  } else {
    ans <- parseTex(x,argNum,delims="{}")
  }

  ans
}
