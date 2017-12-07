#' @title convertTexTag
#'
#' @description Convert a tex tag, like \\texttt, to something else for the RMD file
#'
#' @param x A vector of strings containing lines from the Latex file
#'
#' @param tagFrom The Latex tag to convert from
#'
#' @param mdTo The string to surround argument of Latex command with.
#'
#'
#' @export

convertTexTag <- function( x, tagFrom, mdTo ){

  RegE <- paste0("\\\\", tagFrom, "\\{[^\\}]*\\}")
  tmp2 <- gregexpr( RegE, x)
  linesWith <-  unlist(lapply(tmp2, function(x){sum(x!=-1)}))
  for(i in which(linesWith>0)){
    for(j in 1:linesWith[i]){
      tmp2 <- regexpr(RegE, x[i])
      tmp3 <- substring(x[i], tmp2+nchar(tagFrom)+2,
                        tmp2+nchar(tagFrom)+2
                      +attr(tmp2,"match.length")-(nchar(tagFrom)+4))
      tmp4 <- substring(x[i],1,tmp2-1)
      tmp5 <- substring(x[i],tmp2+attr(tmp2,"match.length"),nchar(x[i]))
      x[i] <- paste0(tmp4,
                    paste0(mdTo,tmp3,mdTo),
                    tmp5)
    }
  }

  x
}
