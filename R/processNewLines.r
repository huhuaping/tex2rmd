#' @title processNewLines - Process manual new lines
#'
#' @description Process (remove) double blanks at the end of
#' a line which causes R Markdown to start a new line, and
#' replace double backslashes or newline commands with double blanks to force
#' a new line.
#'
#' @param tex a vector of Latex file lines
#' @param keep_slash logical default "keep_slash = TRUE"
#'
#'
#' @return a processed vector of Latex file lines with lists
#' formated in R Markdown.
#'
#' @export
#'
processNewLines <- function(tex, keep_slash = TRUE){

  tex <- gsub("\\ \\ $", "\\ ", tex)

  # double backslashes as new lines
  ## this will affect the equation environment with double backslashes
  ## so no replace will be the default option
  if (!isTRUE(keep_slash)){
    dblbacks <- gregexpr("\\\\\\\\", tex)
    for( i in 1:length(dblbacks)){
      if( dblbacks[[i]] > 0){
        part1 <- substring(tex[i], 1, dblbacks[[i]]-1)
        part1 <- paste0(part1, "  ")
        part3 <- substring(tex[i], dblbacks[[i]]+attr(dblbacks[[i]],"match.length"))
        if(nchar(part3)>0){
          tex <- c(tex[1:(i-1)], part1, part3, tex[(i+1):length(tex)])
        } else {
          tex <- c(tex[1:(i-1)], part1, tex[(i+1):length(tex)])
        }
      }
    }
  }
  

  # \\newline
  dblbacks <- gregexpr("\\\\newline", tex)
  for( i in 1:length(dblbacks)){
    if( dblbacks[[i]] > 0){
      part1 <- substring(tex[i], 1, dblbacks[[i]]-1)
      part1 <- paste0(part1, "  ")
      part3 <- substring(tex[i], dblbacks[[i]]+attr(dblbacks[[i]],"match.length"))
      if(nchar(part3)>0){
        tex <- c(tex[1:(i-1)], part1, part3, tex[(i+1):length(tex)])
      } else {
        tex <- c(tex[1:(i-1)], part1, tex[(i+1):length(tex)])
      }
    }
  }


  tex

}

