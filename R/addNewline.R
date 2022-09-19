#' Add newlines to a vector of Latex file lines, used in cases of section/subsection, 
#' or double dollar symbol for math equation
#'
#' @param x a vector of Latex file lines
#' @param grep_ptn match pattern for function "grep"
#' @param paired character, specify paired status c("none","start","end"),
#'     the default "paired = 'none'"
#' @param pos character, where to add newline c("before","after"),
#'     the default "pos = 'after'"
#' @details use this function to add newlines in case section/subsection, 
#' or double dollar symbol for math equation
#'
#' @return vector
#' @export 
#'
#' @examples
#' \dontrun{
#'  ## add newline before "$$" by using custom function
#' tex <- add_newline(          
#'  x = tex,  grep_ptn = "\\$\\$",
#'  paired = "start",
#'  pos = "before")
#' ## add newline after "$$" by using custom function
#' tex <- add_newline(          
#'  x = tex,  grep_ptn = "\\$\\$",
#'  paired = "end",
#'  pos = "after")
#' }
#' 
add_newline <- function(x, grep_ptn, 
                        paired = "none",
                        pos ="after"){
  #search target lines' positions
  lines <- grep(grep_ptn, x)
  # identify add type 
  row_tar <- seq_len(length(lines))%%2  # row indicator
  if (paired=="start" ){
    lines <- lines[row_tar==1]   # start row (odd)
  } else if (paired=="end" ){
    lines <- lines[row_tar==0]   # end row (enven)
  } else if (paired =="none")  {
    lines <- lines
  }
  
  # add line by loop
  for (i in 1:length(lines)) {
    # tot lines of the text
    tot <- length(x)
    # all target lines
    line_tar <- lines[i]
    
    # add line conditional
    if (pos== "before"& paired =="none"){
      x <- c(x[1:(line_tar-1)], 
             "",   # add empty before
             x[line_tar:tot])
    } else if (pos== "after"& paired =="none"){
      x <- c(x[1:line_tar], 
             "",   # add empty after 
             x[(line_tar+1):tot])
    } else if (pos== "before" & paired =="start"){
      x <- c(x[1:(line_tar-1)], 
             "",   # add empty before
             x[line_tar:tot])
    } else if (pos== "after" & paired =="end"){
      x <- c(x[1:line_tar], 
             "",   # add empty after 
             x[(line_tar+1):tot])
    } 
    #cat(lines[i],sep = "\n")
    
    # search again
    lines <- grep(grep_ptn, x)
    row_tar <- seq_len(length(lines))%%2  # row indicator
    if (paired=="start" ){
      lines <- lines[row_tar==1]   # start row (odd)
    } else if (paired=="end" ){
      lines <- lines[row_tar==0]   # end row (enven)
    } else if(paired =="none")  {
      lines <- lines
    }
    
  } # end loop
  return(x)
}
