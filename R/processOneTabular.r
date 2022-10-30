#' @title processOneTable
#'
#' @description Process one latex table into Rmd format
#'
#' @param x A vector of latex lines
#'
#' @param begin The beginning line number of the latex table to process
#'
#' @param end The ending line number of the latex table to process
#' 
#' @param tabNum integer for table numbering.
#'
#' @details The \\begin{table} and \\end{table} command must be on lines by themselves.
#'
#' The header of a latex table is signified by the first \\hline, or the
#' last row with a \\multicolumn command.  Only one-line headers are
#' allowed in Markdown.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @import purrr 
#' @importFrom dplyr mutate
#'   
#' @export
processOneTabular <- function(x, begin, end, tabNum){

  # cat(paste("in processOneTable.r",begin,end,"\n"))
  # rows of tabular
  x <- x[begin:end]

  cap <- regexpr("\\\\caption\\{", x)
  if(any(cap > 0)){
    # has a caption
    tmp <- x[which(cap>0):length(x)]
    tmp <- paste(tmp, collapse = " ")
    caption <- parseTex(tmp, 1)
    caption <- paste0("_**Table ", tabNum, "**: ", caption,"_")
  } else {
    caption <- ""
  }


  beginTable <- regexpr("\\\\begin\\{tabular\\}", x)
  endTable <- regexpr("\\\\end\\{tabular\\}", x)


  # assume only one tabular environment in each table
  x <- x[which(beginTable>0):which(endTable>0)]

  # print(x)
  # cat("=========\n")

  just <- parseTex(x = x[1],argNum = 2)
  # cat("-----\n")
  # print(x)
  just <- gsub("\\|","",just)
  # Figure out how to handle other column designators, like p{1.5in}
  just <- substring(just, 1:nchar(just), 1:nchar(just))

  # find min line with & using grep
  # find max line with & using grep
  # subset to just those lines
  # concat into one long line
  
  # detect amp symbols
  amps <- which(regexpr("\\&",x)>0)
  if (length(amps)>0){
    # process line from // to //, splitting on &
    begLine <- min(amps)
    endLine <- max(amps)
  } else {   
    ## cases in which no amp symbols
    ## process line  on "\\\\"
    other <- which(regexpr("\\\\\\\\",x)>0)
    begLine <- min(other)
    endLine <- max(other)
  }
  
  x <- x[begLine:endLine]  # just the table cells
  x <- paste(x, collapse="") # one long string
  x <- strsplit(x, "\\\\\\\\")[[1]] # back to vector, this time one element per table row

  # find header row for later
  hline <- which(regexpr("\\\\hline",x)>0)  #assume first hline marks header
  if(length(hline)>0){
    hline <- min(hline)
  } else {
    # set header after last line with any command like \multicolumn
     hline <- which(regexpr("\\\\\\multicolumn\\{",x)>0)
    if(length(hline)>0){
      hline <- max(hline)+1 # but, maximum header is 3
    } else {
      hline <- 2
    }
  }
  
  if( hline > 2){
    warning("Multiline headers not allowed in Markdown tables. Last row of header used.")
    x <- x[(hline-1):length(x)]
  }

  x <- gsub("\\\\hline","",x) # remove all hlines
  x <- gsub("\\\\cline.*?\\{.*?\\}","",x) # and clines
  x <- gsub("\\\\multirow.*?\\{.*?\\}\\{.*?\\}","",x) # and multirow
  x <- gsub("\\\\thead\\{","",x)
  x <- gsub("\\}\\s+&"," &",x)
  x <- gsub("\\}$","",x)
  x <- gsub("\\}\\s+$","",x)
  x <- gsub("^\\{","",x)  # remove "{" leading row
  
  # detect empty first row ??
  emptyRows <- which(!stringr::str_detect(x,"[a-z0-9]"))
  if (length(emptyRows)>0) {
    x <- x[-c(emptyRows)]
  }

  # handle multiple columns
  hasMultiCol <- which(regexpr("\\\\multicolumn\\{",x)>0)
  
  if(length(hasMultiCol)>0){
    warning(paste("multicolumns detected.", length(hasMultiCol), "row(s) in Table",
                  tabNum, "have been cleaned and kept."))
    # show the row
    multicolRow <- x[hasMultiCol]
    
    ## first split
    col_splits <- stringr::str_split(multicolRow, pattern = "\\&") |>
      unlist()
    ## extract text
    text_splits <- col_splits |>
      sapply(FUN =  function(x){
        ifelse(
          stringr::str_detect(x,"\\\\multicolumn"),
          stringr::str_extract_all(x,
                          "(?<=\\\\multicolumn\\{\\d{1}\\}\\{c\\}\\{)(.+)"),
          x)}  ) |>
      unname() |>
      unlist()
    ## get numbers of the real amps
    num_splits <- col_splits |>
      stringr::str_extract_all(
        "(?<=\\\\multicolumn\\{)(\\d{1})(?=\\}\\{c\\}\\{)"
      ) |>
      sapply(
        FUN = function(x) {
        ifelse(length(x)==0,0, as.numeric(x)-1)}
        )
    ## construct real text and amps
    amp_splits <- data.frame(string = text_splits, 
                                 amp=num_splits) |>
      mutate(amps = purrr::map(amp,~paste0(rep("& ", .x), collapse = " "))) |>
      tidyr::unnest(cols = c(amps)) |>
      dplyr::mutate(amps = stringr::str_c(string, amps, sep = " "))
    ## paste all result
    multicolRow_tidy <- amp_splits |>
      dplyr::pull("amps") |>
      paste0(collapse = " & ") 
    
    # replace row and tidy it
    x[hasMultiCol] <- multicolRow_tidy
  }
  
  rmdMat <- NULL
  #j <-1
  for(j in 1:length(x)){
    colCells <- strsplit(x[j], "\\&")[[1]]
    colCells <- gsub("^\\s+","",colCells)  # remove leading blanks
    colCells <- gsub("\\s+$","",colCells)  # remove trailing blanks
    rmdMat <- rbind(rmdMat, matrix(colCells,1)) 
  }
  
  # remove empty column
  not_all_empty <- function(x) any(x!=" ")
  not_any_empty <- function(x) all(x!=" ")
  rmdMat <- rmdMat |>
    tibble::as_tibble() |>
    dplyr::mutate_all(~ifelse(.x=="", " ", .x)) |>
    dplyr::select(tidyselect::vars_select_helpers$where(not_all_empty)) |>
    as.matrix()


  # put in the separators and header line. In markdown, header is only 1 row always
  # print(just)

  rmdTab <- rep("|",nrow(rmdMat)+1)
  # j <- 1
  for(j in 1:ncol(rmdMat)){
    if( !(just[j] %in% c("l","r","c") )){
      just[j] <- "none"
    }
    colj <- format(rmdMat[,j], justify=just[j])
    hline <- paste0(rep("-",nchar(colj[1])), collapse = "")
    if(just[j]=="l"){
      substring(hline,1,1)<- ":"
    } else if(just[j]=="r"){
      substring(hline,nchar(hline),nchar(hline))<-":"
    }
    colj <- c(colj[1], hline, colj[-1])
    rmdTab <- paste0(rmdTab, colj, "|")
  }

  # put caption back in
  rmdTab <- c(caption, " ", rmdTab)

  rmdTab
}
