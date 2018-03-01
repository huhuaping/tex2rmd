#' @title parseBib
#'
#' @description Parse a bibitem and return the bibliography item
#'
#' @param x A latex bibitem entry from a Latex .bbl file
#'
#' @return A string containing the bibitem
#'
#' @author Trent McDonald
#'
#' @examples
#' bibItem <- "\\bibitem[{Bai et~al.(2011)Bai, L{\\\"{u}},
#' Wang, Zhou and Ding}]{Bai2011a} Bai, S., L{\\\"{u}}, G.,
#' Wang, J., Zhou, P. and Ding, L. (2011)
#' {GIS-based rare   events logistic regression for
#' landslide-susceptibility mapping of   Lianyungang, China}.
#' \\newblock \\textit{Environmental Earth Sciences},
#' \\textbf{62}, 139--149. "
#'
#' parseBib(bibItem)
#'
#' @export

parseBib <- function(x){

  delims <- "{}"
  delims <- substring(delims, 1:2, 1:2)
  openDelim <- delims[1]
  endDelim <- delims[2]

  x <- sub("\\[","\\{",x)
  x <- sub("\\]","\\}",x)

  # remove anything before first {
  RegE <- paste0("^\\W?\\W?[^\\", openDelim, "]*")
  x <- sub(RegE,"",x)

  xchars <- substring(x, 1:nchar(x), 1:nchar(x))

  argNum <- 2
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

  x <- substring(x, end+2, nchar(x))

  x <- sub("^ ","",x)
  x <- gsub("\\\\newblock","",x)

  # put title of article in quotes
  # Title is right after year in authoryear format e.g., Mcdonald (2018) Title.
  # But other styles are different, e.g., plain style.
  tmp <- regexpr("\\) +\\{", x)
  if( tmp > 0 ){
    tmp <- tmp + attr(tmp,"match.length") - 1
    substring(x, tmp, tmp) <- '"'


    tmp <- regexpr("\\}\\. ", x)
    if(tmp > 0){
      substring(x, tmp, tmp) <- '"'
    }
  }

  # special case of a book with \textit{} surrounding title
  # Don't need this -- textit replaced after this
  # tmp <- regexpr("\\\\textit\\{\\{", x)
  # if(tmp > 0){
  #   substring(x, tmp, tmp+attr(tmp,"match.length")) <- '"\\textit{'
  # }

  # remove squiggle
  x <- gsub("~"," ", x)

  # remove url stuff
  x <- sub("\\\\url.+$","",x)

  # Must deal with {\em Text} here because \textit processing will zap it later.
  tmp <- regexpr("\\{\\\\em[^\\}]+\\}", x)
  if( any(tmp > 0) ){
    for(i in 1:length(tmp)){
      x <- paste0(substring(x,1,tmp[i]-1),
                  "*",
                  substring(x,tmp[i]+5,tmp[i]+attr(tmp,"match.length")[i]-2),
                  "*",
                  substring(x,tmp[i]+attr(tmp,"match.length")[i]))
    }
  }

  # remove any remaining { and }
  x <- gsub("(\\{|\\})","", x)

  x
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
