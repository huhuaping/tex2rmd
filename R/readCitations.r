#' @title readCitations
#'
#' @description Reads the .AUX file and create a citation table so that
#' citations can be substituted into the translated .Rmd file.
#'
#'
#' @export
readCitations <- function(auxFile){

  aux <- readLines(auxFile)

  cites <- aux[grep("\\\\bibcite",aux)]

  tmp2 <- regexpr("\\{[^\\}]*", cites)
  labels <- substr(cites, tmp2+1, tmp2 + attr(tmp2,"match.length") - 1)


  cite.df <- NULL
  for( i in 1:length(cites)){
    lab <- parseTex(cites[i], 1)

    secondArg <- parseTex(cites[i], 2)
    ord <- parseTex(secondArg, 1)
    yr <- parseTex(secondArg, 2)
    auth <- parseTex(secondArg, 3)
    lst <- parseTex(secondArg, 4)

    auth <- sub("^\\{","",auth)
    auth <- sub("\\}$","",auth)
    lst  <- sub("^\\{","",lst)
    lst  <- sub("\\}$","",lst)

    auth <- sub("et~","et ", auth)

    cite.df <- rbind( cite.df, data.frame(
                  key  = lab,
                  order= as.numeric(ord),
                  year = as.numeric(yr),
                  citeAuth = auth,
                  fullAuth = lst))

  }

  cite.df
}

