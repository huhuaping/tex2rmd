#' @title readCitations
#'
#' @description Reads the .AUX file and create a citation table so that
#' citations can be substituted into the translated .Rmd file.
#'
#' @param auxFile A character scaler specifying the .aux file.
#'
#' @return A data frame containing information necessary to do
#' the citation substitutions.  The data frame contains one line per
#' reference in the paper and the following
#' columns:
#' \enumerate{
#'   \item  key : the Bibtex key of the reference
#'   \item order : the rank or order of the reference in the bibliography.
#'   Bibliography is sorted by this number.
#'   \item year : year of the citation
#'   \item citeAuth : the author string for the in-text citation
#'   \item fullAuth : the full author string for the citation (expanded.  no "et al.")
#' }
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

    # The following is where we distiguish citation styles
    ord <- parseTex(secondArg, 1)
    if( nchar(ord) > 0){
      # AuthorYear format
      yr <- parseTex(secondArg, 2)
      auth <- parseTex(secondArg, 3)
      lst <- parseTex(secondArg, 4)

      auth <- sub("^\\{","",auth)
      auth <- sub("\\}$","",auth)
      lst  <- sub("^\\{","",lst)
      lst  <- sub("\\}$","",lst)

      auth <- sub("et~","et ", auth)
    } else {
      # Plain style
      ord <- i
      yr <- NA
      auth <- secondArg
      lst <- ""
    }

    cite.df <- rbind( cite.df, data.frame(
                  key  = lab,
                  order= as.numeric(ord),
                  year = as.numeric(yr),
                  citeAuth = auth,
                  fullAuth = lst))

  }

  cite.df
}

