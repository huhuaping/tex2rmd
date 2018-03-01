#' @title processBibliography
#'
#' @description Process the bibliography of a Latex document, substituting
#' the references and inserting the formatted bibliography.
#'
#' @param tex A vector of character strings, each element representing
#' a single line in a Latex file.
#'
#' @param fileRoot The name of the Latex file being processed, without
#' the .tex extension. If latex file is <latex>.tex, fileRoot should be
#' <latex>.
#'
#' @details A <fileRoot>.aux and a <fileRoot>.bbl file must both be
#' present and in the same
#' directory as the <fileRoot>.tex file. Hence, one must compile the Latex
#' document first to generate these files.
#'
#' Citation styles allowed here are \code{citet} (cite text,
#' e.g., McDonald (2018)) and \code{citep} (cite paratheses,
#' e.g., (McDonald, 2018)).
#' The plain \code{cite} style is treated exactly like \code{citep}.
#'
#' @return A vector of character strings representing the file
#' but with references substituted and the bibliography in it.
#'
#' @export

processBibliography <- function(tex,fileRoot){

  bib <- grep("\\\\bibliography\\{",tex)
  if(length(bib)>0){

    # ---- First figure out the style
    bibStylePos <- grep("\\\\bibliographystyle\\{",tex)
    if(length(bibStylePos) > 0){
      bibStyle <- parseTex(tex[bibStylePos],1)
      tex <- c(tex[1:(bibStylePos-1)], tex[(bibStylePos+1):length(tex)])
    } else {
      bibStyle <- "plain"  # not sure this is correct default
    }
    if( bibStyle == "plain"){
      citeStartChar <- "["
      citeEndChar <- "]"
      citeSep <- ","
    } else {
      citeStartChar <- "("
      citeEndChar <- ")"
      citeSep <- "; "
    }

    # ---- Second, process the bibliography
    bbl <- readBibliography(paste0(fileRoot, ".bbl"), bibStyle)
    bbl <- c(rbind(bbl," "))
    bbl <- c("# References"," ", bbl)
    tex <- c(tex[1:(bib-1)], bbl, tex[(bib+1):length(tex)])

    # ---- Third, fix up all the in-text citations
    # Note, cite.df has same column names but columns change
    # definition depending on bibStyle
    cite.df <- readCitations( paste0(fileRoot, ".aux") )

    # change \cite to \citep before processing \cite(t|p).
    # \cite is for plain style mostly
    tex <- gsub("\\\\cite\\{", "\\\\citep\\{", tex)

    # For your info: regexpr that matches \cite and \citet and \citep is:
    #  "\\\\cite(t|p)?\\{[^\\}]*\\}"
    # The following regex must match the whole deal, from \cite{ to },
    # otherwise the replacement leaves stuff.
    citeRegExpr <- "\\\\cite(t|p)\\{[^\\}]*\\}"

    tmp2 <- gregexpr( citeRegExpr, tex)
    linesWithRefs <- unlist(lapply(tmp2, function(x){sum(x!=-1)}))


    for(i in which(linesWithRefs > 0)){
      for(j in 1:linesWithRefs[i]){
        # must re-compute position because when j>1, position could have changed
        citePos <- regexpr( citeRegExpr, tex[i])
        keys <- substring(tex[i],
                          citePos+7,
                          citePos+7+attr(citePos,"match.length")-9)
        keys <- gsub(" ","",keys)
        citeType <- substring(tex[i], citePos+1, citePos+5)
        Keys <- strsplit(keys, ",")[[1]]

        if( citeType == "citet" ){
          # only one key allowed in each citet, could say Keys[1]
          citeStr <- paste0(cite.df$citeAuth[ cite.df$key == keys ], " (",
                            cite.df$year[ cite.df$key == keys], ")")
        } else {
          citeStr <- citeStartChar
          for( k in 1:length(Keys)){
            if( k > 1 ){
              citeStr <- paste0(citeStr, citeSep)
            }
            if(bibStyle == "plain"){
              citeStr <- paste0(citeStr,
                                cite.df$citeAuth[ cite.df$key == Keys[k] ])
            } else {
              # assume Author, Year
              citeStr <- paste0(citeStr,
                              cite.df$citeAuth[ cite.df$key == Keys[k] ], ", ",
                              cite.df$year[ cite.df$key == Keys[k]])
            }
          }
          citeStr <- paste0(citeStr, citeEndChar)
        }
        strBeg <- substring(tex[i], 1, citePos-1)
        strEnd <- substring(tex[i], citePos+attr(citePos,"match.length"), nchar(tex[i]))
        tex[i] <- paste0(strBeg, citeStr, strEnd)
      }
    }
  }

  tex
}
