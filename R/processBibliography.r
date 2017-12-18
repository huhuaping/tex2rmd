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
#' @return A vector of character strings representing the file
#' but with references substituted and the bibliography in it.
#'
#' @export

processBibliography <- function(tex,fileRoot){

  bib <- grep("\\\\bibliography\\{",tex)
  if(length(bib)>0){
    # ---- First, process the bibliography
    bbl <- readBibliography(paste0(fileRoot, ".bbl"))
    bbl <- c(rbind(bbl," "))
    bbl <- c("# Refernces"," ", bbl)
    tex <- c(tex[1:(bib-1)], bbl, tex[(bib+1):length(tex)])

    # ---- Second, fix up all the in-text citations
    cite.df <- readCitations( paste0(fileRoot, ".aux") )

    tmp2 <- gregexpr( "\\\\cite(t|p)\\{[^\\}]*\\}", tex)
    linesWithRefs <- unlist(lapply(tmp2, function(x){sum(x!=-1)}))


    for(i in which(linesWithRefs > 0)){
      for(j in 1:linesWithRefs[i]){
        # must re-compute position because when j>1, position could have changed
        citePos <- regexpr( "\\\\cite(t|p)\\{[^\\}]*\\}", tex[i])
        keys <- substring(tex[i],
                          citePos+7,
                          citePos+7+attr(citePos,"match.length")-9)
        citeType <- substring(tex[i], citePos+1, citePos+5)
        Keys <- strsplit(keys, ",")[[1]]

        if( citeType == "citet" ){
          # only one key allowed in each citet, could say Keys[1]
          citeStr <- paste0(cite.df$citeAuth[ cite.df$key == keys ], " (",
                            cite.df$year[ cite.df$key == keys], ")")
        } else {
          citeStr <- "("
          for( k in 1:length(Keys)){
            if( k > 1 ){
              citeStr <- paste0(citeStr, "; ")
            }
            citeStr <- paste0(citeStr,
                              cite.df$citeAuth[ cite.df$key == Keys[k] ], ", ",
                              cite.df$year[ cite.df$key == Keys[k]])
          }
          citeStr <- paste0(citeStr, ")")
        }
        strBeg <- substring(tex[i], 1, citePos-1)
        strEnd <- substring(tex[i], citePos+attr(citePos,"match.length"), nchar(tex[i]))
        tex[i] <- paste0(strBeg, citeStr, strEnd)
      }
    }
  }

  tex
}
