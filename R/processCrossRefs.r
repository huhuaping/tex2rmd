#' @title processCrossRefs
#'
#' @description Process the cross references in a Latex file, substituting
#' the correct number in for all the \\ref{} tags.
#'
#' @param tex A character vector representing the lines of a Latex file.
#'
#' @param fileRoot The root name of the Latex file. This function
#' looks for an reads the file fileRoot.aux.
#'
#' @return A character vector representing the lines of a Latex file,
#' but with all the cross references fixed up.
#'
#' @export
#'
processCrossRefs <- function(tex, fileRoot){

  linesWithRefs <- grep("\\\\ref\\{",tex)

  if( length(linesWithRefs) > 0){
    if( !file.exists(paste0(fileRoot, ".aux")) ){
      stop("File contains cross-references, but no .aux file found. Process the Latex file and re-run.")
    }

    # build the citation data frame
    auxFile <- readLines(paste0(fileRoot, ".aux"))
    labLines <- grep("\\\\newlabel\\{", auxFile)
    auxFile <- auxFile[labLines] # one per line (I'm pretty sure)

    crossRefs <- data.frame(matrix(NA,length(auxFile),3))
    names(crossRefs) <- c("label", "num", "page")
    for(i in 1:length(auxFile)){
      crossRefs$label[i] <- parseTex(auxFile[i],1)
      tmp <- parseTex(auxFile[i],2)
      crossRefs$num[i] <- parseTex(tmp,1)
      crossRefs$page[i] <- parseTex(tmp,2)
    }

    # Substitute the cross references
    for(i in 1:length(linesWithRefs)){
      texLine <- tex[linesWithRefs[i]]
      refLab <- returnCommandArg(texLine,"ref")
      repeat{
        refnum <- crossRefs$num[ crossRefs$label == refLab ]
        refij <-  regexpr(paste0("\\\\ref\\{",refLab,"\\}"),texLine)
        part1 <- substring(texLine, 1, refij-1)
        part2 <- substring(texLine, refij + attr(refij,"match.length"))
        texLine <- paste0(part1, refnum, part2)

        refLab <- returnCommandArg(texLine,"ref")
        if( nchar(refLab) == 0 ){
          break
        }
      }
      tex[linesWithRefs[i]] <- texLine
    }



  }

  tex
}
