#' @title tex2rmd
#'
#' @description Converts a raw LaTex file to RMarkdown format, then to Word
#' format.  Can optionally convert to any format supported by RMarkdown
#'
#' @param infile Full path to the input Latex file.
#'
#' @return The markdown code is written to a file named <root>.Rmd,
#' where \code{inFile} is <root>.tex.  The markdown code in the
#' form of a vector, one line per element, is invisably returned.
#'
#' @details
#' The general workflow to convert a Latex document into Markdown,
#' and eventually Word, is as follows:
#' \enumerate{
#'   \item Compile the latex document, using pdftex or whatever,
#'   so that the .aux and .bbl files are generated.
#'   \item Run \code{tex3rmd} specifying the file containing
#'   the raw Latex code as the input.  The associated .aux
#'   and .bbl files must be in the same directory. This will
#'   generate a markdown document of the same root name but
#'   with .Rmd extension.
#'   \item Compile the .Rmd file.  In RStudio, open it and hit <ctrl>-<shift>-K.
#' }
#'
#' @author Trent McDonald
#'
#' @examples
#' \notrun{
#' tex2rmd("main.tex")
#'  }
#' @export

tex2rmd <- function(infile){

  fileRoot <- sub("\\..+$","",infile)
  tex <- readLines(infile)

  # ---- Put in place holders for legit percent signs
  tex <- gsub("\\\\%","##",tex)

  # remove any comments
  tex <- sub("\\%.+$","",tex)

  # ---- Restore legit percent signs
  tex <- gsub("\\#\\#","%",tex)

  # ---- Remove Latex double quotes
  tex <- gsub("``","'",tex)
  tex <- gsub("''","'",tex)


  # Remove header
  headPos <- grep("\\\\begin\\{document\\}",tex)
  header <- tex[1:headPos]
  tex <- tex[(headPos+1):length(tex)]

  # extract title
  title <- header[grep("\\\\title\\{",header)]
  title <- sub("\\\\title\\{","", title)
  title <- sub("\\}","",title)

  # extract author(s)
  auth <- header[grep("\\\\author", header)]
  tmp3 <- regexpr("\\{.+\\}",auth)
  auth <- substring(auth,tmp3+1, tmp3 + attr(tmp3,"match.length") - 2)
  auth <- auth[nchar(auth)>0]
  if( length(auth) > 1 ){
    auth[length(auth)] <- paste0("and ", auth[length(auth)])
  }
  auth <- paste(auth, collapse = ", ")

  # extract date
  dt <- header[grep("\\\\date\\{",header)]
  if(length(dt) == 0){
    dt <- format(Sys.time(), "%d-%b-%Y")
  } else {
    tmp3 <- regexpr("\\{.+\\}",dt)
    dt <- substring(dt,tmp3+1, tmp3 + attr(tmp3,"match.length") - 2)
  }


  # ---- Remove maketitle
  tex <- sub("\\\\maketitle","",tex)

  # ---- Keywords
  keyw <- tex[grep("\\\\keywords\\{",tex)]
  if(length(keyw) > 0){
    tmp3 <- regexpr("\\{.+\\}",keyw)
    keyw <- substring(keyw,tmp3+1, tmp3 + attr(tmp3,"match.length") - 2)
    tex <- sub("\\\\keywords\\{.+\\}", "", tex)
  }


  # ---- Fix up Abstract
  begline <- grep("\\\\begin\\{abstract\\}",tex)
  endline <- grep("\\\\end\\{abstract\\}",tex)
  abst <- paste(tex[begline:endline], collapse=" ")
  tmp3 <- regexpr("\\\\begin\\{abstract\\}.+\\\\end\\{abstract\\}", abst)
  abst <- substring(abst,tmp3+16, tmp3 + 16 + attr(tmp3,"match.length") - (16+15))
  abst <- paste("**Abstract:**", abst)
  tex[begline] <- abst
  tex <- tex[-((begline+1):endline)]

  if(length(keyw) > 0){
    tex <- c(tex[1:begline], " ", paste("*Keywords:*", keyw)," ", tex[(begline+1):length(tex)])
  }


  # ---- Add bibliography (now, so texttt and textbf get changed)
  bibLoc <- grep("\\\\bibliography\\{",tex)
  if( length(bibLoc) > 0 ){
    bbl <- readBibliography(paste0(fileRoot, ".bbl"))
    bbl <- c(rbind(bbl," "))
    bbl <- c("# Refernces"," ", bbl)
    tex <- c(tex[1:(bibLoc-1)], bbl, tex[(bibLoc+1):length(tex)])
  }


  # ---- Sections
  # Sections must be on a line by themselves.  Can't have "\section{A} more text"
  seclines <- grep("\\\\section\\*?\\{", tex)
  secs <- tex[seclines]
  secs <- sub("\\\\section\\*?\\{","",secs)
  secs <- sub("\\}","", secs)
  tex[seclines] <- paste("#", secs)

  # ---- SubSections
  # Subsections must be on a line by themselves.
  seclines <- grep("\\\\subsection\\*?\\{", tex)
  secs <- tex[seclines]
  secs <- sub("\\\\subsection\\*?\\{","",secs)
  secs <- sub("\\}","", secs)
  tex[seclines] <- paste("##", secs)

  # ---- SubSubSections
  # Must be on a line by themselves.
  seclines <- grep("\\\\subsubsection\\*?\\{", tex)
  secs <- tex[seclines]
  secs <- sub("\\\\subsubsection\\*?\\{","",secs)
  secs <- sub("\\}","", secs)
  tex[seclines] <- paste("###", secs)

  # ---- Texttt
  tex <- convertTexTag(tex, "texttt", "`")

  # ---- Textit
  tex <- convertTexTag(tex, "textit", "*")

  # ---- Textbf
  tex <- convertTexTag(tex, "textbf", "**")

  # ---- Fix up citations
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

  # Process tables
  tex <- processTables(tex)

  # process display equations
  tex <- processDisplayEqns(tex)

  # add header info to tex lines
  header <- c("---",
              paste0('title: "',title,'"'),
              paste0('author: "',auth,'"'),
              paste0('date: "',dt,'"'),
              "output: word_document",
              "---")
  tex <- c(header, tex)

  # Make outfile name
  outfile <- paste0(sub("\\..+$","",infile), ".Rmd")

  # write out all modified text
  writeLines(tex, outfile)

  invisible(tex)
}

