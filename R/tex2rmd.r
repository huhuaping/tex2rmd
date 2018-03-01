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

  # Images can also be included using either raw HTML with img
  # tags (<img src = "" />) or using markdown directly (![image](imagepath)).
  #
  # For differencing text files, try online tools or suggestions here:
  #https://stackoverflow.com/questions/4078933/find-difference-between-two-text-files-with-one-item-per-line


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
  title <- returnCommandArg(header, "title")
  if(nchar(title) == 0){
    title <- paste("Contents of", fileRoot)
  } else {
    title <- convertTexTag(title, "textbf", "")
  }

  # extract author(s)
  auth <- returnCommandArg(header, "author")
  if(nchar(auth) == 0){
    auth <- Sys.info()["user"]
  }
  # if there's a comma between authors, replace last with ", and"
  commas <- gregexpr(",", auth)[[1]]
  if( commas[1] > 0 ){
    if(length(commas) > 1){
      harvardComma <- ","
    } else {
      harvardComma <- ""
    }
    lastComma <- commas[length(commas)]
    auth <- paste0( substring(auth,1,lastComma-1), harvardComma,
                    " and", substring(auth,lastComma+1))
  }
  # Remove any textbf from author string
  auth <- convertTexTag(auth, "textbf", "")

  # extract date
  dt <- returnCommandArg(header, "date")
  if(nchar(dt) == 0){
    dt <- format(Sys.time(), "%d-%b-%Y")
  }


  # ---- Remove maketitle
  tex <- sub("\\\\maketitle","",tex)

  # ---- Remove end{document}
  tex <- sub("\\\\end\\{document\\}","",tex)

  # ---- Keywords
  keyw <- tex[grep("\\\\keywords\\{",tex)]
  if(length(keyw) > 0){
    tmp3 <- regexpr("\\{.+\\}",keyw)
    keyw <- substring(keyw,tmp3+1, tmp3 + attr(tmp3,"match.length") - 2)
    tex <- sub("\\\\keywords\\{.+\\}", "", tex)
  }


  # ---- Fix up Abstract
  begline <- grep("\\\\begin\\{abstract\\}",tex)
  if( length(begline) >0){
    endline <- grep("\\\\end\\{abstract\\}",tex)
    abst <- paste(tex[begline:endline], collapse=" ")
    tmp3 <- regexpr("\\\\begin\\{abstract\\}.+\\\\end\\{abstract\\}", abst)
    abst <- substring(abst,tmp3+16, tmp3 + 16 + attr(tmp3,"match.length") - (16+15))
    abst <- paste("**Abstract:**", abst)
    tex[begline] <- abst
    tex <- tex[-((begline+1):endline)]
  }

  if(length(keyw) > 0){
    tex <- c(tex[1:begline], " ", paste("*Keywords:*", keyw)," ", tex[(begline+1):length(tex)])
  }


  # ---- Fix up bibliography and citations
  #      Do this here so that textbf and texttt get changed below.
  tex <- processBibliography(tex, fileRoot)

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

  # ---- Process tables
  tex <- processTables(tex)

  # ---- Process Figures
  tex <- processFigures(tex)

  # ---- Process display equations
  tex <- processDisplayEqns(tex)

  # ---- Process crossRefs
  tex <- processCrossRefs(tex, fileRoot)

  # ---- Process labels
  #      Just need to remove labels. All Table and Fig labels are taken care
  #      of, and other labels should just be deleted.
  tex <- gsub("\\\\label\\{[^\\}]+\\}", "", tex)

  # ---- Process List environments
  tex <- processLists(tex)

  # ---- Remove double blanks at end of lines
  tex <- processNewLines(tex)

  # ---- add header info to tex lines
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

