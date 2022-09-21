#' @title tex2rmd
#'
#' @description Converts a raw LaTex file to RMarkdown format, then to Word
#' format.  Can optionally convert to any format supported by RMarkdown
#'
#' @param infile Full path to the input Latex file.
#' @param ext_out final output file extension, defualt "ext_out = '.Rmd'"
#' @param dir_img parent directory path of images
#' @param ext_img the extension of image, default "ext_img='.jpg'"
#' @param head2_only whether contains only heading 1 and 2 only, default "head2_only = TRUE"
#' @param keep_yml whether use yml header, default "keep_yml =TRUE"
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
#' \dontrun{
#' input_md <- "data-raw/main.tex"
#' dir.img <- "data-raw/images/"
#' tex2rmd(infile = input_md, ext_out = ".qmd",
#'   dir_img = dir.img,
#'   ext_img = ".jpg", 
#'   head2_only = TRUE,keep_yml =FALSE)
#'  }
#'  
#' @export


tex2rmd <- function(infile, ext_out = ".Rmd",
                    dir_img, 
                    ext_img=".jpg", 
                    head2_only=TRUE, keep_yml =TRUE){

  # Images can also be included using either raw HTML with img
  # tags (<img src = "" />) or using markdown directly (![image](imagepath)).
  #
  # For differencing text files, try online tools or suggestions here:
  #https://stackoverflow.com/questions/4078933/find-difference-between-two-text-files-with-one-item-per-line

  isTex <- stringr::str_detect(infile,"\\.tex$")
  if (!isTex) stop("The input is not a .tex file!")
  
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
  
  # ---- Fix up theorem----
  ## mathpix error ocr with: \section{Theorem 4.6 MSFE}
  myCases <- paste0(
    c("\\\\section\\{Theorem",
      "\\\\section\\{Assumption",
      "\\\\section\\{Proposition",
      "\\\\section\\{Stata Commands",
      "\\\\section\\{Stata do"),
    collapse = "|"
    )
  theoremlines <- grep(myCases, tex)
  theorems <- tex[theoremlines]
  theorems <- stringr::str_extract_all(
    theorems,
    "(?<=\\\\section\\{)(.+)(?=\\}$)" ) |> 
    unlist()
  # fix case: \section{Assumption $8.3$}
  theorems <- stringr::str_replace_all(
    theorems,
    "\\$(?=\\d.\\d)|(?<=\\d{1,2}\\.\\d{1,2})\\$", "" ) |> 
    unlist()
  tex[theoremlines] <- theorems
  
  # ---- Sections
  # Sections must be on a line by themselves.  Can't have "\section{A} more text"
  seclines <- grep("\\\\section\\*?\\{", tex)
  secs <- tex[seclines]
  ## case when mathsnap error
  if (isTRUE(head2_only)){
    secs[-1] <- stringr::str_replace_all(
      secs[-1], "\\\\section", "\\\\subsection")
    } 
  ## pipe to tex
  tex[seclines] <- secs
  ## search again
  seclines <- grep("\\\\section\\*?\\{", tex)
  ## add newline after by using custom function
  tex <- add_newline(          #
    x = tex,  grep_ptn = "\\\\section\\*?\\{",
    paired = "none",
    pos = "after")
  ## search again
  seclines <- grep("\\\\section\\*?\\{", tex)
  ## get secs 
  secs <- tex[seclines]
  ## subtitude
  secs <- sub("\\\\section\\*?\\{","",secs)
  secs <- sub("\\}$","", secs)  # add newline
  tex[seclines] <- paste("#", secs)


  # ---- SubSections
  # Subsections must be on a line by themselves.
  seclines <- grep("\\\\subsection\\*?\\{", tex)
  ## add newline after by using custom function
  tex <- add_newline(          #
    x = tex,  grep_ptn = "\\\\subsection\\*?\\{",
    paired = "none",
    pos = "after")
  ## search again
  seclines <- grep("\\\\subsection\\*?\\{", tex)
  ## get secs
  secs <- tex[seclines]
  ## subtitude
  secs <- sub("\\\\subsection\\*?\\{","",secs)
  secs <- sub("\\}$","", secs) # add newline
  ## clean subsection number "$9.2$ " dollar with blank space
  secs <- stringr::str_replace_all(secs, "(\\$\\d{1,2}\\.\\d{1,2}\\$\\s+)","") 
  ## clean subsection number "9 " with blank space
  secs <- stringr::str_replace_all(secs, "(^[\\d]\\s+)","") 
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
  
  # ---- double dollar for math equation
  ## we assumed that must exactly paired 
  ## with start "$$" and end "$$"
  ddolarlines <- grep("\\$\\$", tex)
  ## add newline before "$$" by using custom function
  tex <- add_newline(          #
    x = tex,  grep_ptn = "\\$\\$",
    paired = "start",
    pos = "before")
  ## add newline after "$$" by using custom function
  tex <- add_newline(          #
    x = tex,  grep_ptn = "\\$\\$",
    paired = "end",
    pos = "after")

  # ---- Process tabulars
  tex <- processTabulars(tex,tabInbed = FALSE)

  # ---- Process includegraphics
  graphlines <- grep("\\includegraphics", tex)
  graphs <- tex[graphlines]
  # remove any graph pars, such as [max width=\textwidth]
  graphs <- stringr::str_replace_all( 
    graphs, "(?<=includegraphics)(.+)(?=\\{)", "") 
  # correct image path
  graphs <- stringr::str_replace_all( 
    graphs, "\\{", paste0("\\{",dir_img,"/") ) 
  # fix image extension
  graphs <- stringr::str_replace_all( 
    graphs, "\\}", paste0(ext_img,"\\)" )) 
  # fix markdown format
  graphs <- stringr::str_replace_all( 
    graphs, "\\\\includegraphics\\{", "!\\[\\]\\(")
  tex[graphlines] <- graphs
  
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
              if (ext_out == ".qmd"){
                "format: html"
              } else {
                "output: html_document"
              },
              "---")
  # whether use the yml header
  if (isTRUE(keep_yml)){
    tex <- c(header, tex)
  } 

  # Make outfile name
  outfile <- paste0(sub("\\..+$","",infile), ext_out)

  # write out all modified text
  writeLines(tex, outfile)

  invisible(tex)
}

