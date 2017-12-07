# tex2rmd

An R package for converting LaTex files into RMarkdown files.

From there, one can edit the markdown document and compile to PDF or Word or whatever. 

## Features

* Strips all the header information (between \\documentclass and \\begin{document})
* Removes all comments, but preserves all legitimate % signs (i.e., perserves \\%)
* Converts all \\texttt, \\textit, and \\textbf to the markdown equivalent. 
* Converts \\section, \\subsection, and \\subsubsection to the markdown equivalent
* If a .aux and .bbl file are present in the same directory, `tex2rmd` works out all 
the citations and inserts them.  They are not live references anymore, they are text.  Re-run Latex (to get new .aux and .bbl files) and re-run tex2rmd if references change. 
* All tables in \\tabular environments are converted to markdown equivalents. 
Some functionality is lost here.  For example, multiline headers are not allowed (by markdown). Column spanning is not allowed (by markdown). 

## What is left

* Figures
* Cross references to Tables and Figures (i.e. work out the \\ref and \\label mapping)

## How to use

1. Compile your latex file, perserve .aux and .bbl if present
2. Fire up R and run `tex2rmd(myLatex.tex)`
3. Edit the generated `myLatex.Rmd`
4. Compile the markdown document to whatever you like. 


