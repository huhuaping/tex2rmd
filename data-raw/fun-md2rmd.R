
# 
renv::install("tmcd82070/tex2rmd")
require("tex2rmd")
require(here)

input_md <- ("data-raw/chpt09-hypothesit-test/chpt09-hypothesit-test.tex"
)
dir.img <- "data-raw/images/"
tex2rmd(infile = input_md, 
        ext_out = ".qmd",
        dir_img = dir.img, 
        ext_img = ".jpg", 
        head2_only = TRUE, keep_yml =FALSE)

# ---- Process Figures
tex <- processFigures(x = tex)









tex_sec <- add_newline(x = tex, lines = seclines, pos = "after")

head(tex_sec)

tex_add <- tex
seclines <- grep("\\$\\$", tex_add)


i <- 1
for (i in 1:length(seclines)) {
  
  line_tar <- seclines[i]
  tot <- length(tex_add)
  tex_add <- c(tex_add[1:line_tar],"",
               tex_add[(line_tar+1):tot])
  # search again
  seclines <- grep("\\\\subsection\\*?\\{", tex_add)

  # tex_add[seclines]
}


