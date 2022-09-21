
# 
renv::install("tmcd82070/tex2rmd")
require("tex2rmd")
require(here)

input_md <- "data-raw/chpt13-gmm.tex"

dir.img <- "images/"
tex2rmd(infile = input_md, 
        ext_out = ".qmd",
        dir_img = dir.img, 
        ext_img = ".jpg", 
        head2_only = TRUE, 
        keep_yml = TRUE)

# ---- Process Figures
processTabulars(x = tex,tabInbed = FALSE )
 
protbl <-x 

b1 <- beginTable[1]
e1 <- endTable[1]
processOneTable(x = protbl,
                begin = b1, 
                end = e1, 
                tabNum = tabNum)

tex_eq <- tex

eqbegin<- max(which(str_detect(tex_eq, "begin\\{aligned")))
eqend<- max(which(str_detect(tex_eq, "end\\{aligned")))

tex_eq[eqbegin:eqend]



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


