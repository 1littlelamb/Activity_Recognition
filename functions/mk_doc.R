# Function for compiling the .Rnw files so that they might include the bibliography

# Code was found on stack exchange and is a band-aid fix to some issues RStudio has when compliling
# a Latex + Bibtex document. Thank you u/conjugateprior

# https://tex.stackexchange.com/questions/71565/knitr-and-biblatex

mkdoc <- function(fname){ require(knitr); res <- knit(fname); if (!is.null(res)) { system2('latexmk', args=c('-xelatex', res)) } }
