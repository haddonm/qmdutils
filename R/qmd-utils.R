
#' @title codeblock outlines a standardized code block 
#'
#' @return sends some code to the console to be copied to the source
#' @export
#'
#' @examples
#' codeblock()
codeblock <- function() {
  cat("```{r } \n")
  cat("#| label: lst-code \n")
  cat("#| echo: false \n")
  cat("#| warning: false \n")
  cat("     \n")
  cat("``` \n")
} # end of codeblock

#' @title commoncode generates initial code chunk at start of each chapter
#' 
#' @description commoncode solves the problem of having the same R environment 
#'     operating in each chapter. It generates an initial code chunk to be
#'     placd at the start of each chapter.
#'
#' @return nothing but it writes R syntax to the console ready to be copied to
#'     the source window 
#' @export
#'
#' @examples
#' commoncode()
commoncode <- function() {
  cat("```{r} \n")
  cat("#| echo: false \n")
  cat('source("_common.R") \n')
  cat("```  \n")
} # end of commoncode

#' @title figuresetup generates Quarto syntax to generate a new figure
#' 
#' @description figuresetup generates Quarto syntax to generate a new table
#'
#' @return nothing but it writes R syntax to the console ready to be copied to
#'     the source window 
#' @export
#'
#' @examples
#'  figuresetup()
figuresetup <- function() {
    cat("```{r} \n")
    cat("#| echo: false \n")  
    cat("#| label: fig-name \n")
    cat("#| warning: false  \n")
    cat("#| message: false  \n")    
    cat("#| fig-cap: caption \n")
    cat("#| fig-alt: alttext \n")
    cat("# #| out-width: 80% \n") 
    cat("#| fig.width: 5) \n")
    cat("#| fig.height: 3) \n")
    cat("  plotcode \n")    
    cat("``` \n")
} # end of figuresetup

#' @title figureimport generates Quarto code to import a figure from a file
#' 
#' @description figureimport generates Quarto to import a figure from a file
#'
#' @return nothing but it writes R syntax to the console ready to be copied to
#'     the source window 
#' @export
#'
#' @examples
#' figureimport()
figureimport <- function() {
  cat("```{r} \n")
  cat("#| label: fig-name \n")
  cat("#| echo: false \n")
  cat("#| fig-cap: caption  \n")
  cat("#| fig-alt: alttext \n")
  cat("#| out-width: 100% \n")
  cat("# #| fig.width: 5) \n")
  cat("# #| fig.height: 3) \n")
  cat('filen <- pathtopath(prefixdir,"/figures/filename.png")  \n')
  cat("knitr::include_graphics(filen,dpi=270) \n")
  cat("``` \n")
} # end of figureimport

#' @title makeQuarto generates a template for a quarto manuscript
#' 
#' @description makeQuarto generates a template for a quarto based manuscript
#'     that can be any or all of html, pdf, of docx. If using docx it is 
#'     recommended that the word macros found at 
#'     https//:github.com/haddonm/quarto_macros, although these are still being
#'     developed.
#'
#' @param rundir the directory in which the quarto files are to be held. It 
#'     might also hold the wordreference file.
#' @param filename what name for the qmd file, default='manuscript.qmd'
#' @param title what title for the manuscript, default = 'A Title'
#' @param author who is the first author, default = 'Malcolm Haddon'
#' @param affiliation the institutional affiliation, default = 
#'     'IMAS, University of Tasmania'
#' @param department the department within teh institution, default = IMAS-FA
#' @param city the city for the Institution, default = Hobart
#' @param DOCX should the option of a docx be included, default = TRUE
#' @param PDF should the option of a pdf be included, default = TRUE
#' @param HTML should the option of an html document be included, 
#'     default = TRUE. If TRUE then a custom.scss file is also generated into
#'     the rundir
#' @param docref the word document which is used as a reference if a docx
#'     document is to be generated. It is recommended that this be placed 
#'     within the rundir. If this is not present in the rundir then the full 
#'     path and filename is required. 
#'
#' @return nothing but it does generate one or two files into rundir
#' @export
#'
#' @examples
#' require(codeutils)
#' usedir <- tempdir()
#' makeQuarto(usedir,title="An Ideal World")
#' txt <- readLines(pathtopath(usedir,"manuscript.qmd"))
#' nline <- length(txt)
#' for (i in 1:nline) print(txt[i],quote=FALSE)
makeQuarto <- function(rundir,filename="manuscript.qmd",title="A Title",
                       author="Malcolm Haddon",
                       affiliation="IMAS, University of Tasmania",
                       department="IMAS-FA", city="Hobart",
                       DOCX=TRUE,PDF=TRUE,HTML=TRUE,
                       docref="wordreferencestyle.docx") {
  filen <- pathtopath(rundir,filename)
  # start generating file contents: 
  cat("--- \n",file=filen,append=FALSE)
  cat("title: ",title,"\n",file=filen,append=TRUE)
  cat("\n",file=filen,append=TRUE)
  cat("author: \n",file=filen,append=TRUE)
  cat("  - name: ",author,"\n",file=filen,append=TRUE)
  cat("    affil-id: 1","\n",file=filen,append=TRUE)
  cat("\n",file=filen,append=TRUE)
  cat("    affiliation: \n",file=filen,append=TRUE)
  cat("     -  id: 1")
  cat("        name: ",affiliation,"\n",file=filen,append=TRUE)
  cat("        department: ",department,"\n",file=filen,append=TRUE)
  cat("        city: ",city,"\n",file=filen,append=TRUE)  
  cat("\n",file=filen,append=TRUE)
  cat("date: last-modified \n",file=filen,append=TRUE)
  cat("date-format: '[Updated on] DD MMMM YYYY' \n",file=filen,append=TRUE)
  cat("page-navigation: true \n",file=filen,append=TRUE)
  cat("\n",file=filen,append=TRUE)
  cat("format: \n",file=filen,append=TRUE)
  if (DOCX) {
    cat("  docx: \n",file=filen,append=TRUE)
    cat("    crossref: \n",file=filen,append=TRUE)
    cat("      chapters: true \n",file=filen,append=TRUE)
    cat("    highlight-style: github \n",file=filen,append=TRUE)
    cat("    papersize: A4 \n",file=filen,append=TRUE)
    cat("    code-overflow: 'wrap' \n",file=filen,append=TRUE)
    cat("    reference-doc: ",docref,"\n",file=filen,append=TRUE)
    cat("    toc: true \n",file=filen,append=TRUE)
    cat("    number-sections: true \n",file=filen,append=TRUE)
    cat("    toc-depth: 3 \n",file=filen,append=TRUE)
    cat("    number-depth: 3 \n",file=filen,append=TRUE)
    cat("    margin-left: 0.75in \n",file=filen,append=TRUE)
    cat("    margin-right: 0.75in \n",file=filen,append=TRUE)
    cat("    margin-top: 1in \n",file=filen,append=TRUE)
    cat("    margin-bottom: 1in \n",file=filen,append=TRUE)
  }
  if (PDF) {
    cat("  pdf: \n",file=filen,append=TRUE)
    cat("    documentclass: scrreport \n",file=filen,append=TRUE)
    cat("    keep-tex: true \n",file=filen,append=TRUE)
    cat("    dpi: 600 \n",file=filen,append=TRUE)
    cat("    #pdf-engine: pdflatex \n",file=filen,append=TRUE)
    cat("    toc: true \n",file=filen,append=TRUE)
    cat("    toc-depth: 3 \n",file=filen,append=TRUE)
    cat("    toc-float: true \n",file=filen,append=TRUE)
    cat("    number-sections: true \n",file=filen,append=TRUE)
    cat("    number-depth: 3 \n",file=filen,append=TRUE)
    cat("    crossref: \n",file=filen,append=TRUE)
    cat("      chapters: true \n",file=filen,append=TRUE)
    cat("    highlight-style: github \n",file=filen,append=TRUE)
    cat("    papersize: A4paper \n",file=filen,append=TRUE)
    cat("    geometry: \n",file=filen,append=TRUE)
    cat("      - left = 19mm \n",file=filen,append=TRUE)
    cat("      - right = 19mm \n",file=filen,append=TRUE)
    cat("      - top = 25mm \n",file=filen,append=TRUE)
    cat("      - bottom = 25mm \n",file=filen,append=TRUE)
  }
  if (HTML) {
    cat("  html: \n",file=filen,append=TRUE)
    cat("    theme: \n",file=filen,append=TRUE)
    cat("      - cosmo \n",file=filen,append=TRUE)
    cat("      - custom.scss \n",file=filen,append=TRUE)
    cat("    code-copy: true \n",file=filen,append=TRUE)
  }
  cat("--- \n\n\n",file=filen,append=TRUE) # end of yaml header
  cat("```{r} \n",file=filen,append=TRUE)
  cat("#| label: setup-main \n",file=filen,append=TRUE)
  cat("#| echo: false \n",file=filen,append=TRUE)
  cat("#| warning: false \n\n",file=filen,append=TRUE)
  cat("knitr::opts_chunk$set( \n",file=filen,append=TRUE)
  cat("  echo = FALSE, \n",file=filen,append=TRUE)
  cat("  message = FALSE, \n",file=filen,append=TRUE)
  cat("  warning = FALSE) \n",file=filen,append=TRUE)
  cat("options(knitr.kable.NA = '', \n",file=filen,append=TRUE)
  cat("        knitr.table.format = 'pandoc') \n\n",file=filen,append=TRUE)
  cat("options(tinytex.verbose = TRUE) # change to suit \n\n",
      file=filen,append=TRUE)
  cat("suppressPackageStartupMessages({ \n",file=filen,append=TRUE)
  cat("  library(codeutils) \n",file=filen,append=TRUE)
  cat("  library(hplot) \n",file=filen,append=TRUE)
  cat("  library(knitr) \n",file=filen,append=TRUE)
  cat("  library(qmdutils) \n",file=filen,append=TRUE)
  cat("}) \n\n",file=filen,append=TRUE)  
  cat("ddir <- getDBdir()  \n",file=filen,append=TRUE)
  cat("``` \n",file=filen,append=TRUE)
  if (HTML) {
    # generate custom css file if using HTML
    cssfile <- pathtopath(rundir,"custom.scss")
    cat("/*-- scss:rules --*/ \n",file=cssfile,append=FALSE)
    cat("\n",file=cssfile,append=TRUE)
    cat("body { \n",file=cssfile,append=TRUE)
    cat("  font-size: 18px; \n",file=cssfile,append=TRUE)
    cat("  font-family: serif; \n",file=cssfile,append=TRUE)
    cat("  color: black; \n",file=cssfile,append=TRUE)
    cat("} \n",file=cssfile,append=TRUE)
    cat("\n",file=cssfile,append=TRUE)
    cat("figcaption { \n",file=cssfile,append=TRUE)
    cat("  font-size: 18px; \n",file=cssfile,append=TRUE)
    cat("  font-family: serif; \n",file=cssfile,append=TRUE)
    cat("  color: black; \n",file=cssfile,append=TRUE)
    cat("} \n",file=cssfile,append=TRUE)
    cat("\n",file=cssfile,append=TRUE)
    cat("figure { \n",file=cssfile,append=TRUE)
    cat("  font-size: 18px; \n",file=cssfile,append=TRUE)
    cat("  font-family: serif; \n",file=cssfile,append=TRUE)
    cat("  color: black; \n",file=cssfile,append=TRUE)
    cat("} \n",file=cssfile,append=TRUE)    
  }
} # end of makeQuarto


#' @title qmdhelp prints a list of qmdutils to the console
#'
#' @return nothing but does print a list of qmdutils to the console
#' @export
#'
#' @examples
#' qmdhelp()
qmdhelp <- function() {
  cat("codeblock()  \n")
  cat("commoncode()   \n")
  cat("figuresetup()  \n")
  cat("figureimport() \n")
  cat("qmdhelp() \n")
  cat("tablesetup()   \n")
  cat("makeQuarto()  \n")
} # end of qmdhelp

#' @title tablesetup outlines a standardized table block 
#'
#' @return sends some code to the console to be copied to the source
#' @export
#'
#' @examples
#' tablesetup()
tablesetup <- function() {
  cat("```{r } \n")
  cat("#| label: tbl-text \n") 
  cat("#| echo: false \n")
  cat("#| warning: false  \n")
  cat("#| tbl-cap:    \n")
  cat("# #| tbl-colwidths: [60,40] \n")  
  cat("# kable(x, digits=c(3,3,3))   \n")
  cat("``` \n")
} # end of tablesetup































