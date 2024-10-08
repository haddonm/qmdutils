
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
    cat("```{r } \n")
    cat("#| echo: false \n")  
    cat("#| label: fig-name \n")
    cat("#| warning: false  \n")
    cat("#| message: false  \n")    
    cat("#| fig-cap: caption \n")
    cat("#| fig-alt: alttext \n")
    cat("#| out-width: 80% \n") 
    cat("# #| fig.width: 5) \n")
    cat("# #| fig.height: 3) \n")
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
  cat("```{r } \n")
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































