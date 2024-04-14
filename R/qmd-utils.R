

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
    cat("#| label: fig-name \n")
    cat("#| echo: false \n")
    cat("#| fig-cap: | \n")
    cat("#|   caption \n") 
    cat("#| fig-alt: | \n")
    cat("#|   alt-text \n")
    cat("#| out-width: 80% \n")  
    cat("#| comment: description  \n")
    cat("#| echo: false  \n")
    cat("#| warning: false  \n)")
    cat("#| message: false  \n")
    cat("  plotcode \n")    
    cat("``` \n")
} # end of figuresetup

#' @title importfigure generates Quarto code to import a figure from a file
#' 
#' @description generates Quarto to import a figure from a file
#'
#' @return nothing but it writes R syntax to the console ready to be copied to
#'     the source window 
#' @export
#'
#' @examples
#' importfigure()
importfigure <- function() {
  cat("```{r } \n")
  cat("#| label: fig-name \n")
  cat("#| echo: false \n")
  cat("#| fig-cap: |  \n")
  cat("#|   caption \n")
  cat("#| fig-alt: | \n")
  cat("#|   alt-text \n")
  cat("#| out-width: 100% \n")
  cat('filen <- pathtopath(prefixdir,"/figures/filename.png")  \n')
  cat("knitr::include_graphics(filen,dpi=270) \n")
  cat("``` \n")
} # end of importfigure







































