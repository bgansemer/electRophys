workDir <- "C:/Users/benja/Documents/Postdoc/Data/ephys/"
resultsDir <- "Pic4AP-DSE/20230215-pic4ap-3h/results/"
outDir <- paste0(workDir, resultsDir)
#outDir <- "C:/Users/benja/Desktop/temp/"
expName <- "230215-DSE-pic4ap-3h"

rmarkdown::render(input = "R-DSE_markdown.Rmd",
                  output_file = paste0(outDir, expName, "_report", ".html"),
                  output_format = "html_document")


#' renderRmd <- function() {
#' 
#'     
#'   #' Helper function to render the Rmd file.
#'   #' @description This is a helper function to render the Rmd file that generates
#'   #' the final report. Using this function ensures that the report and all output 
#'   #' files get saved to the correct location and with the correct names.
#'   #' 
#'   #' @param outDir string of the full path of the output directory. It should 
#'   #' contain the final '/'.
#'   #' @param expName string of the experiment name. This will be the common prefix 
#'   #' for all output files.
#'   #' 
#'   
#'   #outputDir <- "C:/Users/benja/Documents/Postdoc/Data/ephys/Pic4AP-DSE/20230214-Pic4ap-3h/results/"
#'   #expName <- "230214-DSE-pic4ap-3h"
#' 
#'   rmarkdown::render(input = "R-DSE_markdown.Rmd", 
#'                   output_file = paste0(outDir, expName, "_report", ".html"),
#'                   output_format = "html_document")
#'   
#'}
#'

