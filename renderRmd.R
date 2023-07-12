#set default/starting directory here if desired
wkDir <- "C:/Users/benja/Documents/Postdoc/Data/ephys/"

#select output directory for all files
outDir <- jchoose.dir(default = wkDir, "Select output directory")

#get experiment name from user
expNameDlg <- dlg_input(message = "Enter experiment name")

rmarkdown::render(input = "R-DSE_markdown.Rmd",
                  output_file = paste0(outDir,"/", expNameDlg$res, "_report", ".html"),
                  output_format = "html_document",
                  params = list(workDir = wkDir,
                                outputDir = outDir, 
                                expTitle = expNameDlg$res))


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

