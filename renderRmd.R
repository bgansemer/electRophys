renderRmd <- function(wkDir = "C:/Users/") { 
  #' This is a helper script to render the Rmd file.
  #' @description This is a helper function to render the Rmd file that generates
  #' the final report. Using this function ensures that the report and all output
  #' files get saved to the correct location and with the correct names.
  #'
  #' @param wkDir string of working directory to specify where file dialog
  #' starts. Can be changed by user if desired.
  #' @param outDir string of the full path of the output directory. Retrieved as 
  #' input from user using file dialog. Set within script.
  #' @param expName string of the experiment name. This will be the common prefix
  #' for all output files. Retrieved as input from user. Set within script.


library(rJava)
library(rChoiceDialogs)
library(svDialogs)

#set default/starting directory here if desired
#wkDir <- "C:/Users/benja/Documents/Postdoc/Data/ephys/"
wkDir <- "C:/Users/"

#select output directory for all files
outDir <- jchoose.dir(default = wkDir, "Select output directory")

#get experiment name from user
expNameDlg <- dlg_input(message = "Enter experiment name")
expName <- expNameDlg$res

rmarkdown::render(input = "R-DSE_markdown.Rmd",
                  output_file = paste0(outDir,"/", expNameDlg$res, "_report", ".html"),
                  output_format = "html_document",
                  params = list(workDir = wkDir,
                                outputDir = outDir, 
                                expTitle = expName))

}
