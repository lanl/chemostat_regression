#!/usr/bin/env Rscript

# ______________________________ ------------------------------------------
# Functions ---------------------------------------------------------------

identify_and_set_wd_cli <- function(n_back=0) {
  
  initial.options <- commandArgs(trailingOnly=FALSE)
  file.arg.name <- "--file="
  vPath_current <- sub(file.arg.name, "", initial.options[grep(file.arg.name, initial.options)]) ## Identify the directory of the current R-Studio script
  vPath_base    <- Reduce(file.path, vPath_current[1:length(vPath_current)-n_back])       ## Define the directory to be set
  setwd(system("pwd", intern=T) )
  return(vPath_base)                                                                      ## Return string of working directory path
}



# ______________________________ ------------------------------------------
# Shiny -------------------------------------------------------------------

library(shiny)
library(bslib)

## Initialize Environment
identify_and_set_wd_cli()

runApp("chemostat_regression_Rshiny", launch.browser=T)
