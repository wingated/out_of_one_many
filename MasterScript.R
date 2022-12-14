##Master File
#Last updated: 17 November 2022

##This script enables users who already have R (we're running version 4.2.1 in RStudio version 2022.07.0) and Python (we're using version 3.11) on their machines to run all the R and Python scripts associated with this project. 

##To run the Python scripts sourced below, users will first need to install a few additional packages described in the "Master_ReadMe.txt" file as well: seaborn, matplotlib, numpy, pandas, tqdm and sklearn. We used pip for this:

# pip install seaborn matplotlib numpy pandas tqdm sklearn #

#For a list of the versions of each of these python packages used by the authors, see the "Master_ReadMe.txt" file.

########################
##To load packages necessary to run this script:
library(rstudioapi)
library(here)
library(reticulate)

########################
##A function to set the working directory to the location of this master script regardless on the user's personal computer, enabling the running of all other R and Python files in the replication folder without changing paths:

stub <- function() {}
thisPath <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  if (length(grep("^-f$", cmdArgs)) > 0) {
    # R console option
    normalizePath(dirname(cmdArgs[grep("^-f", cmdArgs) + 1]))[1]
  } else if (length(grep("^--file=", cmdArgs)) > 0) {
    # Rscript/R console option
    scriptPath <- normalizePath(dirname(sub("^--file=", "", cmdArgs[grep("^--file=", cmdArgs)])))[1]
  } else if (Sys.getenv("RSTUDIO") == "1") {
    if (rstudioapi::isAvailable(version_needed=NULL,child_ok=FALSE)) {
      # RStudio interactive
      dirname(rstudioapi::getActiveDocumentContext()$path)
    } else if (is.null(knitr::current_input(dir = TRUE)) == FALSE) {
      # Knit
      knitr::current_input(dir = TRUE)
    } else {
      # R markdown on RStudio
      getwd()
    }
  } else if (is.null(attr(stub, "srcref")) == FALSE) {
    # 'source'd via R console
    dirname(normalizePath(attr(attr(stub, "srcref"), "srcfile")$filename))
  } else {
    stop("Cannot find file path")
  }
}

setwd(here(thisPath()))

#####################
#####To install all R packages necessary for analysis across all R scripts for this project, uncomment the following command. For a list of these packages and the versions of each used by authors for analysis, see the "Master_ReadMe.txt" file.
#source("InstallRPackages.R")



#######################
##To calculate script run times on your machine, uncomment these lines:

#system.time(source("Study1DataGen.R")) #2.319 seconds
#system.time(source("Study1Analysis.R")) #6.022 seconds
#system.time(reticulate::source_python("Study1Python.py",envir=NULL,convert=F)) #1.724 seconds
#system.time(source("Study2Analysis.R")) #5.935 seconds
#system.time(reticulate::source_python("Study2Python.py",envir=NULL,convert=F)) #15.69 seconds
#system.time(source("Study3Analysis.R")) #7.150 seconds

#The run times listed here are from a 2018 15" MacBook Pro with a 2.9 GHz 6-Core Intel Core i9 processor and 16 GB of memory


#######################
##The following scripts will automatically generate raw versions of all the tables and figures presented in the paper itself and in the appendix, with the exception of the tables for Study 2. The R script "Study2Analysis.R" generates all the numbers for these tables, which we then inputted by hand into the tables presented in both the main paper and appendix. 

#We post-processed all tables and figures in the paper and appendix, making formatting changes to the tables in LaTeX, and using Adobe Illustrator to improve the figures. Thus, while the code here was used to initially generate the tables and figures, it does not produce tables and figures as clean as the final products presented in the paper and appendix.

#Study 1:
source("Study1DataGen.R")
source("Study1Analysis.R")
reticulate::source_python("Study1Python.py",envir=NULL,convert=F)

##Study 2:
source("Study2Analysis.R") 
reticulate::source_python("Study2Python.py",envir=NULL,convert=F)

##Study 3: 
source("Study3Analysis.R")

###Figures and tables generated by this code can be found in the "Figures" and "Tables" folders in our replication files. The "AllTables.tex" and "AllTables.pdf" documents in the "Tables" folder combine all tables into one document.

