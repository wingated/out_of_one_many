#####Creating data file for Study 1####
#Last updated: 26 October 2022


##Packages:
library(plyr)
library(tidyverse)
library(here)
library(rstudioapi)

##To set path for data loading:
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

####Load data for merging and cleaning:
#Load the background data on the original creators of the text/GPT-3 conditioning
background=read.csv(here::here(thisPath(),"Study1_Data","Background_data_to_merge.csv"))

#Load in the data from coding survey (hosted on Qualtrics)
dataset = here::here(thisPath(),"Study1_Data","Lucid_Coding_4Feb.csv")
col_names <- names(read_csv(dataset, n_max = 0, show_col_types = FALSE))
qualtrics <- read_csv(dataset, col_names = col_names, skip = 3, show_col_types = FALSE)
rm(col_names,dataset)
qualtrics <- as.data.frame(qualtrics) #To make this a normal df, not a tibble


#We have 16 ID variables in the coding data; these each include a prefix of "GPT-3_" or "Human_"
#Let's make a new column in the background dataset to correspond to each

background$ID_GPT3=paste("GPT-3_", background$ID, sep="")
background$ID_Human=paste("Human_", background$ID, sep="")
background$ID=NULL

gpt3=background
gpt3$ID_Human=NULL
gpt3$ID=gpt3$ID_GPT3
gpt3$ID_GPT3=NULL

human=background
human$ID_GPT3=NULL
human$ID=human$ID_Human
human$ID_Human=NULL

background=rbind(human, gpt3)

#Now  merge the data, one by one for each of the ID columns (we need to put in the background data for each)
combined=merge(qualtrics, background, by.x="ID.1", by.y=c("ID"), all.x=T)
combined=plyr::rename(combined, replace=c("Inc" ="Inc.1", "Hisp"="Hisp.1", "Age"="Age.1",
                                   "WHITE"="White.1", "Gender"="Gender.1"))
#Let's do it for all of the texts (there are 16)
combined=merge(combined, background, by.x="ID.2", by.y=c("ID"), all.x=T)
combined=plyr::rename(combined, replace=c("Inc" ="Inc.2", "Hisp"="Hisp.2", "Age"="Age.2",
                                    "WHITE"="White.2", "Gender"="Gender.2"))
combined=merge(combined, background, by.x="ID.3", by.y=c("ID"), all.x=T)
combined=plyr::rename(combined, replace=c("Inc" ="Inc.3", "Hisp"="Hisp.3", "Age"="Age.3",
                                    "WHITE"="White.3", "Gender"="Gender.3"))
combined=merge(combined, background, by.x="ID.4", by.y=c("ID"), all.x=T)
combined=plyr::rename(combined, replace=c("Inc" ="Inc.4", "Hisp"="Hisp.4", "Age"="Age.4",
                                    "WHITE"="White.4", "Gender"="Gender.4"))
combined=merge(combined, background, by.x="ID.5", by.y=c("ID"), all.x=T)
combined=plyr::rename(combined, replace=c("Inc" ="Inc.5", "Hisp"="Hisp.5", "Age"="Age.5",
                                    "WHITE"="White.5", "Gender"="Gender.5"))
combined=merge(combined, background, by.x="ID.6", by.y=c("ID"), all.x=T)
combined=plyr::rename(combined, replace=c("Inc" ="Inc.6", "Hisp"="Hisp.6", "Age"="Age.6",
                                    "WHITE"="White.6", "Gender"="Gender.6"))
combined=merge(combined, background, by.x="ID.7", by.y=c("ID"), all.x=T)
combined=plyr::rename(combined, replace=c("Inc" ="Inc.7", "Hisp"="Hisp.7", "Age"="Age.7",
                                    "WHITE"="White.7", "Gender"="Gender.7"))
combined=merge(combined, background, by.x="ID.8", by.y=c("ID"), all.x=T)
combined=plyr::rename(combined, replace=c("Inc" ="Inc.8", "Hisp"="Hisp.8", "Age"="Age.8",
                                    "WHITE"="White.8", "Gender"="Gender.8"))
combined=merge(combined, background, by.x="ID.9", by.y=c("ID"), all.x=T)
combined=plyr::rename(combined, replace=c("Inc" ="Inc.9", "Hisp"="Hisp.9", "Age"="Age.9",
                                    "WHITE"="White.9", "Gender"="Gender.9"))
combined=merge(combined, background, by.x="ID.10", by.y=c("ID"), all.x=T)
combined=plyr::rename(combined, replace=c("Inc" ="Inc.10", "Hisp"="Hisp.10", "Age"="Age.10",
                                    "WHITE"="White.10", "Gender"="Gender.10"))
combined=merge(combined, background, by.x="ID.11", by.y=c("ID"), all.x=T)
combined=plyr::rename(combined, replace=c("Inc" ="Inc.11", "Hisp"="Hisp.11", "Age"="Age.11",
                                    "WHITE"="White.11", "Gender"="Gender.11"))
combined=merge(combined, background, by.x="ID.12", by.y=c("ID"), all.x=T)
combined=plyr::rename(combined, replace=c("Inc" ="Inc.12", "Hisp"="Hisp.12", "Age"="Age.12",
                                    "WHITE"="White.12", "Gender"="Gender.12"))
combined=merge(combined, background, by.x="ID.13", by.y=c("ID"), all.x=T)
combined=plyr::rename(combined, replace=c("Inc" ="Inc.13", "Hisp"="Hisp.13", "Age"="Age.13",
                                    "WHITE"="White.13", "Gender"="Gender.13"))
combined=merge(combined, background, by.x="ID.14", by.y=c("ID"), all.x=T)
combined=plyr::rename(combined, replace=c("Inc" ="Inc.14", "Hisp"="Hisp.14", "Age"="Age.14",
                                    "WHITE"="White.14", "Gender"="Gender.14"))
combined=merge(combined, background, by.x="ID.15", by.y=c("ID"), all.x=T)
combined=plyr::rename(combined, replace=c("Inc" ="Inc.15", "Hisp"="Hisp.15", "Age"="Age.15",
                                    "WHITE"="White.15", "Gender"="Gender.15"))
combined=merge(combined, background, by.x="ID.16", by.y=c("ID"), all.x=T)
combined=plyr::rename(combined, replace=c("Inc" ="Inc.16", "Hisp"="Hisp.16", "Age"="Age.16",
                                    "WHITE"="White.16", "Gender"="Gender.16"))

names(combined)

#Export the file to a .csv for the analysis file for study 1
write.csv(combined, here::here(thisPath(),"Study1_Data","Lucid_FullData_4Feb_with_background.csv"), row.names = F)
