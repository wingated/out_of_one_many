#####Study 2: ANES vote data from GPT-3 and the ANES#####
##Last updated: 26 October 2022

####Libraries and working directories####
library(plyr)
library(psych)
library(correlation)
library(here)
library(rstudioapi)
options(scipen=9)

###To set the path for loading the data:

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

#Now to import the data; the following files contain the survey data from the ANES (with original variable names from the ANES) along with corresponding predictions from GPT-3 for each respondent

####2012####
anes2012=read.csv(here::here(thisPath(),"Study2_Data","full_results_2012_2.csv"))
names(anes2012)

#Give intuitive names
anes2012=plyr::rename(anes2012, replace=c("presvote2012_x" = "anes2012_vote", "dem_raceeth_x"="race", 
                            "discuss_disc"="discuss_politics", "libcpre_self"="ideology",
                            "pid_x"="party_id", "relig_church"="attend_church",
                            "dem_age_r_x"="age", "gender_respondent_x"="gender", "paprofile_interestpolit"="interest",
                            "patriot_flag"="flag", "sample_stfips"="state"))

#Create a binary vote for the anes2012
anes2012$vote_romney[anes2012$anes2012_vote==2]=1
anes2012$vote_romney[anes2012$anes2012_vote==1]=0

#We can also turn GPT-3's predictions into something binary
anes2012$gpt3_vote[anes2012$p_romney>0.5]=1
anes2012$gpt3_vote[anes2012$p_romney<0.5]=0

#Make all negative values on the anes2012 variables missing
anes2012$race[anes2012$race<0]=NA
anes2012$discuss_politics[anes2012$discuss_politics<0]=NA
anes2012$ideology[anes2012$ideology<0]=NA
anes2012$party_id[anes2012$party_id<0]=NA
anes2012$attend_church[anes2012$attend_church<0]=NA
anes2012$age[anes2012$age<0]=NA
anes2012$gender[anes2012$gender<0]=NA
anes2012$interest[anes2012$interest<0]=NA
anes2012$flag[anes2012$flag<0]=NA
anes2012$state[anes2012$state<0]=NA

####Let's restrict just to those for whom we have voting data
anes20122=anes2012[!is.na(anes2012$vote_romney),]


####2016####
anes=read.csv(here::here(thisPath(),"Study2_Data","full_results_2016_2.csv"))
names(anes)

#Give intuitive names
anes=plyr::rename(anes, replace=c("V162062x" = "anes_vote", "V161310x"="race", 
                            "V162174"="discuss_politics", "V161126"="ideology",
                            "V161158x"="party_id", "V161244"="attend_church",
                            "V161267"="age", "V161342"="gender", "V162256"="interest",
                            "V162125x"="flag", "V161010d"="state"))

#Create a binary vote for trump or clinton version for the ANES
anes$vote_trump[anes$anes_vote==2]=1
anes$vote_trump[anes$anes_vote==1]=0
mean(anes$p_trump[!is.na(anes$vote_trump)], na.rm=T)

#We can also turn GPT-3's predictions into something binary
anes$gpt3_vote[anes$p_trump>0.5]=1
anes$gpt3_vote[anes$p_trump<0.5]=0

#Make all negative values on the anes variables missing
anes$race[anes$race<0]=NA
anes$discuss_politics[anes$discuss_politics<0]=NA
anes$ideology[anes$ideology<0]=NA
anes$party_id[anes$party_id<0]=NA
anes$attend_church[anes$attend_church<0]=NA
anes$age[anes$age<0]=NA
anes$gender[anes$gender<0]=NA
anes$interest[anes$interest<0]=NA
anes$flag[anes$flag<0]=NA
anes$state[anes$state<0]=NA

##Restrict just to those for whom we have voting data
anes2=anes[!is.na(anes$vote_trump),]

####2020####
anes2020=read.csv(here::here(thisPath(),"Study2_Data","full_results_2020_2.csv"))
names(anes2020)

#Give intuitive names
anes2020=plyr::rename(anes2020, replace=c("V202110x" = "anes2020_vote", "V201549x"="race", 
                                    "V202022"="discuss_politics", "V201200"="ideology",
                                    "V201231x"="party_id", "V201452"="attend_church",
                                    "V201507x"="age", "V201600"="gender", "V202406"="interest"))
#Create a binary vote for the anes2020
anes2020$vote_trump[anes2020$anes2020_vote==2]=1
anes2020$vote_trump[anes2020$anes2020_vote==1]=0

#We can also turn GPT-3's predictions into something binary
anes2020$gpt3_vote[anes2020$p_trump>0.5]=1
anes2020$gpt3_vote[anes2020$p_trump<0.5]=0

####Let's restrict just to those for whom we have voting data
anes20202=anes2020[!is.na(anes2020$vote_trump),]

####Results in the order presented in the text####
####Overall voting####
##2012
prop.test(x=c(mean(anes2012$vote_romney, na.rm=T)*nrow(anes2012[!is.na(anes2012$vote_romney),]), mean(anes2012$p_romney, na.rm=T)*nrow(anes2012[!is.na(anes2012$vote_romney),])), n=c(nrow(anes2012[!is.na(anes2012$vote_romney),]), nrow(anes2012[!is.na(anes2012$vote_romney),])))
##2016
prop.test(x=c(mean(anes$vote_trump, na.rm=T)*nrow(anes[!is.na(anes$vote_trump),]), mean(anes$p_trump[!is.na(anes$vote_trump)], na.rm=T)*nrow(anes[!is.na(anes$vote_trump),])), n=c(nrow(anes[!is.na(anes$vote_trump),]), nrow(anes[!is.na(anes$vote_trump),])))
##2020
prop.test(x=c(mean(anes2020$vote_trump, na.rm=T)*nrow(anes2020[!is.na(anes2020$vote_trump),]), mean(anes2020$p_trump[!is.na(anes2020$vote_trump)], na.rm=T)*nrow(anes2020[!is.na(anes2020$vote_trump),])), n=c(nrow(anes2020[!is.na(anes2020$vote_trump),]), nrow(anes2020[!is.na(anes2020$vote_trump),])))


#####2012 detailed results for Table 1####
#Whole sample
temp=anes20122
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Men
temp=anes20122[anes20122$gender==1,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Women
temp=anes20122[anes20122$gender==2,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Strong partisans
temp=anes20122[anes20122$party_id==7|anes20122$party_id==1,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Weak partisans
temp=anes20122[anes20122$party_id==6|anes20122$party_id==2,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Leaners
temp=anes20122[anes20122$party_id==5|anes20122$party_id==3,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Independents
temp=anes20122[anes20122$party_id==4,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Conservatives
temp=anes20122[anes20122$ideology>4,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Moderates
temp=anes20122[anes20122$ideology==4,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Liberals
temp=anes20122[anes20122$ideology<4,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Whites
temp=anes20122[anes20122$race==1,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Blacks
temp=anes20122[anes20122$race==2,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Hispanics
temp=anes20122[anes20122$race==5,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Attend church
temp=anes20122[anes20122$attend_church==1,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Doesn't attend church
temp=anes20122[anes20122$attend_church==2,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Interest in politics (high)
temp=anes20122[anes20122$interest==1,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Interest in politics (low)
temp=anes20122[anes20122$interest==4,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Discuss politics
temp=anes20122[anes20122$discuss_politics==1,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Discuss politics (not)
temp=anes20122[anes20122$discuss_politics==2,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Age (18-30)
temp=anes20122[anes20122$age>=18&anes20122$age<31,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Age (31-45)
temp=anes20122[anes20122$age>=31&anes20122$age<46,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Age (46-60)
temp=anes20122[anes20122$age>=46&anes20122$age<61,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Age (Over 60)
temp=anes20122[anes20122$age>=61,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

####2016 results for Table 1####
#Whole sample
temp=anes2
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Men
temp=anes2[anes2$gender==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Women
temp=anes2[anes2$gender==2,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Strong partisans
temp=anes2[anes2$party_id==7|anes2$party_id==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Weak partisans
temp=anes2[anes2$party_id==6|anes2$party_id==2,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Leaners
temp=anes2[anes2$party_id==5|anes2$party_id==3,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Independents
temp=anes2[anes2$party_id==4,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Conservatives
temp=anes2[anes2$ideology>4,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Moderates
temp=anes2[anes2$ideology==4,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Liberals
temp=anes2[anes2$ideology<4,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Whites
temp=anes2[anes2$race==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Blacks
temp=anes2[anes2$race==2,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Hispanics
temp=anes2[anes2$race==5,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Attend church
temp=anes2[anes2$attend_church==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Doesn't attend church
temp=anes2[anes2$attend_church==2,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Interest in politics (high)
temp=anes2[anes2$interest==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Interest in politics (low)
temp=anes2[anes2$interest==4,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Discuss politics
temp=anes2[anes2$discuss_politics==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Discuss politics (not)
temp=anes2[anes2$discuss_politics==2,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Age (18-30)
temp=anes2[anes2$age>=18&anes2$age<31,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Age (31-45)
temp=anes2[anes2$age>=31&anes2$age<46,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Age (46-60)
temp=anes2[anes2$age>=46&anes2$age<61,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Age (Over 60)
temp=anes2[anes2$age>=61,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

####2020 results for Table 1####
#Whole sample
temp=anes20202
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Men
temp=anes20202[anes20202$gender==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Women
temp=anes20202[anes20202$gender==2,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Strong partisans
temp=anes20202[anes20202$party_id==7|anes20202$party_id==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Weak partisans
temp=anes20202[anes20202$party_id==6|anes20202$party_id==2,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Leaners
temp=anes20202[anes20202$party_id==5|anes20202$party_id==3,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Independents
temp=anes20202[anes20202$party_id==4,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Conservatives
temp=anes20202[anes20202$ideology>4,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Moderates
temp=anes20202[anes20202$ideology==4,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Liberals
temp=anes20202[anes20202$ideology<4,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Whites
temp=anes20202[anes20202$race==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Blacks
temp=anes20202[anes20202$race==2,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Hispanics
temp=anes20202[anes20202$race==5,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Attend church
temp=anes20202[anes20202$attend_church==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Doesn't attend church
temp=anes20202[anes20202$attend_church==2,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Interest in politics (high)
temp=anes20202[anes20202$interest==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Interest in politics (low)
temp=anes20202[anes20202$interest==4,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Discuss politics
temp=anes20202[anes20202$discuss_politics==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Discuss politics (not)
temp=anes20202[anes20202$discuss_politics==2,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Age (18-30)
temp=anes20202[anes20202$age>=18&anes20202$age<31,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Age (31-45)
temp=anes20202[anes20202$age>=31&anes20202$age<46,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Age (46-60)
temp=anes20202[anes20202$age>=46&anes20202$age<61,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Age (Over 60)
temp=anes20202[anes20202$age>=61,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]


####Online Appendix####
##These commands replicate the numbers presented in the Online Appendix.

###Appendix Table 7: 2012 results####
#Whole sample
temp=anes20122
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Men
temp=anes20122[anes20122$gender==1,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Women
temp=anes20122[anes20122$gender==2,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Strong partisans
temp=anes20122[anes20122$party_id==7|anes20122$party_id==1,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Weak partisans
temp=anes20122[anes20122$party_id==6|anes20122$party_id==2,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Leaners
temp=anes20122[anes20122$party_id==5|anes20122$party_id==3,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Independents
temp=anes20122[anes20122$party_id==4,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Conservatives
temp=anes20122[anes20122$ideology>4,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Moderates
temp=anes20122[anes20122$ideology==4,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Liberals
temp=anes20122[anes20122$ideology<4,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Whites
temp=anes20122[anes20122$race==1,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Blacks
temp=anes20122[anes20122$race==2,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Hispanics
temp=anes20122[anes20122$race==5,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Attend church
temp=anes20122[anes20122$attend_church==1,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Doesn't attend church
temp=anes20122[anes20122$attend_church==2,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Interest in politics (high)
temp=anes20122[anes20122$interest==1,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Interest in politics (low)
temp=anes20122[anes20122$interest==4,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Discuss politics
temp=anes20122[anes20122$discuss_politics==1,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Discuss politics (not)
temp=anes20122[anes20122$discuss_politics==2,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Age (18-30)
temp=anes20122[anes20122$age>=18&anes20122$age<31,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Age (31-45)
temp=anes20122[anes20122$age>=31&anes20122$age<46,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Age (46-60)
temp=anes20122[anes20122$age>=46&anes20122$age<61,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#Age (Over 60)
temp=anes20122[anes20122$age>=61,]
tetrachoric(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))

#States:
#CA
temp=anes20122[anes20122$state==6,]
tetrachoric(table(temp$vote_romney, temp$gpt3_vote))
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Texas
temp=anes20122[anes20122$state==48,]
tetrachoric(table(temp$vote_romney, temp$gpt3_vote))
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#NY
temp=anes20122[anes20122$state==36,]
tetrachoric(table(temp$vote_romney, temp$gpt3_vote)) 
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Ohio
temp=anes20122[anes20122$state==39,]
tetrachoric(table(temp$vote_romney, temp$gpt3_vote))
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#Arizona
temp=anes20122[anes20122$state==4,]
tetrachoric(table(temp$vote_romney, temp$gpt3_vote))
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

#WI
temp=anes20122[anes20122$state==55,]
tetrachoric(table(temp$vote_romney, temp$gpt3_vote))
cohen.kappa(cbind(temp$vote_romney, temp$gpt3_vote))
ICC(cbind(temp$vote_romney, temp$gpt3_vote))
prop.table(table(temp$vote_romney, temp$gpt3_vote))[4]+prop.table(table(temp$vote_romney, temp$gpt3_vote))[1]

####Appendix Table 8: 2016 results####
#Whole sample
temp=anes2
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Men
temp=anes2[anes2$gender==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Women
temp=anes2[anes2$gender==2,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Strong partisans
temp=anes2[anes2$party_id==7|anes2$party_id==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Weak partisans
temp=anes2[anes2$party_id==6|anes2$party_id==2,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Leaners
temp=anes2[anes2$party_id==5|anes2$party_id==3,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Independents
temp=anes2[anes2$party_id==4,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Conservatives
temp=anes2[anes2$ideology>4,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Moderates
temp=anes2[anes2$ideology==4,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Liberals
temp=anes2[anes2$ideology<4,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Whites
temp=anes2[anes2$race==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Blacks
temp=anes2[anes2$race==2,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Hispanics
temp=anes2[anes2$race==5,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Attend church
temp=anes2[anes2$attend_church==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Doesn't attend church
temp=anes2[anes2$attend_church==2,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Interest in politics (high)
temp=anes2[anes2$interest==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Interest in politics (low)
temp=anes2[anes2$interest==4,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Discuss politics
temp=anes2[anes2$discuss_politics==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Discuss politics (not)
temp=anes2[anes2$discuss_politics==2,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Age (18-30)
temp=anes2[anes2$age>=18&anes2$age<31,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Age (31-45)
temp=anes2[anes2$age>=31&anes2$age<46,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Age (46-60)
temp=anes2[anes2$age>=46&anes2$age<61,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Age (Over 60)
temp=anes2[anes2$age>=61,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#States:
#Arizona
temp=anes2[anes2$state==4,]
tetrachoric(table(temp$vote_trump, temp$gpt3_vote))
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#CA
temp=anes2[anes2$state==6,]
tetrachoric(table(temp$vote_trump, temp$gpt3_vote))
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Texas
temp=anes2[anes2$state==48,]
tetrachoric(table(temp$vote_trump, temp$gpt3_vote))
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#NY
temp=anes2[anes2$state==36,]
tetrachoric(table(temp$vote_trump, temp$gpt3_vote)) 
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#Ohio
temp=anes2[anes2$state==39,]
tetrachoric(table(temp$vote_trump, temp$gpt3_vote))
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

#WI
temp=anes2[anes2$state==55,]
tetrachoric(table(temp$vote_trump, temp$gpt3_vote))
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]

####Appendix Table 9: 2020 results####
#Whole sample
temp=anes20202
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Men
temp=anes20202[anes20202$gender==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Women
temp=anes20202[anes20202$gender==2,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Strong partisans
temp=anes20202[anes20202$party_id==7|anes20202$party_id==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Weak partisans
temp=anes20202[anes20202$party_id==6|anes20202$party_id==2,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Leaners
temp=anes20202[anes20202$party_id==5|anes20202$party_id==3,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Independents
temp=anes20202[anes20202$party_id==4,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Conservatives
temp=anes20202[anes20202$ideology>4,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Moderates
temp=anes20202[anes20202$ideology==4,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Liberals
temp=anes20202[anes20202$ideology<4,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Whites
temp=anes20202[anes20202$race==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Blacks
temp=anes20202[anes20202$race==2,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Hispanics
temp=anes20202[anes20202$race==5,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Attend church
temp=anes20202[anes20202$attend_church==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Doesn't attend church
temp=anes20202[anes20202$attend_church==2,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Interest in politics (high)
temp=anes20202[anes20202$interest==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Interest in politics (low)
temp=anes20202[anes20202$interest==4,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Discuss politics
temp=anes20202[anes20202$discuss_politics==1,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Discuss politics (not)
temp=anes20202[anes20202$discuss_politics==2,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Age (18-30)
temp=anes20202[anes20202$age>=18&anes20202$age<31,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Age (31-45)
temp=anes20202[anes20202$age>=31&anes20202$age<46,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Age (46-60)
temp=anes20202[anes20202$age>=46&anes20202$age<61,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

#Age (Over 60)
temp=anes20202[anes20202$age>=61,]
tetrachoric(cbind(temp$vote_trump, temp$gpt3_vote))
prop.table(table(temp$vote_trump, temp$gpt3_vote))[4]+prop.table(table(temp$vote_trump, temp$gpt3_vote))[1]
cohen.kappa(cbind(temp$vote_trump, temp$gpt3_vote))
ICC(cbind(temp$vote_trump, temp$gpt3_vote))

