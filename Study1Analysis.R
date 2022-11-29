###Study 1 Analysis
##Last updated: 26 October 2022

############################################
############################################
##To load packages necessary for analysis:
library(data.table)
library(readr)
library(car)
library(vcd)
library(rstatix)
library(fixest)
library(psych)
library(ggplot2)
library(stringi)
library(here)
library(rstudioapi)


############################################
############################################
###A function to help with analysis:

rescale.to.1 <- function(var, num) (1/(num-1)*(var-num)+1) #var=name of variable, num = max number on current scale (so, for a 7-point scale, you would put 7 -- starting number must be 1)

############################################
############################################
##To load the data:

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

lucid.df <- read.csv(here::here(thisPath(),"Study1_Data","Lucid_FullData_4Feb_with_background.csv"))

#######################################
#######################################
###Code to remove the repeats from a server error (A few texts were coded more than 3 times before we caught this). This code keeps the first assignment, deleting the others:

#sort(table(lucid.df$Person),decreasing=T)
lucid.dt <- as.data.table(lucid.df)
setkey(lucid.dt, Person)
lucid.dt <- unique(lucid.dt, by = key(lucid.dt))
lucid.df <- as.data.frame(lucid.dt)
#sort(table(lucid.df$Person),decreasing=T)
rm(lucid.dt)


############################################
############################################
##Now to reshape the data for the coding analysis. 

#Since we're using grep, we need to change some of the names so that it can correctly call just the variables we want:

setnames(lucid.df, old=c("political_party"), new=c("pp")) #The new name for the respondent's political party provided by Lucid; had to change this so grep would work below

setnames(lucid.df, old=c("ID.1","ID.2","ID.3","ID.4","ID.5","ID.6","ID.7","ID.8"), new=c("codtex1","codtex2","codtex3","codtex4","codtex5","codtex6","codtex7","codtex8")) #To pull just those texts that were coded by respondents, not the turing texts.

setnames(lucid.df, old=c("source.1","source.2","source.3","source.4","source.5","source.6","source.7","source.8"), new=c("codso1","codso2","codso3","codso4","codso5","codso6","codso7","codso8"))

setnames(lucid.df, old=c("PID3_p.1","PID3_p.2","PID3_p.3","PID3_p.4","PID3_p.5","PID3_p.6","PID3_p.7","PID3_p.8"), new=c("corrpid1","corrpid2","corrpid3","corrpid4","corrpid5","corrpid6","corrpid7","corrpid8"))

setnames(lucid.df, old=c("text.1","text.2","text.3","text.4","text.5","text.6","text.7","text.8"), new=c("codwords1","codwords2","codwords3","codwords4","codwords5","codwords6","codwords7","codwords8"))

setnames(lucid.df, old=c("turing1","turing2","turing3","turing4","turing5","turing6","turing7","turing8"), new=c("tada1","tada2","tada3","tada4","tada5","tada6","tada7","tada8"))

setnames(lucid.df, old=c("text.9","text.10","text.11","text.12","text.13","text.14","text.15","text.16"), new=c("turwords1","turwords2","turwords3","turwords4","turwords5","turwords6","turwords7","turwords8"))

setnames(lucid.df, old=c("ID.9","ID.10","ID.11","ID.12","ID.13","ID.14","ID.15","ID.16"), new=c("tng1","tng2","tng3","tng4","tng5","tng6","tng7","tng8"))

setnames(lucid.df, old=c("source.9","source.10","source.11","source.12","source.13","source.14","source.15","source.16"), new=c("tsso1","tsso2","tsso3","tsso4","tsso5","tsso6","tsso7","tsso8"))

setnames(lucid.df, old=c("PID3_p.9","PID3_p.10","PID3_p.11","PID3_p.12","PID3_p.13","PID3_p.14","PID3_p.15","PID3_p.16"), new=c("trrpid1","trrpid2","trrpid3","trrpid4","trrpid5","trrpid6","trrpid7","trrpid8"))

setnames(lucid.df, old=c("PID7.1","PID7.2","PID7.3","PID7.4","PID7.5","PID7.6","PID7.7","PID7.8"), new=c("ctsevpid1","ctsevpid2","ctsevpid3","ctsevpid4","ctsevpid5","ctsevpid6","ctsevpid7","ctsevpid8"))

setnames(lucid.df, old=c("PID7.9","PID7.10","PID7.11","PID7.12","PID7.13","PID7.14","PID7.15","PID7.16"), new=c("tasevpid1","tasevpid2","tasevpid3","tasevpid4","tasevpid5","tasevpid6","tasevpid7","tasevpid8"))

setnames(lucid.df, old=c("Inc.1","Inc.2","Inc.3","Inc.4","Inc.5","Inc.6","Inc.7","Inc.8"), new=c("ctsevinc1","ctsevinc2","ctsevinc3","ctsevinc4","ctsevinc5","ctsevinc6","ctsevinc7","ctsevinc8"))

setnames(lucid.df, old=c("Inc.9","Inc.10","Inc.11","Inc.12","Inc.13","Inc.14","Inc.15","Inc.16"), new=c("tasevinc1","tasevinc2","tasevinc3","tasevinc4","tasevinc5","tasevinc6","tasevinc7","tasevinc8"))

setnames(lucid.df, old=c("Hisp.1","Hisp.2","Hisp.3","Hisp.4","Hisp.5","Hisp.6","Hisp.7","Hisp.8"), new=c("ctsevhisp1","ctsevhisp2","ctsevhisp3","ctsevhisp4","ctsevhisp5","ctsevhisp6","ctsevhisp7","ctsevhisp8"))

setnames(lucid.df, old=c("Hisp.9","Hisp.10","Hisp.11","Hisp.12","Hisp.13","Hisp.14","Hisp.15","Hisp.16"), new=c("tasevhisp1","tasevhisp2","tasevhisp3","tasevhisp4","tasevhisp5","tasevhisp6","tasevhisp7","tasevhisp8"))

setnames(lucid.df, old=c("Age.1","Age.2","Age.3","Age.4","Age.5","Age.6","Age.7","Age.8"), new=c("ctsevage1","ctsevage2","ctsevage3","ctsevage4","ctsevage5","ctsevage6","ctsevage7","ctsevage8"))

setnames(lucid.df, old=c("Age.9","Age.10","Age.11","Age.12","Age.13","Age.14","Age.15","Age.16"), new=c("tasevage1","tasevage2","tasevage3","tasevage4","tasevage5","tasevage6","tasevage7","tasevage8"))

setnames(lucid.df, old=c("White.1","White.2","White.3","White.4","White.5","White.6","White.7","White.8"), new=c("ctsevwhite1","ctsevwhite2","ctsevwhite3","ctsevwhite4","ctsevwhite5","ctsevwhite6","ctsevwhite7","ctsevwhite8"))

setnames(lucid.df, old=c("White.9","White.10","White.11","White.12","White.13","White.14","White.15","White.16"), new=c("tasevwhite1","tasevwhite2","tasevwhite3","tasevwhite4","tasevwhite5","tasevwhite6","tasevwhite7","tasevwhite8"))

setnames(lucid.df, old=c("Gender.1","Gender.2","Gender.3","Gender.4","Gender.5","Gender.6","Gender.7","Gender.8"), new=c("ctsevgender1","ctsevgender2","ctsevgender3","ctsevgender4","ctsevgender5","ctsevgender6","ctsevgender7","ctsevgender8"))

setnames(lucid.df, old=c("Gender.9","Gender.10","Gender.11","Gender.12","Gender.13","Gender.14","Gender.15","Gender.16"), new=c("tasevgender1","tasevgender2","tasevgender3","tasevgender4","tasevgender5","tasevgender6","tasevgender7","tasevgender8"))

setnames(lucid.df, old=c("uniqueID.1","uniqueID.2","uniqueID.3","uniqueID.4","uniqueID.5","uniqueID.6","uniqueID.7","uniqueID.8"), new=c("cdtexuni1","cdtexuni2","cdtexuni3","cdtexuni4","cdtexuni5","cdtexuni6","cdtexuni7","cdtexuni8"))

setnames(lucid.df, old=c("uniqueID.9","uniqueID.10","uniqueID.11","uniqueID.12","uniqueID.13","uniqueID.14","uniqueID.15","uniqueID.16"), new=c("trtexuni1","trtexuni2","trtexuni3","trtexuni4","trtexuni5","trtexuni6","trtexuni7","trtexuni8"))

setnames(lucid.df, old=c("Ideo.1","Ideo.2","Ideo.3","Ideo.4","Ideo.5","Ideo.6","Ideo.7","Ideo.8"), new=c("codideo1","codideo2","codideo3","codideo4","codideo5","codideo6","codideo7","codideo8"))

setnames(lucid.df, old=c("Ideo.9","Ideo.10","Ideo.11","Ideo.12","Ideo.13","Ideo.14","Ideo.15","Ideo.16"), new=c("trideo1","trideo2","trideo3","trideo4","trideo5","trideo6","trideo7","trideo8"))

##Now to combine everything into the new "long" dataset:
ll.df <- reshape(lucid.df, direction = "long", 
                 varying = list(c(grep("party", names(lucid.df))),
                                c(grep("positive", names(lucid.df))),
                                c(grep("extreme", names(lucid.df))),
                                c(grep("traits", names(lucid.df))),
                                c(grep("issues", names(lucid.df))),
                                c(grep("groups", names(lucid.df))),
                                rev(c(grep("codtex", names(lucid.df)))),
                                c(grep("codso", names(lucid.df))),
                                c(grep("corrpid", names(lucid.df))),
                                c(grep("codwords", names(lucid.df))),
                                c(grep("tada", names(lucid.df))),
                                rev(c(grep("tng", names(lucid.df)))),
                                c(377,363,364,365,366,367,368,369), ##turwords
                                c(344,316,318,320,322,324,326,328), ##tsso
                                c(265,251,252,253,254,255,256,257), ##trrpid
                                c(grep("ctsevpid", names(lucid.df))), 
                                c(281,267,268,269,270,271,272,273), ##tasevpid
                                c(grep("ctsevinc", names(lucid.df))),
                                c(grep("tasevinc", names(lucid.df))),
                                c(grep("ctsevhisp", names(lucid.df))),
                                c(grep("tasevhisp", names(lucid.df))),
                                c(grep("ctsevage", names(lucid.df))),
                                c(grep("tasevage", names(lucid.df))),
                                c(grep("ctsevwhite", names(lucid.df))),
                                c(grep("tasevwhite", names(lucid.df))),
                                c(grep("ctsevgender", names(lucid.df))),
                                c(grep("tasevgender", names(lucid.df))),
                                c(grep("cdtexuni", names(lucid.df))),
                                c(409,395,396,397,398,399,400,401), ##trtexuni
                                c(grep("codideo", names(lucid.df))),
                                c(249,235,236,237,238,239,240,241) ##trideo
                                ), 
                 sep = "", 
                 v.names = c("party","positive","extreme","traits","issues","groups","codtex","codso","corrpid","codwords","tada","tng","turwords","tsso","trrpid","ctsevpid","tasevpid","ctsevinc","tasevinc","ctsevhisp","tasevhisp","ctsevage","tasevage","ctsevwhite","tasevwhite","ctsevgender","tasevgender","cdtexuni","trtexuni","codideo","trideo"))
ll.df <- ll.df[order(ll.df$ResponseId),]
long.df <- data.frame(ridQ = ll.df$ResponseId,
                      widerownum = ll.df$ExternalReference,
                      responsenum = rep(1:8,nrow(lucid.df)),
                      party = ll.df$party,
                      positive = ll.df$positive,
                      extreme = ll.df$extreme,
                      traits = ll.df$traits,
                      issues = ll.df$issues,
                      groups = ll.df$groups,
                      codtextID = ll.df$codtex,
                      codsource = ll.df$codso,
                      codcorrpid3 = ll.df$corrpid,
                      codcorrpid7 = ll.df$ctsevpid,
                      codcorrhisp=ll.df$ctsevhisp,
                      codcorrage=ll.df$ctsevage,
                      codcorrinc=ll.df$ctsevinc,
                      codcorrwhite=ll.df$ctsevwhite,
                      codcorrgender=ll.df$ctsevgender,
                      codtexuni=ll.df$cdtexuni,
                      codideo=ll.df$codideo,
                      age = ll.df$age, 
                      gender = ll.df$gender, 
                      hhi = ll.df$hhi, 
                      ethnicity = ll.df$ethnicity, 
                      hispanic = ll.df$hispanic, 
                      education = ll.df$education, 
                      political_party = ll.df$pp, 
                      region = ll.df$region, 
                      zip = ll.df$zip, 
                      codtextwords = ll.df$codwords,
                      turing = ll.df$tada,
                      turtextID = ll.df$tng,
                      turtextwords = ll.df$turwords,
                      tursource = ll.df$tsso,
                      turcorrpid3 = ll.df$trrpid,
                      turcorrpid7 = ll.df$tasevpid,
                      turcorrhisp=ll.df$tasevhisp,
                      turcorrage=ll.df$tasevage,
                      turcorrinc=ll.df$tasevinc,
                      turcorrwhite=ll.df$tasevwhite,
                      turcorrgender=ll.df$tasevgender,
                      turtexuni=ll.df$trtexuni,
                      trideo=ll.df$trideo
                      )
rm(ll.df)

############################################
############################################
##To code/clean the variables for analysis:

long.df$party <- as.factor(long.df$party)
long.df$positive <- car::recode(long.df$positive, '
                                  "Very negative" = 1;
                                  "A little negative" = 2;
                                  "Neither positive nor negative" = 3;
                                  "A little positive" = 4;
                                  "Very positive" = 5
                                  ')
long.df$positive <- rescale.to.1(long.df$positive,5) #So it's on the same scale as the other traits
long.df$extreme <- car::recode(long.df$extreme, '
                                  "Yes" = 1;
                                  "No" = 0
                                  ')
long.df$traits <- car::recode(long.df$traits, '
                                  "Yes" = 1;
                                  "No" = 0
                                  ')
long.df$issues <- car::recode(long.df$issues, '
                                  "Yes" = 1;
                                  "No" = 0
                                  ')
long.df$groups <- car::recode(long.df$groups, '
                                  "Yes" = 1;
                                  "No" = 0
                                  ')
long.df$codtextID <- as.factor(long.df$codtextID)
long.df$codcorrpid3 <- car::recode(long.df$codcorrpid3, '
                                  "Independent (excludes leaners)" = "Independent"
                                  ')
long.df$codcorrpid3 <- as.factor(long.df$codcorrpid3)
long.df$codcorrpid7 <- as.factor(long.df$codcorrpid7)
long.df$codcorrhisp <- as.factor(long.df$codcorrhisp)
long.df$codcorrhisp[grepl("^\\s*$", long.df$codcorrhisp)] <- NA
long.df$codcorrhisp = droplevels(long.df$codcorrhisp)
long.df$codcorrhisp <- relevel(long.df$codcorrhisp, ref="Hispanic")
long.df$codcorrinc <- car::recode(long.df$codcorrinc, '
                                  "" = NA;
                                  "-8" = NA;
                                  "Prefer not to answer" = NA;
                                  "Less than $15K" = 1;
                                  "$15K to $25K" = 2;
                                  "$25K to $50K" = 3;
                                  "$50K to $75K" = 4;
                                  "$75K to $100K" = 5;
                                  "$100K to $150K" = 6;
                                  "$150K to $200K" = 7;
                                  "$200K to $250K" = 8;
                                  "$250K to $500K" = 9;
                                  "$500K to $1,000K" = 10;
                                  "More than $1,000K" = 11
                                  ')
long.df$codcorrwhite <- as.factor(long.df$codcorrwhite)
long.df$codcorrwhite[grepl("^\\s*$", long.df$codcorrwhite)] <- NA
long.df$codcorrwhite = droplevels(long.df$codcorrwhite)
long.df$codcorrwhite <- relevel(long.df$codcorrwhite, ref="Non-white")
long.df$codcorrgender <- as.factor(long.df$codcorrgender)
long.df$codcorrgender[grepl("^\\s*$", long.df$codcorrgender)] <- NA
long.df$codcorrgender = droplevels(long.df$codcorrgender)
long.df$codcorrgender <- relevel(long.df$codcorrgender, ref="Male")
long.df$hhi[long.df$hhi<0] <- NA
long.df$ethnicity <- as.factor(long.df$ethnicity)
long.df$hispanic <- as.factor(long.df$hispanic)
long.df$education[long.df$education<0] <- NA
long.df$region <- as.factor(long.df$region)
long.df$zip <- as.factor(long.df$zip)
long.df$turing <- car::recode(long.df$turing, '
                                  "Computer" = 1;
                                  "Person" = 0
                                  ')
long.df$turtextID <- as.factor(long.df$turtextID)
long.df$turcorrpid3 <- as.factor(long.df$turcorrpid3)
long.df$turcorrpid7 <- as.factor(long.df$turcorrpid7)
long.df$turcorrhisp <- as.factor(long.df$turcorrhisp)
long.df$turcorrhisp[grepl("^\\s*$", long.df$turcorrhisp)] <- NA
long.df$turcorrhisp = droplevels(long.df$turcorrhisp)
long.df$turcorrhisp <- relevel(long.df$turcorrhisp, ref="Hispanic")
long.df$turcorrinc <- car::recode(long.df$turcorrinc, '
                                  "" = NA;
                                  "-8" = NA;
                                  "Prefer not to answer" = NA;
                                  "Less than $15K" = 1;
                                  "$15K to $25K" = 2;
                                  "$25K to $50K" = 3;
                                  "$50K to $75K" = 4;
                                  "$75K to $100K" = 5;
                                  "$100K to $150K" = 6;
                                  "$150K to $200K" = 7;
                                  "$200K to $250K" = 8;
                                  "$250K to $500K" = 9;
                                  "$500K to $1,000K" = 10;
                                  "More than $1,000K" = 11
                                  ')
long.df$turcorrwhite <- as.factor(long.df$turcorrwhite)
long.df$turcorrwhite[grepl("^\\s*$", long.df$turcorrwhite)] <- NA
long.df$turcorrwhite = droplevels(long.df$turcorrwhite)
long.df$turcorrwhite <- relevel(long.df$turcorrwhite, ref="Non-white")
long.df$turcorrgender <- as.factor(long.df$turcorrgender)
long.df$turcorrgender[grepl("^\\s*$", long.df$turcorrgender)] <- NA
long.df$turcorrgender = droplevels(long.df$turcorrgender)
long.df$turcorrgender <- relevel(long.df$turcorrgender, ref="Male")

long.df$codideo <- factor(long.df$codideo)
long.df$trideo <- factor(long.df$trideo)

######To generate a variable that identifies the partisanship of the text source:

long.df$codtextarget <- NA
long.df$codtextarget[grepl("Democrats", long.df$codtexuni)=="TRUE"] <- "Dems"
long.df$codtextarget[grepl("Republicans", long.df$codtexuni)=="TRUE"] <- "Reps"

long.df$turtextarget <- NA
long.df$turtextarget[grepl("Democrats", long.df$turtexuni)=="TRUE"] <- "Dems"
long.df$turtextarget[grepl("Republicans", long.df$turtexuni)=="TRUE"] <- "Reps"

#######################
##To generate variables that contain the character and word length of both coding and turing texts:

long.df$codtextlength <- nchar(long.df$codtextwords)
long.df$turtextlength <- nchar(long.df$turtextwords)

long.df$codtextwnum <- stringi::stri_count_words(long.df$codtextwords)
long.df$turtextwnum <- stringi::stri_count_words(long.df$turtextwords)


############################################
############################################
##Code to replicate results in the Manuscript
############################################
############################################

####################################################
###Percentages and t-tests on the bottom of page 5 and the top of page 6
####################################################

long.df$turcorrect <- ifelse(long.df$turing==long.df$tursource, 1, 0)

#Human
sum(table(long.df$turcorrect[long.df$turcorrect==1 & long.df$tursource==0])) #6617
sum(table(long.df$turcorrect[long.df$turcorrect==0 & long.df$tursource==0])) #4104
#10721
#AI
sum(table(long.df$turcorrect[long.df$turcorrect==1 & long.df$tursource==1])) #4728
sum(table(long.df$turcorrect[long.df$turcorrect==0 & long.df$tursource==1])) #7463
#12191
##Diff in human/ai guessing it's human:
prop.test(x = c(6617,7463), n = c(10721,12191)) #No difference

####################################################
##Percentages of traits and t-test reported on the top of page 8 and graphed in Figure 3B
####################################################

##Traits:
traits.feols <- feols(traits ~ codsource + codcorrgender + codcorrhisp + codcorrinc + codcorrwhite + codcorrage + codcorrpid3 | ridQ, long.df, cluster = ~ridQ + ~codtextID)

codpredtrt.df <- data.frame(codsource = c(rep(0,length(unique(long.df$ridQ))),rep(1,length(unique(long.df$ridQ)))), 
                            ridQ = rep(unique(long.df$ridQ),2), 
                            codcorrgender = rep(factor("Female",levels = c("Male","Female","Other")),2), 
                            codcorrhisp = rep(factor("Not Hispanic", levels = c("Not Hispanic","Hispanic")),2),
                            codcorrwhite = rep(factor("White", levels = c("Non-white","White")),2),
                            codcorrpid3 = rep(factor("Democrat", levels = c("Independent","Democrat","Republican")),2),
                            codcorrinc = rep(mean(long.df$codcorrinc, na.rm=T),2),
                            codcorrage = rep(mean(long.df$codcorrage, na.rm=T),2)
) 
codpredtrt.df$pred <- predict(traits.feols,codpredtrt.df)

trtplot.df <- data.frame(
  codsource = factor(c("Human","GPT-3")),
  pred = c((t.test(codpredtrt.df$pred[codpredtrt.df$codsource==0])$estimate),t.test(codpredtrt.df$pred[codpredtrt.df$codsource==1])$estimate)
)
trtplot.df


##Extremity
extreme.feols <- feols(extreme ~ codsource + codcorrgender + codcorrhisp + codcorrinc + codcorrwhite + codcorrage + codcorrpid3 | ridQ, long.df, cluster = ~ridQ + ~codtextID)

codpredext.df <- data.frame(codsource = c(rep(0,length(unique(long.df$ridQ))),rep(1,length(unique(long.df$ridQ)))), 
                            ridQ = rep(unique(long.df$ridQ),2), 
                            codcorrgender = rep(factor("Female",levels = c("Male","Female","Other")),2), 
                            codcorrhisp = rep(factor("Not Hispanic", levels = c("Not Hispanic","Hispanic")),2),
                            codcorrpid3 = rep(factor("Democrat", levels = c("Independent","Democrat","Republican")),2),
                            codcorrwhite = rep(factor("White", levels = c("Non-white","White")),2),
                            codcorrinc = rep(mean(long.df$codcorrinc, na.rm=T),2),
                            codcorrage = rep(mean(long.df$codcorrage, na.rm=T),2)
) 
codpredext.df$pred <- predict(extreme.feols,codpredext.df)

extplot.df <- data.frame(
  codsource = factor(c("Human","GPT-3")),
  pred = c((t.test(codpredext.df$pred[codpredext.df$codsource==0])$estimate),t.test(codpredext.df$pred[codpredext.df$codsource==1])$estimate)
)
extplot.df

###Positivity
positive.feols <- feols(positive ~ codsource + codcorrgender + codcorrhisp + codcorrinc + codcorrwhite + codcorrage + codcorrpid3 | ridQ, long.df, cluster = ~ridQ + ~codtextID)

codpredpos.df <- data.frame(codsource = c(rep(0,length(unique(long.df$ridQ))),rep(1,length(unique(long.df$ridQ)))),
                            ridQ = rep(unique(long.df$ridQ),2), 
                            codcorrgender = rep(factor("Female",levels = c("Male","Female","Other")),2), 
                            codcorrhisp = rep(factor("Not Hispanic", levels = c("Not Hispanic","Hispanic")),2),
                            codcorrwhite = rep(factor("White", levels = c("Non-white","White")),2),
                            codcorrpid3 = rep(factor("Democrat", levels = c("Independent","Democrat","Republican")),2),
                            codcorrinc = rep(mean(long.df$codcorrinc, na.rm=T),2),
                            codcorrage = rep(mean(long.df$codcorrage, na.rm=T),2)
) 
codpredpos.df$pred <- predict(positive.feols,codpredpos.df)
t.test(codpredpos.df$pred ~ codpredpos.df$codsource)

posplot.df <- data.frame(
  codsource = factor(c("Human","GPT-3")),
  pred = c((t.test(codpredpos.df$pred[codpredpos.df$codsource==0])$estimate),t.test(codpredpos.df$pred[codpredpos.df$codsource==1])$estimate)
)
posplot.df

##Issues:
issues.feols <- feols(issues ~ codsource + codcorrgender + codcorrhisp + codcorrinc + codcorrwhite + codcorrage + codcorrpid3 | ridQ, long.df, cluster = ~ridQ + ~codtextID)

codprediss.df <- data.frame(codsource = c(rep(0,length(unique(long.df$ridQ))),rep(1,length(unique(long.df$ridQ)))), 
                            ridQ = rep(unique(long.df$ridQ),2), 
                            codcorrgender = rep(factor("Female",levels = c("Male","Female","Other")),2), 
                            codcorrhisp = rep(factor("Not Hispanic", levels = c("Not Hispanic","Hispanic")),2),
                            codcorrwhite = rep(factor("White", levels = c("Non-white","White")),2),
                            codcorrpid3 = rep(factor("Democrat", levels = c("Independent","Democrat","Republican")),2),
                            codcorrinc = rep(mean(long.df$codcorrinc, na.rm=T),2),
                            codcorrage = rep(mean(long.df$codcorrage, na.rm=T),2)
) 
codprediss.df$pred <- predict(issues.feols,codprediss.df)

issplot.df <- data.frame(
  codsource = factor(c("Human","GPT-3")),
  pred = c((t.test(codprediss.df$pred[codprediss.df$codsource==0])$estimate),t.test(codprediss.df$pred[codprediss.df$codsource==1])$estimate)
)
issplot.df

##groups
groups.feols <- feols(groups ~ codsource + codcorrgender + codcorrhisp + codcorrinc + codcorrwhite + codcorrage + codcorrpid3 | ridQ, long.df, cluster = ~ridQ + ~codtextID) #sig more mention of groups

codpredgrp.df <- data.frame(codsource = c(rep(0,length(unique(long.df$ridQ))),rep(1,length(unique(long.df$ridQ)))), 
                            ridQ = rep(unique(long.df$ridQ),2), 
                            codcorrgender = rep(factor("Female",levels = c("Male","Female","Other")),2), 
                            codcorrhisp = rep(factor("Not Hispanic", levels = c("Not Hispanic","Hispanic")),2),
                            codcorrwhite = rep(factor("White", levels = c("Non-white","White")),2),
                            codcorrpid3 = rep(factor("Democrat", levels = c("Independent","Democrat","Republican")),2),
                            codcorrinc = rep(mean(long.df$codcorrinc, na.rm=T),2),
                            codcorrage = rep(mean(long.df$codcorrage, na.rm=T),2)
) 
codpredgrp.df$pred <- predict(groups.feols,codpredgrp.df)

grpplot.df <- data.frame(
  codsource = factor(c("Human","GPT-3")),
  pred = c((t.test(codpredgrp.df$pred[codpredgrp.df$codsource==0])$estimate),t.test(codpredgrp.df$pred[codpredgrp.df$codsource==1])$estimate)
)
grpplot.df

##To estimate percent of PIDs correctly predicted
long.df$codcorrect <- ifelse(long.df$party==long.df$codcorrpid3, 1, 0)

codprobcorr.feols <- feols(codcorrect ~ codsource + codcorrgender + codcorrhisp + codcorrinc + codcorrwhite + codcorrage + codtextwnum + codcorrpid3 | ridQ, long.df, cluster = ~ridQ + ~codtextID)

codpredcorr.df <- data.frame(codsource = c(rep(0,length(unique(long.df$ridQ))),rep(1,length(unique(long.df$ridQ)))), 
                             ridQ = rep(unique(long.df$ridQ),2), 
                             codcorrgender = rep(factor("Female",levels = c("Male","Female","Other")),2), 
                             codcorrhisp = rep(factor("Not Hispanic", levels = c("Not Hispanic","Hispanic")),2),
                             codcorrwhite = rep(factor("White", levels = c("Non-white","White")),2),
                             codcorrpid3 = rep(factor("Democrat", levels = c("Independent","Democrat","Republican")),2),
                             codcorrinc = rep(mean(long.df$codcorrinc, na.rm=T),2),
                             codcorrage = rep(mean(long.df$codcorrage, na.rm=T),2),
                             codtextwnum = rep(mean(long.df$codcorrage, na.rm=T),2)
) 
codpredcorr.df$pred <- predict(codprobcorr.feols,codpredcorr.df)

codcorrplot.df <- data.frame(
  codsource = factor(c("Human","GPT-3")),
  pred = c((t.test(codpredcorr.df$pred[codpredcorr.df$codsource==0])$estimate),t.test(codpredcorr.df$pred[codpredcorr.df$codsource==1])$estimate)
)
codcorrplot.df
t.test(codpredcorr.df$pred ~ codpredcorr.df$codsource)

##Table of values used to plot Figure 3B (These were inputted into fig3_data.py and then graphed using python. The graph was then cleaned using Adobe Illustrator.)

char.df <- rbind(
  "Pcorrect" = codcorrplot.df,
  "Positive" = posplot.df,
  "Extreme" = extplot.df,
  "Traits" = trtplot.df,
  "Issues" = issplot.df,
  "Groups" = grpplot.df
)

####################################################
##To generate the values used to plot Figure 3A:
####################################################

##Predicted proportion of texts identified as positive, by ideology:
uhood3.f <- function(IDEO,PID){
  positive.feols <- feols(positive ~ codsource + codcorrgender + codcorrhisp + codcorrinc + codcorrwhite + codcorrage | ridQ, long.df[long.df$codideo==IDEO & long.df$codtextarget==PID,], cluster = ~ridQ + ~codtextID)
  
  codpredpos.df <- data.frame(codsource = c(rep(0,length(unique(long.df$ridQ))),rep(1,length(unique(long.df$ridQ)))),
                              ridQ = rep(unique(long.df$ridQ),2), 
                              codcorrgender = rep(factor("Female",levels = c("Male","Female","Other")),2), 
                              codcorrhisp = rep(factor("Not Hispanic", levels = c("Not Hispanic","Hispanic")),2),
                              codcorrwhite = rep(factor("White", levels = c("Non-white","White")),2),
                              codcorrinc = rep(mean(long.df$codcorrinc, na.rm=T),2),
                              codcorrage = rep(mean(long.df$codcorrage, na.rm=T),2)
  ) 
  codpredpos.df$pred <- predict(positive.feols,codpredpos.df)
  
  posplot.df <- data.frame(
    codsource = factor(c("Human","GPT-3")),
    pred = c((t.test(codpredpos.df$pred[codpredpos.df$codsource==0])$estimate),t.test(codpredpos.df$pred[codpredpos.df$codsource==1])$estimate)
  )
  posplot.df
  #positive.feols
}

posreps.df <- as.data.frame(rbind(
uhood3.f("Extremely conservative","Reps"),
uhood3.f("Conservative","Reps"),
uhood3.f("Slightly conservative","Reps"),
uhood3.f("Moderate/Haven't thought about it","Reps"),
uhood3.f("Slightly liberal","Reps"),
uhood3.f("Liberal","Reps"),
uhood3.f("Extremely Liberal","Reps")
))
posreps.df$Ideology <- c("Extremely conservative","Extremely conservative","Conservative","Conservative","Slightly conservative","Slightly conservative","Moderate/Haven't thought about it","Moderate/Haven't thought about it","Slightly liberal","Slightly liberal","Liberal","Liberal","Extremely Liberal","Extremely Liberal")
posreps.df$PID <- c(rep("Republicans",14))


posdems.df <- as.data.frame(rbind(
uhood3.f("Extremely conservative","Dems"),
uhood3.f("Conservative","Dems"),
uhood3.f("Slightly conservative","Dems"),
uhood3.f("Moderate/Haven't thought about it","Dems"),
uhood3.f("Slightly liberal","Dems"),
uhood3.f("Liberal","Dems"),
uhood3.f("Extremely Liberal","Dems")
))
posdems.df$Ideology <- c("Extremely conservative","Extremely conservative","Conservative","Conservative","Slightly conservative","Slightly conservative","Moderate/Haven't thought about it","Moderate/Haven't thought about it","Slightly liberal","Slightly liberal","Liberal","Liberal","Extremely Liberal","Extremely Liberal")
posdems.df$PID <- c(rep("Democrats",14))

##Predicted proportion of texts identified as "extreme," by ideology:
uhood4.f <- function(IDEO,PID){
  extreme.feols <- feols(extreme ~ codsource + codcorrgender + codcorrhisp + codcorrinc + codcorrwhite + codcorrage | ridQ, long.df[long.df$codideo==IDEO & long.df$codtextarget==PID,], cluster = ~ridQ + ~codtextID) 
  
  codpredext.df <- data.frame(codsource = c(rep(0,length(unique(long.df$ridQ))),rep(1,length(unique(long.df$ridQ)))), 
                              ridQ = rep(unique(long.df$ridQ),2), 
                              codcorrgender = rep(factor("Female",levels = c("Male","Female","Other")),2), 
                              codcorrhisp = rep(factor("Not Hispanic", levels = c("Not Hispanic","Hispanic")),2),
                              codcorrwhite = rep(factor("White", levels = c("Non-white","White")),2),
                              codcorrinc = rep(mean(long.df$codcorrinc, na.rm=T),2),
                              codcorrage = rep(mean(long.df$codcorrage, na.rm=T),2)
  ) 
  codpredext.df$pred <- predict(extreme.feols,codpredext.df)
  
  extplot.df <- data.frame(
    codsource = factor(c("Human","GPT-3")),
    pred = c((t.test(codpredext.df$pred[codpredext.df$codsource==0])$estimate),t.test(codpredext.df$pred[codpredext.df$codsource==1])$estimate)
  )
  extplot.df
  #extreme.feols
}

extreps.df <- as.data.frame(rbind(
uhood4.f("Extremely conservative","Reps"),
uhood4.f("Conservative","Reps"),
uhood4.f("Slightly conservative","Reps"),
uhood4.f("Moderate/Haven't thought about it","Reps"),
uhood4.f("Slightly liberal","Reps"),
uhood4.f("Liberal","Reps"),
uhood4.f("Extremely Liberal","Reps")
))
extreps.df$Ideology <- c("Extremely conservative","Extremely conservative","Conservative","Conservative","Slightly conservative","Slightly conservative","Moderate/Haven't thought about it","Moderate/Haven't thought about it","Slightly liberal","Slightly liberal","Liberal","Liberal","Extremely Liberal","Extremely Liberal")
extreps.df$PID <- c(rep("Republicans",14))

extdems.df <- as.data.frame(rbind(
uhood4.f("Extremely conservative","Dems"),
uhood4.f("Conservative","Dems"),
uhood4.f("Slightly conservative","Dems"),
uhood4.f("Moderate/Haven't thought about it","Dems"),
uhood4.f("Slightly liberal","Dems"),
uhood4.f("Liberal","Dems"),
uhood4.f("Extremely Liberal","Dems")
))
extdems.df$Ideology <- c("Extremely conservative","Extremely conservative","Conservative","Conservative","Slightly conservative","Slightly conservative","Moderate/Haven't thought about it","Moderate/Haven't thought about it","Slightly liberal","Slightly liberal","Liberal","Liberal","Extremely Liberal","Extremely Liberal")
extdems.df$PID <- c(rep("Democrats",14))

posreps.df
posdems.df
extreps.df
extdems.df

##The values from these tables were fed into fig3_data.py, plotted in Python using fig3.py, and then the plot was processed using Adobe Illustrator).



############################################
############################################
##Code to replicate results in the Study 1 Appendix
############################################
############################################


##########
##To replicate results in Appendix 2.1
sum(table(levels(as.factor(long.df$codtexuni)))) #7675 unique texts
sum(table(levels(as.factor(long.df$codtexuni[long.df$codsource==0])))) #3592 Human
sum(table(levels(as.factor(long.df$codtexuni[long.df$codsource==1])))) #4083 GPT-3 

#Figure 3 plots (plotted in python in the paper and then cleaned up using Adobe Illustrator)
pdf(here::here(thisPath(),"Figures","Appendix_Figure3.pdf"), width=8, height=6,family="Times")
par(mfrow=c(1,2))
hist(long.df$codtextwnum[long.df$codsource==0]) #Humans
hist(long.df$codtextwnum[long.df$codsource==1]) #GPT-3
dev.off()

##Summary statistics
describe(long.df$codtextwnum[long.df$codsource==0]) #Humans
describe(long.df$codtextwnum[long.df$codsource==1]) #GPT-3


##########
##To replicate results in Appendix 2.2
table(table(long.df$codtexuni)) #120 coded twice, 7 coded 4 times; all the rest coded 3 times as planned


##########
##To generate the six tables of results, in the order presented, in Appendix 2.3. Note: we removed the significance stars from these by hand for the tables in the appendix.

dict = c(codsource = "Source:GPT-3",codcorrgenderFemale = "Gender:Female",codcorrgenderOther = "Gender:Other",codcorrhispNotHispanic = "Not Hispanic", codcorrinc = "Income",codcorrwhiteWhite = "White",codcorrage = "Age",codtextwnum = "Word Length",codcorrpid3Independent = "PID:Indep.",codcorrpid3Republican = "PID:Rep.",ridQ="Evaluators",codtextID = "Lists")

fixest::etable(positive.feols,extreme.feols,traits.feols,issues.feols,groups.feols,se.below = TRUE,digits=3,fitstat=~n+rmse+pr2, dict=dict,
               tex=TRUE,file=here::here(thisPath(),"Tables","Appendix_Table1.tex"),cluster=~ridQ + ~codtextID,replace = TRUE,signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),style.tex = style.tex("aer", tablefoot=T,tablefoot.value="default"))

fixest::etable(codprobcorr.feols,se.below = TRUE,digits=3,fitstat=~n+rmse+pr2, dict=dict,
               tex=TRUE,file=here::here(thisPath(),"Tables","Appendix_Table2.tex"),cluster=~ridQ + ~codtextID,replace = TRUE,signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),style.tex = style.tex("aer", tablefoot=T,tablefoot.value="default"))


uhood3.f <- function(IDEO,PID){
  positive.feols <- feols(positive ~ codsource + codcorrgender + codcorrhisp + codcorrinc + codcorrwhite + codcorrage | ridQ, long.df[long.df$codideo==IDEO & long.df$codtextarget==PID,], cluster = ~ridQ + ~codtextID)
  
  codpredpos.df <- data.frame(codsource = c(rep(0,length(unique(long.df$ridQ))),rep(1,length(unique(long.df$ridQ)))),
                              ridQ = rep(unique(long.df$ridQ),2), 
                              codcorrgender = rep(factor("Female",levels = c("Male","Female","Other")),2), 
                              codcorrhisp = rep(factor("Not Hispanic", levels = c("Not Hispanic","Hispanic")),2),
                              codcorrwhite = rep(factor("White", levels = c("Non-white","White")),2),
                              codcorrinc = rep(mean(long.df$codcorrinc, na.rm=T),2),
                              codcorrage = rep(mean(long.df$codcorrage, na.rm=T),2)
  ) 
  codpredpos.df$pred <- predict(positive.feols,codpredpos.df)
  
  posplot.df <- data.frame(
    codsource = factor(c("Human","GPT-3")),
    pred = c((t.test(codpredpos.df$pred[codpredpos.df$codsource==0])$estimate),t.test(codpredpos.df$pred[codpredpos.df$codsource==1])$estimate)
  )
  #posplot.df
  positive.feols
}

fixest::etable(uhood3.f("Extremely conservative","Reps"),uhood3.f("Conservative","Reps"),uhood3.f("Slightly conservative","Reps"),uhood3.f("Moderate/Haven't thought about it","Reps"),uhood3.f("Slightly liberal","Reps"),uhood3.f("Liberal","Reps"),uhood3.f("Extremely Liberal","Reps"),se.below = TRUE,digits=3,fitstat=~n+rmse+pr2, dict=dict,
               tex=TRUE,file=here::here(thisPath(),"Tables","Appendix_Table3.tex"),cluster=~ridQ + ~codtextID,replace = TRUE,signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),style.tex = style.tex("aer", tablefoot=T,tablefoot.value="default"))

fixest::etable(uhood3.f("Extremely conservative","Dems"),uhood3.f("Conservative","Dems"),uhood3.f("Slightly conservative","Dems"),uhood3.f("Moderate/Haven't thought about it","Dems"),uhood3.f("Slightly liberal","Dems"),uhood3.f("Liberal","Dems"),uhood3.f("Extremely Liberal","Dems"),se.below = TRUE,digits=3,fitstat=~n+rmse+pr2, dict=dict,
               tex=TRUE,file=here::here(thisPath(),"Tables","Appendix_Table4.tex"),cluster=~ridQ + ~codtextID,replace = TRUE,signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),style.tex = style.tex("aer", tablefoot=T,tablefoot.value="default"))

uhood4.f <- function(IDEO,PID){
  extreme.feols <- feols(extreme ~ codsource + codcorrgender + codcorrhisp + codcorrinc + codcorrwhite + codcorrage | ridQ, long.df[long.df$codideo==IDEO & long.df$codtextarget==PID,], cluster = ~ridQ + ~codtextID) 
  
  codpredext.df <- data.frame(codsource = c(rep(0,length(unique(long.df$ridQ))),rep(1,length(unique(long.df$ridQ)))), 
                              ridQ = rep(unique(long.df$ridQ),2), 
                              codcorrgender = rep(factor("Female",levels = c("Male","Female","Other")),2), 
                              codcorrhisp = rep(factor("Not Hispanic", levels = c("Not Hispanic","Hispanic")),2),
                              codcorrwhite = rep(factor("White", levels = c("Non-white","White")),2),
                              codcorrinc = rep(mean(long.df$codcorrinc, na.rm=T),2),
                              codcorrage = rep(mean(long.df$codcorrage, na.rm=T),2)
  ) 
  codpredext.df$pred <- predict(extreme.feols,codpredext.df)
  
  extplot.df <- data.frame(
    codsource = factor(c("Human","GPT-3")),
    pred = c((t.test(codpredext.df$pred[codpredext.df$codsource==0])$estimate),t.test(codpredext.df$pred[codpredext.df$codsource==1])$estimate)
  )
  #extplot.df
  extreme.feols
}

fixest::etable(uhood4.f("Extremely conservative","Reps"),uhood4.f("Conservative","Reps"),uhood4.f("Slightly conservative","Reps"),uhood4.f("Moderate/Haven't thought about it","Reps"),uhood4.f("Slightly liberal","Reps"),uhood4.f("Liberal","Reps"),uhood4.f("Extremely Liberal","Reps"),se.below = TRUE,digits=3,fitstat=~n+rmse+pr2, dict=dict,
               tex=TRUE,file=here::here(thisPath(),"Tables","Appendix_Table5.tex"),cluster=~ridQ + ~codtextID,replace = TRUE,signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),style.tex = style.tex("aer", tablefoot=T,tablefoot.value="default"))

fixest::etable(uhood4.f("Extremely conservative","Dems"),uhood4.f("Conservative","Dems"),uhood4.f("Slightly conservative","Dems"),uhood4.f("Moderate/Haven't thought about it","Dems"),uhood4.f("Slightly liberal","Dems"),uhood4.f("Liberal","Dems"),uhood4.f("Extremely Liberal","Dems"),se.below = TRUE,digits=3,fitstat=~n+rmse+pr2, dict=dict,
               tex=TRUE,file=here::here(thisPath(),"Tables","Appendix_Table6.tex"),cluster=~ridQ + ~codtextID,replace = TRUE,signif.code = c("***"=0.001, "**"=0.01, "*"=0.05),style.tex = style.tex("aer", tablefoot=T,tablefoot.value="default"))


#######################################
#######################################
#Save as RData file:
save(lucid.df, long.df, file=here::here(thisPath(),"Study1_Data","Output","LucidCleanedData.RData"))

#########To write both cleaned dataframes to csv format:
write_csv(lucid.df,here::here(thisPath(),"Study1_Data","Output","LucidCleaned_wide.csv"))
write_csv(long.df,here::here(thisPath(),"Study1_Data","Output","LucidCleaned_long.csv"))
