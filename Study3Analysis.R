###Replication File for Study 3 (both manuscript and appendix results)
#Last Updated: 26 October 2022

##To load necessary packages for analysis:
library(tidyverse)
library(corrplot)
library(DescTools)
library(RColorBrewer)
library(stargazer)
library(xtable)

#To set the path for data loading:

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

############################
####To load the data for analysis in the main body of the paper:
anesgpt3 <- read.csv(here::here(thisPath(),"Study3_Data","anesgpt3_task3.csv"))

#########DATA CLEANING AND PREPARATION#################

##Clean the ANES Responses
anesgpt3 <- mutate(anesgpt3, race_anes = if_else(V161310x == 1, "white",
                                        if_else(V161310x == 2, "black",
                                                if_else(V161310x == 3, "asian",
                                                        if_else(V161310x == 5, "hispanic", NA_character_)))),
                 discuss.politics_anes = if_else(V162174 == 1, "yes discuss politics",
                                            if_else(V162174 == 2, "never discuss politics", NA_character_)),
                 ideology_anes = if_else(V161126 == 1, "extremely liberal", 
                                    if_else(V161126 == 2, "liberal",
                                            if_else(V161126 == 3, "slightly liberal",
                                                    if_else(V161126 == 4, "moderate",
                                                            if_else(V161126 == 5, "slightly conservative",
                                                                    if_else(V161126 == 6, "conservative",
                                                                            if_else(V161126 == 7, "extremely conservative", NA_character_))))))),
                 pid7_anes = if_else(V161158x > 0, V161158x, NA_integer_),
                 church.goer_anes = if_else(V161244 > 0, V161244, NA_integer_),
                 age_anes = if_else(V161267 > 0, V161267, NA_integer_),
                 education_anes = if_else(V161270 >0 & V161270 <= 9, "High School or Less", 
                                     if_else(V161270 >= 10 & V161270 <= 12, "Some College / AA", 
                                             if_else(V161270 == 13, "Bachelor's",
                                                     if_else(V161270 >=14 & V161270 <= 16, "Graduate Degree", NA_character_)))),
                 gender_anes = if_else(V161342 == 1 | V161342 == 2, V161342, NA_integer_),
                 voted.2016_anes = if_else(V162031x == 0, "no", if_else(V162031x == 1, "yes", NA_character_)),
                 votechoice.2016_anes = if_else(V162062x == 1, "Hillary Clinton", if_else(V162062x == 2, "Donald Trump", if_else(V162062x == 3 | V162062x == 4 | V162062x == 5, "someone else", NA_character_))),
                 political.interest_anes = if_else(V162256 > 0, V162256, NA_integer_),
                 patriotism_anes = if_else(V162125x > 0, V162125x, NA_integer_), 
                 vote.2016_anes = if_else(voted.2016_anes == "no", "Did Not Vote", votechoice.2016_anes))

#Clean the GPT-3 responses to match the ANES 
anesgpt3 <- mutate(anesgpt3, race_gpt3 = if_else(race_gpt3 == -1, NA_integer_, race_gpt3),
                         age_gpt3 = if_else(age_gpt3 == -1, NA_integer_, age_gpt3),
                         church.goer_gpt3 = if_else(church_goer_gpt3 == -1, NA_integer_, church_goer_gpt3),
                         discuss.politics_gpt3 = if_else(discuss_politics_gpt3 == -1, NA_integer_, discuss_politics_gpt3),
                         education_gpt3 = if_else(education_gpt3 == -1, NA_integer_, education_gpt3),
                         gender_gpt3 = if_else(gender_gpt3 == -1, NA_integer_, gender_gpt3),
                         ideology_gpt3 = if_else(ideology_gpt3 == -1, NA_integer_, ideology_gpt3),
                         patriotism_gpt3 = if_else(patriotism_gpt3 == -1, NA_integer_, patriotism_gpt3),
                         pid7_gpt3 = if_else(pid7_gpt3 == -1, NA_integer_, pid7_gpt3),
                         political.interest_gpt3 = if_else(political_interest_gpt3 == -1, NA_integer_, political_interest_gpt3),
                         votechoice.2016_gpt3 = if_else(votechoice_2016_gpt3 == -1, NA_integer_, votechoice_2016_gpt3),
                         voted.2016_gpt3 = if_else(voted_2016_gpt3 == -1, NA_integer_, voted_2016_gpt3),
                         vote.2016_gpt3 = if_else(voted_2016_gpt3 == 0, "Did Not Vote", as.character(votechoice.2016_gpt3)))

##########CODE TO CREATE MAIN TEXT FIGURE 4 #################

#Create a subset of the data that only includes complete cases for the relevant variables
anesgpt3_sub <- select(anesgpt3, age_gpt3, age_anes, church.goer_gpt3, church.goer_anes, discuss.politics_gpt3, discuss.politics_anes, race_gpt3, race_anes, education_gpt3, education_anes, gender_gpt3, gender_anes, ideology_gpt3, ideology_anes, patriotism_gpt3, patriotism_anes, pid7_gpt3, pid7_anes, political.interest_gpt3, political.interest_anes, vote.2016_gpt3, vote.2016_anes)
anesgpt3_sub <- anesgpt3_sub[complete.cases(anesgpt3_sub), ]

#Calculate a matrix of Cramer's V values
cramersv.1 <- corrplot.mixed(PairApply(anesgpt3_sub, CramerV), is.corr = F, lower.col = "black", tl.col = "navy", upper = "circle", order = "hclust", tl.cex = .9, number.cex = .8, upper.col = brewer.pal(n = 10, name = "PuOr"))

#Reformat the Cramer's V output for ggplot
cramersv.long <- as.data.frame(cramersv.1$corr) %>%
  rownames_to_column("names.x") %>%
  pivot_longer(cols = !names.x, names_to = "names.y", values_to = "cramersv") %>%
  separate(col = names.x, into = c("variable.x", "source.x"), sep = "_", remove = FALSE) %>%
  separate(col = names.y, into = c("variable.y", "source.y"), sep = "_", remove = FALSE)

####This dataset assumes the same ANES Backstory information for every comparison, and then records either the ANES or GPT3 output; it removes the correlation of a variable with itself

#Clean the data (renaming and reordering to make the graph prettier)
cramersv_compare <- unite(cramersv.long, var_x_y, variable.x, variable.y, sep = "_", remove = FALSE) %>%
  filter(source.x == "anes") %>%
  filter(variable.x != variable.y)

cramersv_compare$variable.x <- factor(cramersv_compare$variable.x, 
                                      levels = c("gender", "race", "age", "education",
                                                 "church.goer", "patriotism", "discuss.politics",
                                                 "political.interest", "vote.2016", "ideology", "pid7"))

cramersv_compare$variable.y <- factor(cramersv_compare$variable.y, 
                                      levels = c("gender", "race", "age", "education",
                                                 "church.goer", "patriotism", "discuss.politics",
                                                 "political.interest", "vote.2016", "ideology", "pid7"))

cramersv_compare$source.x <- dplyr::recode(cramersv_compare$source.x, "anes" = "Human")
cramersv_compare$source.y <- dplyr::recode(cramersv_compare$source.y, "anes" = "Human", "gpt3" = "GPT-3")
cramersv_compare$source.y <- factor(cramersv_compare$source.y, levels = c("GPT-3", "Human"))
cramersv_compare <- arrange(cramersv_compare, desc(source.y))

variablelist <- c(vote.2016 = "2016 Vote", race = "Race / \n Ethnicity", political.interest = "Political \n Interest", pid7 = "Party ID", patriotism = "Patriotism", ideology = "Ideology", gender = "Gender", education =  "Education", discuss.politics = "Discusses \n Politics", church.goer = "Attends \n Church", age ="Age")

#Create the dot plot
ggplot(cramersv_compare, aes(y = fct_rev(as_factor(variable.x)), x = cramersv)) + 
  geom_line(aes(group = variable.x)) +
  geom_point(aes(color = source.y, shape = source.y), size = 3) +
  facet_wrap(. ~ variable.y, nrow = 2, labeller = labeller(variable.y = variablelist)) + 
  scale_color_brewer(palette = "Paired") +
  scale_y_discrete(labels = c("Party ID", "Ideology", "2016 Vote", "Political \n Interest", "Discusses \n Politics", "Patriotism", "Attends \n Church", "Education", "Age", "Race / \n Ethnicity", "Gender"))+
  labs(y = "ANES Background Variables", x= "Cramer's V", shape = "Source", color = "Source")+
  scale_shape_manual(values=c(19,15)) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.3)) 
ggsave(here::here(thisPath(),"Figures","Main_Figure4.pdf"), width=8, height=8,family="Times")


###################CODE TO CREATE APPENDIX TABLES 11, 12, and 13#################################

#Separate GPT3 and ANES results, and clean up headers
cramersv_gpt3 <- filter(cramersv_compare, source.y == "GPT-3")
cramersv_anes <- filter(cramersv_compare, source.y == "Human")

cramersv_gpt3 <- select(cramersv_gpt3, variable.x, variable.y, cramersv)
cramersv_gpt3 <- rename(cramersv_gpt3, cramersv.gpt3 = cramersv)

cramersv_anes <- select(cramersv_anes, variable.x, variable.y, cramersv)
cramersv_anes <- rename(cramersv_anes, cramersv.anes = cramersv)

#Join tables back together
cramersv_table<- left_join(cramersv_gpt3, cramersv_anes)

col_order <- c("variable.x", "variable.y", "cramersv.anes", "cramersv.gpt3")
cramersv_table <- cramersv_table[, col_order]
cramersv_table$variable.x <- as.character(cramersv_table$variable.x)
cramersv_table$variable.y <- as.character(cramersv_table$variable.y)

#Round to two digits
cramersv_table[,-c(1:2)] <-round(cramersv_table[,-c(1:2)],2) #the "-1" excludes column 1

#Calculate the difference
cramersv_table <- mutate(cramersv_table, cramersv_diff = cramersv.anes - cramersv.gpt3)

#Write the table out

##### Function to make stargazer compatible with longtable #####
#### Lucas de Abreu Maia ####
#### Department of Political Science ####
#### UCSD ####
#### lucasamaia.com ####
#### labreumaia@gmail.com ####
longtable.stargazer = function(..., float = T, longtable.float = F, 
                               longtable.head = T, filename = NULL){
  # Capturing stargazer to hack it
  require(stargazer)
  res = capture.output(
    stargazer(..., float = float)
  )
  # Changing tabular environment for longtable
  res = gsub("tabular", "longtable", res)
  # removing floating environment
  if(float == T & longtable.float == F){
    res[grep("table", res)[1]] = res[grep("longtable", res)[1]]
    # Removing extra longtable commands
    res = res[-grep("longtable", res)[2]]
    res = res[-length(res)]
  }
  # Adding page headings
  if(longtable.head == T){
    res = c(res[1:which(res == "\\hline \\\\[-1.8ex] ")[1] - 1], "\\endhead", res[which(res == "\\hline \\\\[-1.8ex] ")[1]:length(res)])
  }
  # Exporting
  cat(res, sep = "\n")
  # Exporting
  if(!is.null(filename)){
    cat(res, file = filename, sep = "\n")
    # Message
    cat(paste("\nLaTeX output printed to", filename, "\n", sep = " ", 
              collapse = ""))
  }else{
    cat(res, sep = "\n")
  }
}

longtable.stargazer(cramersv_table,
          type = "latex",
          style = "apsr",
          label = "tab:cramersvresults",
          title = "Appendix Tables 11-13; Divided into 3 tables in the appendix",
          filename = here::here(thisPath(),"Tables","Appendix_Tables11-13.tex"),
          align = F,
          digits = 2,
          digits.extra = 2,
          summary = FALSE,
          rownames = FALSE)

##################CODE TO GENERATE THE NUMBERS FOR APPENDIX TABLE 14############################

anesgpt3_na <- select(anesgpt3, !starts_with("V1")) 
app_table14 <- as.data.frame(round((colMeans(is.na(anesgpt3_na)))*100, 2))
app_t14_out <- xtable(app_table14)
print.xtable(app_t14_out, type="latex", file=here::here(thisPath(),"Tables","Appendix_Table14.tex"))

##################CODE TO CREATE APPENDIX TABLE 15############################

anesgpt3_sub_descriptive <- anesgpt3_sub
anesgpt3_sub_descriptive$church.goer_anes <- dplyr::recode(anesgpt3_sub_descriptive$church.goer_anes, `1` = 1, `2` = 0)
anesgpt3_sub_descriptive$church.goer_gpt3 <- dplyr::recode(anesgpt3_sub_descriptive$church.goer_gpt3, `1` = 1, `2` = 0)
anesgpt3_sub_descriptive$discuss.politics_anes <- dplyr::recode(anesgpt3_sub_descriptive$discuss.politics_anes, "yes discuss politics" = 1, "never discuss politics" = 0)
anesgpt3_sub_descriptive$discuss.politics_gpt3 <- dplyr::recode(anesgpt3_sub_descriptive$discuss.politics_gpt3, `1` = 1, `2` = 0)
anesgpt3_sub_descriptive$ideology_anes <- dplyr::recode(anesgpt3_sub_descriptive$ideology_anes, "extremely liberal" = 1, "liberal" = 2, "slightly liberal" = 3, "moderate" = 4, "slightly conservative" = 5, "conservative" = 6, "extremely conservative" = 7)
anesgpt3_sub_descriptive <- mutate(anesgpt3_sub_descriptive, white_anes = if_else(race_anes == "white", 1, 0),
                                   hispanic_anes = if_else(race_anes == "hispanic", 1, 0),
                                   asian_anes = if_else(race_anes == "asian", 1, 0),
                                   black_anes = if_else(race_anes == "black", 1, 0),
                                   white_gpt3 = if_else(race_gpt3 == 1, 1, 0),
                                   black_gpt3 = if_else(race_gpt3 == 2 , 1, 0),
                                   hispanic_gpt3 = if_else(race_gpt3 == 5, 1, 0),
                                   asian_gpt3 = if_else(race_gpt3 == 3, 1, 0),
                                   somecollege_anes = if_else(education_anes == "Some College / AA", 1, 0),
                                   graddegree_anes = if_else(education_anes == "Graduate Degree", 1, 0),
                                   bachelors_anes = if_else(education_anes == "Bachelor's", 1, 0),
                                   highschool_anes = if_else(education_anes == "High School or Less", 1, 0),
                                   somecollege_gpt3 = if_else(education_gpt3 == 12, 1, 0),
                                   graddegree_gpt3 = if_else(education_gpt3 == 16, 1, 0),
                                   bachelors_gpt3 = if_else(education_gpt3 == 13, 1, 0),
                                   highschool_gpt3 = if_else(education_gpt3 == 9, 1, 0),
                                   male_anes = if_else(gender_anes == 1, 1, 0),
                                   male_gpt3 = if_else(gender_gpt3 == 1, 1, 0),
                                   voted_2016_anes = if_else(vote.2016_anes == "Did Not Vote", 0, 1),
                                   vote_Trump_anes = if_else(vote.2016_anes == "Donald Trump", 1, (if_else(voted_2016_anes == 0, NA_real_, 0))),
                                   vote_Clinton_anes = if_else(vote.2016_anes == "Hillary Clinton", 1, (if_else(voted_2016_anes == 0, NA_real_, 0))),
                                   vote_other_anes = if_else(vote.2016_anes == "someone else", 1, (if_else(voted_2016_anes == 0, NA_real_, 0))),
                                   voted_2016_gpt3 = if_else(vote.2016_gpt3 == "Did Not Vote", 0, 1),
                                   vote_Trump_gpt3 = if_else(vote.2016_gpt3 == 2, 1, (if_else(voted_2016_gpt3 == 0, NA_real_, 0))),
                                   vote_Clinton_gpt3 = if_else(vote.2016_gpt3 == 1, 1, (if_else(voted_2016_gpt3 == 0, NA_real_, 0))),
                                   vote_other_gpt3 = if_else(vote.2016_gpt3 == 42, 1, (if_else(voted_2016_gpt3 == 0, NA_real_, 0))))


anesgpt3_sub_descriptive <- select(anesgpt3_sub_descriptive, -race_anes, -education_anes, -vote.2016_anes, -gender_anes, -race_gpt3, -vote.2016_gpt3, -gender_gpt3, -education_gpt3)

stargazer(anesgpt3_sub_descriptive,
          digits = 1,
          iqr = TRUE,
          type = "latex",
          style = "apsr",
          out = here::here(thisPath(),"Tables","Appendix_Table15.tex"),
          rownames = FALSE)

###############CODE TO CREATE APPENDIX FIGURE 8#############################

#Clean the data (renaming and reordering to make the graph prettier)
cramersv_compare2 <- unite(cramersv.long, var_x_y, variable.x, variable.y, sep = "_", remove = FALSE) %>%
  filter(source.x == source.y) %>%
  filter(variable.x != variable.y)

cramersv_compare2$variable.x <- factor(cramersv_compare2$variable.x, 
                                       levels = c("gender", "race", "age", "education",
                                                  "church.goer", "patriotism", "discuss.politics",
                                                  "political.interest", "vote.2016", "ideology", "pid7"))

cramersv_compare2$variable.y <- factor(cramersv_compare2$variable.y, 
                                       levels = c("gender", "race", "age", "education",
                                                  "church.goer", "patriotism", "discuss.politics",
                                                  "political.interest", "vote.2016", "ideology", "pid7"))

cramersv_compare2$source.x <- dplyr::recode(cramersv_compare2$source.x, "anes" = "Human")
cramersv_compare2$source.y <- dplyr::recode(cramersv_compare2$source.y, "anes" = "Human", "gpt3" = "GPT-3")
cramersv_compare2$source.y <- factor(cramersv_compare2$source.y, levels = c("GPT-3", "Human"))
cramersv_compare2 <- arrange(cramersv_compare2, desc(source.y))

variablelist <- c(vote.2016 = "2016 Vote", race = "Race / \n Ethnicity", political.interest = "Political \n Interest", pid7 = "Party ID", patriotism = "Patriotism", ideology = "Ideology", gender = "Gender", education =  "Education", discuss.politics = "Discusses \n Politics", church.goer = "Attends \n Church", age ="Age")

#Create the dot plot
appendix_fig8 <- ggplot(cramersv_compare2, aes(y = fct_rev(as_factor(variable.x)), x = cramersv)) + 
  geom_line(aes(group = variable.x)) +
  geom_point(aes(color = source.y, shape = source.y), size = 3) +
  facet_wrap(. ~ variable.y, nrow = 2, labeller = labeller(variable.y = variablelist)) + 
  scale_color_brewer(palette = "Paired") +
  scale_y_discrete(labels = c("Party ID", "Ideology", "2016 Vote", "Political \n Interest", "Discusses \n Politics", "Patriotism", "Attends \n Church", "Education", "Age", "Race / \n Ethnicity", "Gender"))+
  labs(y = "Background Variable (From the Same Source)", x= "Cramer's V", shape = "Source", color = "Source")+
  scale_shape_manual(values=c(19,15)) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.3)) 
ggsave(here::here(thisPath(),"Figures","Appendix_Figure8.pdf"), width=8, height=8,family="Times")


########################Code to create Appendix Table 16########################################

##First step - create the cramer's v matrix for the other two temperatures. 
#This code replicates what we've already done with the main analysis (which is at temperature .7), but at temperatures 1.0 and .001.

#read in the data
anesgpt3_10 <- read.csv(here::here(thisPath(),"Study3_Data","anesgpt3_task3_temp10.csv"))

##Clean the ANES Responses
anesgpt3_10 <- mutate(anesgpt3_10, race_anes = if_else(V161310x == 1, "white",
                                                       if_else(V161310x == 2, "black",
                                                               if_else(V161310x == 3, "asian",
                                                                       if_else(V161310x == 5, "hispanic", NA_character_)))),
                      discuss.politics_anes = if_else(V162174 == 1, "yes discuss politics",
                                                      if_else(V162174 == 2, "never discuss politics", NA_character_)),
                      ideology_anes = if_else(V161126 == 1, "extremely liberal", 
                                              if_else(V161126 == 2, "liberal",
                                                      if_else(V161126 == 3, "slightly liberal",
                                                              if_else(V161126 == 4, "moderate",
                                                                      if_else(V161126 == 5, "slightly conservative",
                                                                              if_else(V161126 == 6, "conservative",
                                                                                      if_else(V161126 == 7, "extremely conservative", NA_character_))))))),
                      pid7_anes = if_else(V161158x > 0, V161158x, NA_integer_),
                      church.goer_anes = if_else(V161244 > 0, V161244, NA_integer_),
                      age_anes = if_else(V161267 > 0, V161267, NA_integer_),
                      education_anes = if_else(V161270 >0 & V161270 <= 9, "High School or Less", 
                                               if_else(V161270 >= 10 & V161270 <= 12, "Some College / AA", 
                                                       if_else(V161270 == 13, "Bachelor's",
                                                               if_else(V161270 >=14 & V161270 <= 16, "Graduate Degree", NA_character_)))),
                      gender_anes = if_else(V161342 == 1 | V161342 == 2, V161342, NA_integer_),
                      voted.2016_anes = if_else(V162031x == 0, "no", if_else(V162031x == 1, "yes", NA_character_)),
                      votechoice.2016_anes = if_else(V162062x == 1, "Hillary Clinton", if_else(V162062x == 2, "Donald Trump", if_else(V162062x == 3 | V162062x == 4 | V162062x == 5, "someone else", NA_character_))),
                      political.interest_anes = if_else(V162256 > 0, V162256, NA_integer_),
                      patriotism_anes = if_else(V162125x > 0, V162125x, NA_integer_), 
                      vote.2016_anes = if_else(voted.2016_anes == "no", "Did Not Vote", votechoice.2016_anes))

#Clean the GPT-3 responses to match the ANES 
anesgpt3_10 <- mutate(anesgpt3_10, race_gpt3 = if_else(race_gpt3 == -1, NA_integer_, race_gpt3),
                      age_gpt3 = if_else(age_gpt3 == -1, NA_integer_, age_gpt3),
                      church.goer_gpt3 = if_else(church_goer_gpt3 == -1, NA_integer_, church_goer_gpt3),
                      discuss.politics_gpt3 = if_else(discuss_politics_gpt3 == -1, NA_integer_, discuss_politics_gpt3),
                      education_gpt3 = if_else(education_gpt3 == -1, NA_integer_, education_gpt3),
                      gender_gpt3 = if_else(gender_gpt3 == -1, NA_integer_, gender_gpt3),
                      ideology_gpt3 = if_else(ideology_gpt3 == -1, NA_integer_, ideology_gpt3),
                      patriotism_gpt3 = if_else(patriotism_gpt3 == -1, NA_integer_, patriotism_gpt3),
                      pid7_gpt3 = if_else(pid7_gpt3 == -1, NA_integer_, pid7_gpt3),
                      political.interest_gpt3 = if_else(political_interest_gpt3 == -1, NA_integer_, political_interest_gpt3),
                      votechoice.2016_gpt3 = if_else(votechoice_2016_gpt3 == -1, NA_integer_, votechoice_2016_gpt3),
                      voted.2016_gpt3 = if_else(voted_2016_gpt3 == -1, NA_integer_, voted_2016_gpt3),
                      vote.2016_gpt3 = if_else(voted_2016_gpt3 == 0, "Did Not Vote", as.character(votechoice.2016_gpt3)))

#Create a subset of the data that only includes complete cases for the relevant variables
anesgpt3_10_sub <- select(anesgpt3_10, age_gpt3, age_anes, church.goer_gpt3, church.goer_anes, discuss.politics_gpt3, discuss.politics_anes, race_gpt3, race_anes, education_gpt3, education_anes, gender_gpt3, gender_anes, ideology_gpt3, ideology_anes, patriotism_gpt3, patriotism_anes, pid7_gpt3, pid7_anes, political.interest_gpt3, political.interest_anes, vote.2016_gpt3, vote.2016_anes)
anesgpt3_10_sub <- anesgpt3_10_sub[complete.cases(anesgpt3_10_sub), ]

#Calculate a matrix of Cramer's V values
cramersv.1_10 <- corrplot.mixed(PairApply(anesgpt3_10_sub, CramerV), is.corr = F, lower.col = "black", tl.col = "navy", upper = "circle", order = "hclust", tl.cex = .9, number.cex = .8, upper.col = brewer.pal(n = 10, name = "PuOr"))

#Reformat the Cramer's V output for ggplot
cramersv.long_10 <- as.data.frame(cramersv.1_10$corr) %>%
  rownames_to_column("names.x") %>%
  pivot_longer(cols = !names.x, names_to = "names.y", values_to = "cramersv") %>%
  separate(col = names.x, into = c("variable.x", "source.x"), sep = "_", remove = FALSE) %>%
  separate(col = names.y, into = c("variable.y", "source.y"), sep = "_", remove = FALSE)


#Clean the data (renaming and reordering to make the graph prettier)
cramersv_compare_10 <- unite(cramersv.long_10, var_x_y, variable.x, variable.y, sep = "_", remove = FALSE) %>%
  filter(source.x == "anes") %>%
  filter(variable.x != variable.y)

#Separate GPT3 and ANES results, and clean up headers
cramersv_gpt3_10 <- filter(cramersv_compare_10, source.y == "gpt3")
cramersv_anes_10 <- filter(cramersv_compare_10, source.y == "anes")

cramersv_gpt3_10 <- select(cramersv_gpt3_10, variable.x, variable.y, cramersv)
cramersv_gpt3_10 <- rename(cramersv_gpt3_10, cramersv.gpt3 = cramersv)

cramersv_anes_10 <- select(cramersv_anes_10, variable.x, variable.y, cramersv)
cramersv_anes_10 <- rename(cramersv_anes_10, cramersv.anes = cramersv)

#Join tables back together
cramersv_table_10 <- left_join(cramersv_gpt3_10, cramersv_anes)

#Round to two digits
cramersv_table_10[,-c(1:2)] <-round(cramersv_table_10[,-c(1:2)],2) #the "-1" excludes column 1

#Calculate the difference
cramersv_table_10 <- mutate(cramersv_table_10, cramersv_diff = cramersv.anes - cramersv.gpt3)

###Now we switch to temperature .001###

#read in the data
anesgpt3_001 <- read.csv(here::here(thisPath(),"Study3_Data","anesgpt3_task3_temp001.csv"))

##Clean the ANES Responses
anesgpt3_001 <- mutate(anesgpt3_001, race_anes = if_else(V161310x == 1, "white",
                                                       if_else(V161310x == 2, "black",
                                                               if_else(V161310x == 3, "asian",
                                                                       if_else(V161310x == 5, "hispanic", NA_character_)))),
                      discuss.politics_anes = if_else(V162174 == 1, "yes discuss politics",
                                                      if_else(V162174 == 2, "never discuss politics", NA_character_)),
                      ideology_anes = if_else(V161126 == 1, "extremely liberal", 
                                              if_else(V161126 == 2, "liberal",
                                                      if_else(V161126 == 3, "slightly liberal",
                                                              if_else(V161126 == 4, "moderate",
                                                                      if_else(V161126 == 5, "slightly conservative",
                                                                              if_else(V161126 == 6, "conservative",
                                                                                      if_else(V161126 == 7, "extremely conservative", NA_character_))))))),
                      pid7_anes = if_else(V161158x > 0, V161158x, NA_integer_),
                      church.goer_anes = if_else(V161244 > 0, V161244, NA_integer_),
                      age_anes = if_else(V161267 > 0, V161267, NA_integer_),
                      education_anes = if_else(V161270 >0 & V161270 <= 9, "High School or Less", 
                                               if_else(V161270 >= 10 & V161270 <= 12, "Some College / AA", 
                                                       if_else(V161270 == 13, "Bachelor's",
                                                               if_else(V161270 >=14 & V161270 <= 16, "Graduate Degree", NA_character_)))),
                      gender_anes = if_else(V161342 == 1 | V161342 == 2, V161342, NA_integer_),
                      voted.2016_anes = if_else(V162031x == 0, "no", if_else(V162031x == 1, "yes", NA_character_)),
                      votechoice.2016_anes = if_else(V162062x == 1, "Hillary Clinton", if_else(V162062x == 2, "Donald Trump", if_else(V162062x == 3 | V162062x == 4 | V162062x == 5, "someone else", NA_character_))),
                      political.interest_anes = if_else(V162256 > 0, V162256, NA_integer_),
                      patriotism_anes = if_else(V162125x > 0, V162125x, NA_integer_), 
                      vote.2016_anes = if_else(voted.2016_anes == "no", "Did Not Vote", votechoice.2016_anes))

#Clean the GPT-3 responses to match the ANES 
anesgpt3_001 <- mutate(anesgpt3_001, race_gpt3 = if_else(race_gpt3 == -1, NA_integer_, race_gpt3),
                      age_gpt3 = if_else(age_gpt3 == -1, NA_integer_, age_gpt3),
                      church.goer_gpt3 = if_else(church_goer_gpt3 == -1, NA_integer_, church_goer_gpt3),
                      discuss.politics_gpt3 = if_else(discuss_politics_gpt3 == -1, NA_integer_, discuss_politics_gpt3),
                      education_gpt3 = if_else(education_gpt3 == -1, NA_integer_, education_gpt3),
                      gender_gpt3 = if_else(gender_gpt3 == -1, NA_integer_, gender_gpt3),
                      ideology_gpt3 = if_else(ideology_gpt3 == -1, NA_integer_, ideology_gpt3),
                      patriotism_gpt3 = if_else(patriotism_gpt3 == -1, NA_integer_, patriotism_gpt3),
                      pid7_gpt3 = if_else(pid7_gpt3 == -1, NA_integer_, pid7_gpt3),
                      political.interest_gpt3 = if_else(political_interest_gpt3 == -1, NA_integer_, political_interest_gpt3),
                      votechoice.2016_gpt3 = if_else(votechoice_2016_gpt3 == -1, NA_integer_, votechoice_2016_gpt3),
                      voted.2016_gpt3 = if_else(voted_2016_gpt3 == -1, NA_integer_, voted_2016_gpt3),
                      vote.2016_gpt3 = if_else(voted_2016_gpt3 == 0, "Did Not Vote", as.character(votechoice.2016_gpt3)))

#Create a subset of the data that only includes complete cases for the relevant variables
anesgpt3_001_sub <- select(anesgpt3_001, age_gpt3, age_anes, church.goer_gpt3, church.goer_anes, discuss.politics_gpt3, discuss.politics_anes, race_gpt3, race_anes, education_gpt3, education_anes, gender_gpt3, gender_anes, ideology_gpt3, ideology_anes, patriotism_gpt3, patriotism_anes, pid7_gpt3, pid7_anes, political.interest_gpt3, political.interest_anes, vote.2016_gpt3, vote.2016_anes)
anesgpt3_001_sub <- anesgpt3_001_sub[complete.cases(anesgpt3_001_sub), ]

#Have to drop GPT-3 race from the analysis, because there is no variation in GPT-3's predictions
anesgpt3_001_sub <- select(anesgpt3_001_sub, -race_gpt3)

#Calculate a matrix of Cramer's V values
cramersv.1_001 <- corrplot.mixed(PairApply(anesgpt3_001_sub, CramerV), is.corr = F, lower.col = "black", tl.col = "navy", upper = "circle", order = "hclust", tl.cex = .9, number.cex = .8, upper.col = brewer.pal(n = 10, name = "PuOr"))

#Reformat the Cramer's V output for ggplot
cramersv.long_001 <- as.data.frame(cramersv.1_001$corr) %>%
  rownames_to_column("names.x") %>%
  pivot_longer(cols = !names.x, names_to = "names.y", values_to = "cramersv") %>%
  separate(col = names.x, into = c("variable.x", "source.x"), sep = "_", remove = FALSE) %>%
  separate(col = names.y, into = c("variable.y", "source.y"), sep = "_", remove = FALSE)


#Clean the data (renaming and reordering to make the graph prettier)
cramersv_compare_001 <- unite(cramersv.long_001, var_x_y, variable.x, variable.y, sep = "_", remove = FALSE) %>%
  filter(source.x == "anes") %>%
  filter(variable.x != variable.y)

#Separate GPT3 and ANES results, and clean up headers
cramersv_gpt3_001 <- filter(cramersv_compare_001, source.y == "gpt3")
cramersv_anes_001 <- filter(cramersv_compare_001, source.y == "anes")

cramersv_gpt3_001 <- select(cramersv_gpt3_001, variable.x, variable.y, cramersv)
cramersv_gpt3_001 <- rename(cramersv_gpt3_001, cramersv.gpt3 = cramersv)

cramersv_anes_001 <- select(cramersv_anes_001, variable.x, variable.y, cramersv)
cramersv_anes_001 <- rename(cramersv_anes_001, cramersv.anes = cramersv)

#Join tables back together
cramersv_table_001 <- left_join(cramersv_gpt3_001, cramersv_anes)

#Round to two digits
cramersv_table_001[,-c(1:2)] <-round(cramersv_table_001[,-c(1:2)],2) #the "-1" excludes column 1

#Calculate the difference
cramersv_table_001 <- mutate(cramersv_table_001, cramersv_diff = cramersv.anes - cramersv.gpt3)


######Calculate the summary statistics to complete Appendix Tale 16:

#For .001 temperature
table16_001 <- as.data.frame(rbind(
mean(cramersv_table_001$cramersv_diff),
min(cramersv_table_001$cramersv_diff),
max(cramersv_table_001$cramersv_diff),
sd(cramersv_table_001$cramersv_diff),
nrow(anesgpt3_001_sub)
))

#For .7 temperature
table16_7 <- as.data.frame(rbind(
mean(cramersv_table$cramersv_diff),
min(cramersv_table$cramersv_diff),
max(cramersv_table$cramersv_diff),
sd(cramersv_table$cramersv_diff),
nrow(anesgpt3_sub)
))

#For 1.0 temperature
table16_1 <- as.data.frame(rbind(
mean(cramersv_table_10$cramersv_diff),
min(cramersv_table_10$cramersv_diff),
max(cramersv_table_10$cramersv_diff),
sd(cramersv_table_10$cramersv_diff),
nrow(anesgpt3_10_sub)
))

table16 <- as.data.frame(cbind(Stat = c("Mean","Min","Max","SD","n"), table16_001, table16_7, table16_1))
colnames(table16) <- c("Stat", "Temp=.001", "Temp=0.7", "Temp=1")
table16_out <- xtable(table16)
print.xtable(table16_out, type="latex", file=here::here(thisPath(),"Tables","Appendix_Table16.tex"))


