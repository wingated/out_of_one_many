
This document describes the replication files and the process to follow to replicate results in both the main paper and the appendix of Argyle et al.'s "Out of One, Many: Using Language Models to Simulate Human Samples."

Users will need both R and Python installed on their computers. All R scripts were run in RStudio version 2022.07.0 using version 4.2.1 of R and the following versions of each of the these packages:

data.table: 1.14.2
readr: 2.1.3
car: 3.1-0
vcd: 1.4-10
rstatix: 0.7.0
fixest: 0.10.4
psych: 2.2.9
ggplot2: 3.3.6
stringi: 1.7.8
plyr: 1.8.7
tidyverse: 1.3.2
correlation: 0.8.3
corrplot: 0.92
DescTools: 0.99.46
RColorBrewer: 1.1-3
stargazer: 5.2.3
rstudioapi: 0.14
here: 1.0.1
reticulate: 1.26
xtable: 1.8-4

These packages can be installed in R by running the included "InstallRPackages.R" script.

All Python scripts were run on version 3.11 after installing the following versions of these additional packages; we used pip to install these: 

seaborn: 0.12.1
matplotlib: 3.6.0
numpy: 1.23.4
pandas: 1.5.1
tqdm: 4.64.1
sklearn: 0.0
 

The R and python scripts for each study were run on a 2018 15" MacBook Pro with a 2.9 GHz 6-Core Intel Core i9 processor and 16 GB of memory. These are not computationally taxing scripts: in total, it took less than 40 seconds to run all R and Python scripts on this machine (a combined 6 scripts -- see "MasterScript.R").

The paper contains 3 studies. Our replication files include the following scripts to generate results for each:

Study1DataGen.R
Study1Analysis.R
Study1Python.py

Study2Analysis.R
Study2Python.py

Study3Analysis.R

These scripts call raw data for each study found in the _Data folders: "Study1_Data," "Study2_Data," and "Study3_Data". They generate raw versions of the tables and figures found in both the paper and the appendix; these tables and figures can be located in the "Tables" and "Figures" folders. This is true with one exception: the R script "Study2Analysis.R" generates all the numbers for the tables and figures associated with this study (Table 1 in the paper; figure 4 and tables 7-9 in the appendix), which we then inputted by hand in LaTeX to populate these tables and figures. So, these tables and figures do not appear in our replication folders.

We post-processed all tables and figures in the paper and appendix, making formatting changes to the tables in LaTeX (we used Overleaf for collaborative LaTeX use) and using Adobe Illustrator to improve the figures. Thus, while the code in these scripts was used to initially generate the tables and figures, it does not produce tables and figures as clean as the final products presented in the paper and appendix. The "AllTables.tex" and "AllTables.pdf" documents in the "Tables" folder combine all the raw tables into one document.

While each of these R and Python files can be run individually, they (along with the "InstallRPackages.R" script) can also be run at once from the "MasterScript.R" file. This includes the Python scripts, which we source from R using the "reticulate" package.

Our replication files also include the code used to generate study data from GPT-3 (and, for the ablation analysis in the appendix, the other LLMs) for all three studies. This is located in the file, "GPT3_OtherModels_DataGenerationCode.pdf".

The text of the survey administered in Study 1 can be found in the appendix to the main paper.

---------------

A brief description of the data in the three _Data folders:


* Study1_Data (called by the Study1DataGen.R, Study1Analysis.R, Study1Python.py scripts)
1. Background_data_to_merge.csv: Contains the demographic information from participants in the original "Pigeonholing Partisans" dataset.

2. Lucid_Coding_4Feb.csv: The raw data from the Lucid/Qualtrics survey described in the paper

3. Lucid_FullData_4Feb_with_background.csv: The merging of datasets 1 and 2 (the merging happens in the file "Study1DataGen.R").

4. gpt3_uber_final.csv: A datafile that contains the word lists outputted by GPT-3 to compare with the human lists in the "Pigeonholing Partisans" data

5. ppfull.csv: The human "Pigeonholing Partisans" dataset

6. fig3_data.py: a small python file with the data (generated in "Study1Analysis.R") to make Figure 3 in the paper (which is created by our "Study1Python.py" script)


* Study 2_Data (called by the Study2Analysis.R and Study2Python.py scripts):
1. full_results_2012_2.csv: Cleaned data from the 2012 ANES and the corresponding predictions from GPT-3 
2. full_results_2016_2.csv: Cleaned data from the 2016 ANES and the corresponding predictions from GPT-3 
3. full_results_2020_2.csv: Cleaned data from the 2020 ANES and the corresponding predictions from GPT-3 

4. ablataion_data.pkl: Data of results from the ablation analysis; used to generate Appendix Figure 5 

5. anes2012 (directory): Contains the test results for the ANES 2012 data when run on other large language models. The individual results files are in the following format: `ds_exp_results_[MODEL_NAME]_[DATE]_processed.pkl` where MODEL_NAME is the name of the large language model and DATE is the day the experiment was run. Used to produce Appendix Figure 6. 

6. anes2016 (directory): Contains the test results for the ANES 2016 data when run on other large language models. The individual results files are in the following format: `ds_exp_results_[MODEL_NAME]_[DATE]_processed.pkl` where MODEL_NAME is the name of the large language model and DATE is the day the experiment was run. Used to produce Appendix Figure 6.

7. anes2020 (directory): Contains the test results for the ANES 2020 data when run on other large language models. The individual results files are in the following format: `ds_exp_results_[MODEL_NAME]_[DATE]_processed.pkl` where MODEL_NAME is the name of the large language model and DATE is the day the experiment was run. Used to produce Appendix Figure 6.


* Study3_Data (called by the Study3Analysis.R script):

1. anesgpt3_task3.csv: Individual-level data from a subset of questions in the ANES (the relevant data used in the generation of the silicon sample and subsequent analysis), and the GPT-3 produced value for each of the corresponding questions.  This is the data file used for all main analysis in the paper.
2-3. anesgpt3_task3_temp001.csv and anesgpt3_task3_temp10.csv: robustness-check files, in which the GPT-3 data-generation process was replicated using two different temperature settings. These files are called at the very end of the "Study3Analysis.R" script, and only used for replication of Appendix Table 16.

