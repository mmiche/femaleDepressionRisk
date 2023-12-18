# femaleDepressionRisk

## Download this complete folder
Click on the green button 'Code', then in the open window, click on 'Download ZIP'. After successful download, unzip the folder. Within the unzipped folder, use the folder R as your target directory.

## Produce total sample size
Download 10 NSDUH datasets (years 2012-2021) from this [this website](https://www.datafiles.samhsa.gov/dataset/national-survey-drug-use-and-health-2021-nsduh-2021-ds0001). Select the data format .RData. Then, open the R script makeNSDUHDataset.R and follow the instructions therein. The complete 10-year NSDUH dataset will contain 539'394 individuals, of whom 139'643 are younger than 18 years. They will be removed in the script nsduhMain.R, because for them the outcome variable (diagnosis of a lifetime major depressive episode) is not available (remaining total sample size: 399'751). Save the complete dataset as **nsduh1221.rds**.

## Check
In the target directory R on your computer, there should be 27 R scripts and 3 rds files: nsduh1221, collectRisklme4Ls, and rangeEstimatesLs. Correct? Please check, then continue.

## Only 3 R scripts
Only 3 R scripts need to be open, if you already possess the full 10-year NSDUH dataset. The remaining R scripts are used automatically, by executing their code from within these 3 R scripts:

### nsduhStart.R
Load the packages and set the complete path to the target directory **R**. See instructions on how to set the complete path, having either a Mac or a Windows computer (problems mostly have to do with these two symbols: \ and /.

### nsduhMain.R
Run all analyses and save the collected results as rds files. The collected results will be saved as rds files, so that the results do not have to be computed over and over again, every time you want to do something with the results, e.g., visualize them differently. Computing all results will take *approximately 15 minutes*.
Collected results are saved in nsduhMain.R, script lines 123-130, 183-184, and 264-268. One more collected result is saved in nsduhResults.R, script lines 739-740. After they have been saved, I recommend to outcomment these script lines (put the sharp (#) symbol at beginning of each of these lines).

### nsduhResults.R
When all collected results have been saved as rds files, they will be in the target directory **R**. In this nsduhResults.R script, the rds files (the results) are read and used for further purposes, i.e., displaying them in the R console, computing new results, e.g., convert p values to S values, and visualizing them.

## Comments
Always read the comments in the R scripts. They are there to explain or remind what the R code does and what purpose it serves. I will add some comments in the R scripts at places that require comments but so far have none; the updated version will be uploaded to this GitHub repository.

## 2 choices
Two choices **by you** are necessary (see nsduhResults.R, script lines 20 and 22):

### Script line 20
From 15 available results (risk ratio, risk difference, odds ratio), select one, i.e., select a number between 1 and 15.

### Script line 22
From 9 available results (hazard ratio), select one, i.e., select a number between 1 and 9.
