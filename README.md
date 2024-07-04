# femaleDepressionRisk

## Download this complete folder
Click on the green button 'Code', then in the open window, click on 'Download ZIP'. After successful download, unzip the folder. Within the unzipped folder, use the folder **R** as your target directory.

## Produce total sample size
Download 10 NSDUH datasets (years 2012-2021) from this [this website](https://www.datafiles.samhsa.gov/dataset/national-survey-drug-use-and-health-2021-nsduh-2021-ds0001). Select the data format .RData. Then, open the R script makeNSDUHDataset.R and follow the instructions therein. The complete 10-year NSDUH dataset will contain 539'394 individuals, of whom 139'643 are younger than 18 years. They will be removed in the script nsduhMain.R, because for them the outcome variable (diagnosis of a lifetime major depressive episode) is not available (remaining total sample size: 399'751). Save the complete dataset as **nsduh1221.rds** and put it in the target directory **R**.

## Check
In the target directory R on your computer, there should be 30 R scripts and 13 rds files. Correct? Please check, then continue.

## Tasks
For the tasks of the 30 R scripts, please see Appendix A of the Supplementary Document. The rds files are mostly saved output from the R scripts, in order to not have to run the code every time.
