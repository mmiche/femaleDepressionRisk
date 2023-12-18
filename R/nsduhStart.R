# Load all required packages.
# BEWARE: A package must first be installed, before it can be loaded. How to install a package in R? Using 'DescTools' and 'survival' as exemplary packages, install them like this:
# install.packages(c("DescTools", "survival"), dependencies=TRUE)
library(pwrss)
library(powerSurvEpi)
library(DescTools)
library(survival)
library(lme4)
library(rstatix)
library(brms)
library(coda)
library(meta)
library(tidyverse)
#
# Instruction:
# 1. Make a new directory (synonym: folder) on your computer.
# 2. Put all 27 R scripts into this new directory.
# 3. Put the complete dataset (N = 399'751) into this new directory.
# > See R script 'makeNSDUHData.R', to generate this complete dataset, after having downloaded all 10 datasets from the years 2012-2021 from this URL (use the 'Choose Year' drop-down menu):
# https://www.datafiles.samhsa.gov/dataset/national-survey-drug-use-and-health-2021-nsduh-2021-ds0001
# 4. Set the complete path to this new directory on your computer. Set it here:
nsduhPath <- "/Users/mmiche/Desktop/TeachingClass/HS2023/SeminarOpenScience/PsyArXivProjekt/R/"
#
# When initially running this code, you should save the collected results, so that you do not have to recompute the results time after time again. Therefore, remove the sharp (#) symbols in the script nsduhMain.R, in script lines 123-130, 183-184, and 264-268.
# --------------------------------------------------
#
# How to find, copy, and paste the complete path to a directory?
# Mac:
# ---
# 1. Open the target directory.
# 2. Right click.
# 3. In open window, keep 'option' key pressed.
# 4. Click 'directory copy as path name'.
# 5. Paste the copied path name in this script's line 22.
#
# Windows:
# -------
# 1. Go into the target directory.
# 2. Right click to copy the directory path into the computer's clipboard.
# 3. Then, execute this command: readClipboard()
# 4. Copy the complete path from the R console.
# 5. Paste the copied path name in this script's line 22.
# --------------------------------------------------