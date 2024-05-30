# Data: National Survey on Drug Use and Health (NSDUH) 2012-2021
# --------------------------------------------------------------
#
# Generate the total NSDUH dataset (539'394; adults: 399'751)
#
# # -------------------------------------------------------
#
# # Download of all 10 datasets: 2023-10-25 from SAMHSA's website:
# # https://www.datafiles.samhsa.gov/dataset/national-survey-drug-use-and-health-2021-nsduh-2021-ds0001
#
# Set complete path to where the 10 downloaded datasets are located on your computer. There should be no objects in this directory, except for the 10 datasets (see script line 14, command 'list.files()').
# setwd("completePathToWhereTheDownloadedDatasetsAreOnYourComputer")
setwd("/Users/mmiche/Desktop/TeachingClass/HS2023/SeminarOpenScience/PsyArXivProjekt/NSDUHData/")
nsduhDatasets <- list.files()
years <- 2012:2021

# Empty list, with which to collect the 10 datasets of the years
# 2012-2021
nsduhLs <- list()
start <- Sys.time()
for(i in 1:length(nsduhDatasets)) {
    # Load the data (format .RData)
    load(nsduhDatasets[i])
    # NSDUH original datasets, when loaded in R, start with "PUF"
    # (years 2012-2019; 2021) or with NSDUH (year 2020).
    # Search for current dataset in the workspace.
    datTmpName <- ls()[grepl("PUF", ls()) | grepl("NSDUH", ls())]
    # Assign the dataset to a variable name (Tmp = temporary)
    datTmp <- get(datTmpName)
    # Select three columns: amdelt, irsex, catage
    # amdelt: adult major depressive episode lifetime
    # irsex: 1 = male, 2 = female
    # catage: 1 = 12-17 yrs, 2 = 18-25, 3 = 26-34, 4 = 35+
    datTmp <- datTmp[,c("amdelt", "irsex", "catage")]
    # Add column with the year of the survey
    datTmp$year <- years[i]
    # Add the dataset to the list.
    nsduhLs[[nsduhDatasets[i]]] <- datTmp
    rm(list=datTmpName)
    print(years[i])
}
end <- Sys.time()
difftime(end, start) # 5.654423 mins
#
names(nsduhLs)
lapply(nsduhLs, dim)

# Combine, then save locally as rds file
nsduhAll <- dplyr::bind_rows(nsduhLs)
dim(nsduhAll)
#
rownames(nsduhAll) <- 1:nrow(nsduhAll)
#
head(nsduhAll)
# 
# # saveRDS(nsduhAll, file="nsduh1221.rds")
# # -----------------------------------------