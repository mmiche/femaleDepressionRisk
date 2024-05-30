# This script shows how we generated Table 1 of the main document.

library(readxl)

colNames0 <- readxl::read_excel(path="nsduhDemographics.xlsx")

colNames <- colNames0[,c("Age", "Sex", "Health", "Employment", "Income", "Education", "Marital status", "MilitaryC", "MilitaryE", "AgeCategory", "Depression", "SexIdent")]

years <- paste0("year", 2012:2021)
yearsNum <- 2012:2021

nsduhDatasets <- c("NSDUH_2012.RData", "NSDUH_2013.RData", "NSDUH_2014.RData", "NSDUH_2015.RData", "NSDUH_2016.RData", "NSDUH_2017.RData", "NSDUH_2018.RData", "NSDUH_2019.RData", "NSDUH_2020.Rdata", "NSDUH_2021.RData")

# Empty list to collect the 10 datasets of the years 2012-2021
nsduhLs <- list()
# i <- 1
start <- Sys.time()
demoDataLs <- list()
for(i in 1:length(nsduhDatasets)) {
    # Load the data (format .RData)
    load(nsduhDatasets[i])
    # NSDUH datasets, when loaded in R, start with "PUF" (years 2012-2019; 2021) or with NSDUH (year 2020).
    # Search for current dataset in the workspace.
    datTmpName <- ls()[grepl("PUF", ls()) | grepl("NSDUH", ls())]
    print(datTmpName)
    # Assign the dataset to a variable name (Tmp = temporary)
    datTmp0 <- get(datTmpName)
    rm(list=datTmpName)

    colNamesTmp0 <- colNames[i,]
    colNamesTmp <- colNamesTmp0[,!grepl("na", colNamesTmp0)]
    colNames_i <- as.character(colNamesTmp)
    
    datTmp <- datTmp0[,colNames_i]
    dim(datTmp)
    colnames(datTmp)
    
    # Surveys 2012-2014 does not seem to have this sexual identity variable.
    if(i <= 3) {
        datTmp$SexIdent <- NA
    }
    
    datTmp$year <- yearsNum[i]
    
    demoDataLs[[years[i]]] <- datTmp
    
    print(years[i])
}
end <- Sys.time()
difftime(end, start) # 4.7 mins

# saveRDS(object=demoDataLs, file="nsduhDemographics.rds")

nsduhDem <- readRDS(file="nsduhDemographics.rds")
years <- paste0("year", 2012:2021)

nsduhLs2 <- list()
# i <- 1
for(i in 1:length(nsduhDem)) {
    
    demoLs <- list()
    # j <- 10
    for(j in 1:(ncol(nsduhDem[[i]])-1)) {
        if(all(is.na(nsduhDem[[i]][,j]))) {
            next
        }
        tblTmp0 <- table(nsduhDem[[i]][,j])
        tblTmp <- addmargins(tblTmp0)
        ptblTmp <- addmargins(prop.table(tblTmp0)*100)
        # dfTmp: Resulting data.frame with number of participants per category (column count) as well as the corresponding percentage (column perc).
        dfTmp <- data.frame(count=as.numeric(tblTmp),
                            perc=round(as.numeric(ptblTmp), 3))
        # Add answer options as separate column (column Answers)
        dfTmp$Answers <- names(tblTmp)
        # Rearrange order of the columns of dfTmp.
        colNameTmp <- colnames(nsduhDem[[i]])[j]
        demoLs[[colNameTmp]] <- dfTmp[,c(3,1,2)]
    }
    
    # Add demoLs to the overall nsduh list.
    nsduhLs2[[years[i]]] <- demoLs
    
}

nsduhLs3 <- 
    lapply(nsduhLs2, FUN = function(x) {
        lapply(x, nrow)
    })

# i <- 1
tblRows <- c()
for(i in 1:length(nsduhLs3)) {
    if(i <= 3) {
        tblRows <- c(tblRows, as.numeric(unlist(nsduhLs3[[i]]))-1, 0)
    } else {
        tblRows <- c(tblRows, as.numeric(unlist(nsduhLs3[[i]]))-1)
    }
}
checkDf <- data.frame(matrix(data=tblRows, nrow=10, byrow = TRUE))
colnames(checkDf) <- c("Age", "Sex", "Health", "Employment", "Income", "Education", "Marital status", "MilitaryC", "MilitaryE", "AgeCat", "Depression", "SexIdent")
rownames(checkDf) <- 2012:2021
checkDf

# AGE2
# 2012, 2013, 2014, 2015, 2016, 2017,
# 2018, 2019, 2020
age2Df <- data.frame(
    code=1:17,
    meaning=c(paste(12:21, "years old"),
              "22 or 23 years old",
              "24 or 25 years old",
              "between 26 and 29 years old",
              "between 30 and 34 years old",
              "between 35 and 49 years old",
              "between 50 and 64 years old",
              "65 years old or older"))

# AGE3
# 2021
age3Df <- data.frame(
    code=1:11,
    meaning=c("12 or 13 years old",
              "14 or 15 years old",
              "16 or 17 years old",
              "between 18 and 20 years old",
              "between 21 and 23 years old",
              "24 or 25 years old",
              "between 26 and 29 years old",
              "between 30 and 34 years old",
              "between 35 and 49 years old",
              "between 50 and 64 years old",
              "65 years old or older"))

age2ToAge3 <- function(age2=NULL) {
    age2Out <- age2
    # 12-13 years
    age2Out[age2==2] <- 1
    # 14-15 years
    age2Out[age2==3 | age2==4] <- 2
    # 16-17 years
    age2Out[age2==5 | age2==6] <- 3
    # 18-20 years
    age2Out[age2==7 | age2==8 | age2==9] <- 4
    # 21-23 years
    age2Out[age2==10 | age2==11] <- 5
    # 24-25 years
    age2Out[age2==12] <- 6
    # age 26-65 or older
    age2Out[age2>12] <- age2[age2>12] - 6
    # Return transformed age2 variable.
    return(age2Out)
}


# i <- 1
for(i in 1:length(nsduhDem)) {
    if(i <= 3) {
        nsduhDem[[i]]$AGE2 <- age2ToAge3(nsduhDem[[i]]$AGE2)
        colnames(nsduhDem[[i]]) <- c("Age", "Sex", "Health", "Employment", "Income", "Education", "Marital status", "MilitaryC", "MilitaryE", "AgeCat", "Depression", "SexIdent", "Year")
    } else if(i <= 9) {
        nsduhDem[[i]]$AGE2 <- age2ToAge3(nsduhDem[[i]]$AGE2)
        colnames(nsduhDem[[i]]) <- c("Age", "Sex", "Health", "Employment", "Income", "Education", "Marital status", "MilitaryC", "MilitaryE", "AgeCat", "Depression", "SexIdent", "Year")
    } else {
        colnames(nsduhDem[[i]]) <- c("Age", "Sex", "Health", "Employment", "Income", "Education", "Marital status", "MilitaryC", "MilitaryE", "AgeCat", "Depression", "SexIdent", "Year")
    }
}

lapply(nsduhDem, dim)

demoTotal0 <- dplyr::bind_rows(nsduhDem)
rownames(demoTotal0) <- 1:nrow(demoTotal0)
dim(demoTotal0)

# Two checks
# Check 1: All with demoTotal0$Age <= 3 are 12-17 year-olds, therefore, all must be AgeCat (age category) = 1.
length(which(demoTotal0$Age<=3)) # 133464
all(demoTotal0[demoTotal0$Age<=3,"AgeCat"]==1)
# Check 2: None of demoTotal0$Age > 3 may belong to AgeCat = 1.
any(demoTotal0[demoTotal0$Age>3,"AgeCat"]==1)

addmargins(table(demoTotal0$Depression))
addmargins(prop.table(table(demoTotal0$Depression)))
# In total, 139'643 do not have a diagnosis of lifetime major depressive episode. This includes all 12-17 year-olds, but also adults.
length(which(is.na(demoTotal0$Depression)))

# Adults with a missing diagnosis of lifetime major depressive episode.
idxAdultDeprMissing <- is.na(demoTotal0$Depression) & demoTotal0$Age>3

# Regarding age categories:

# Distribution across the adult age categories. Sum = 6179
addmargins(table(demoTotal0$Age[idxAdultDeprMissing]))
# Percentage within each adult age category.
numeratorAge <- as.numeric(table(demoTotal0$Age[idxAdultDeprMissing]))
denominatorAge <- as.numeric(table(demoTotal0$Age[demoTotal0$Age>3]))
numeratorAge/(numeratorAge+denominatorAge)*100

# Regarding sex:

# Distribution across sex
addmargins(table(demoTotal0$Sex[idxAdultDeprMissing]))
# Percentage within each adult age category.
numeratorSex <- as.numeric(table(demoTotal0$Sex[idxAdultDeprMissing]))
denominatorSex <- as.numeric(table(demoTotal0$Sex[demoTotal0$Age>3]))
numeratorSex/(numeratorSex+denominatorSex)*100


# Missing AMDELT in total (12-17 year-olds and adults)
sum(133464, 6179) # 139643
# Check (complete total sample size)
sum(139643, 399751) # 539394

# Remove all without a diagnosis of lifetime major depressive episode.
demoTotal <- demoTotal0[!is.na(demoTotal0$Depression),]
# Sample where every individual either has or does not have a diagnosis of lifetime major depressive episode.
dim(demoTotal) # 399'751

demoTotalLs <- list()
for(i in 1:(ncol(demoTotal)-1)) {
    
    tblTmp0 <- table(demoTotal[,i])
    # tblTmp <- addmargins(tblTmp0)
    # ptblTmp <- addmargins(prop.table(tblTmp0)*100)
    tblTmp <- tblTmp0
    ptblTmp <- prop.table(tblTmp0)*100
    # dfTmp: Resulting data.frame with number of participants per category (column count) as well as the corresponding percentage (column perc).
    # dfTmp <- data.frame(count=as.numeric(tblTmp),
    #                     perc=round(as.numeric(ptblTmp), 3))
    dfTmp <- data.frame(count=as.numeric(tblTmp),
                        perc=as.numeric(ptblTmp))
    # Add answer options as separate column (column Answers)
    dfTmp$Answers <- names(tblTmp)
    # Rearrange order of the columns of dfTmp.
    colNameTmp <- colnames(demoTotal)[i]
    demoTotalLs[[colNameTmp]] <- dfTmp[,c(3,1,2)]
    
}

# SexIdent = 115'978 missing values, 283'773 self-reported answers.
length(which(is.na(demoTotal$SexIdent))) + 283773

# Age (transformed to how AGE3 is categorized in nsduh 2021)
age3AdultDf <- age3Df[4:11,]

# SEX
# 2012-2021
irsexDf <- data.frame(
    code=1:2,
    meaning=c("male", "female")
)

# health
# 2012-2021
healthDf <- data.frame(
    code=c(1:5, 94, 97, 98),
    meaning=c("Excellent", "Very good",
              "Good", "Fair", "Poor",
              "Don't know", "Refused", "Blank (no answer)")
)

# Employment
# 2012-2021
employmentDf <- data.frame(
    code=c(1:4, 99),
    meaning=c("Employed full time", "Employed part time",
              "Unemployed", "Other (incl. not in labor force)",
              "12-14 years old")
)

# Income
# 2012-2021
incomeDf <- data.frame(
    code=1:4,
    meaning=c("Less than 20'000$", "20'000-49'999$",
              "50'000-74'999$", "75'000$ or more")
)

# Education
# 2012-2021
educationDf <- data.frame(
    code=1:11,
    meaning=c("Fifth grade or less",
              "Sixth grade",
              "Seventh grade",
              "Eighth grade",
              "Ninth grade",
              "Tenth grade",
              "Eleventh grade",
              "Twelfth grade",
              "Freshman/13th year",
              "Sophomore/14th year or Junior/15th year",
              "Senior/16th year or Grad/Prof School (or higher)")
)

# Marital status
# 2012-2021
maritalStatusDf <- data.frame(
    code=1:5,
    meaning=c("Married", "Widowed", "Divorced or Separated",
              "Never Been Married",
              "LEGITIMATE SKIP Respondent is <= 14 years old")
)


# Military service ever (MilitaryE)
# 2012-2021
serviceDf <- data.frame(
    code=c(1, 2, 85, 89, 94, 97, 98, 99),
    meaning=c("Yes", "No",
              "BAD DATA Logically assigned", # 85
              "LEGITIMATE SKIP Logically assigned", # 89
              "DON'T KNOW", # 94
              "REFUSED", # 97
              "BLANK (NO ANSWER)", # 98
              "LEGITIMATE SKIP") # 99
)

# Military status current (MilitaryC)
# 2012-2021
milstatDf <- data.frame(
    code=c(2, 3, 85, 94, 97, 98, 99),
    meaning=c("In a reserves component",
              "Now separated/retired from reserves/active duty",
              "BAD DATA Logically assigned", # 85
              "DON'T KNOW", # 94
              "REFUSED", # 97
              "BLANK (NO ANSWER)", # 98
              "LEGITIMATE SKIP") # 99
)

# Depression (amdelt)
# 2012-2021
mdeDf <- data.frame(
    code=1:2,
    meaning=c("Yes", "No")
)

# Sexual identity
# 2015-2021
sexIdentDf <- data.frame(
    code=c(1:3, 85, 94, 97, 98, 99),
    meaning=c("Heterosexual, that is, straight",
              "Lesbian or Gay",
              "Bisexual",
              "BAD DATA Logically assigned", # 85
              "DON'T KNOW",
              "REFUSED",
              "BLANK (NO ANSWER)",
              "LEGITIMATE SKIP")
)

codeMeaningLs <- list(Age=age3AdultDf,
                      Sex=irsexDf,
                      Health=healthDf,
                      Employment=employmentDf,
                      Income=incomeDf,
                      Education=educationDf,
                      MaritalStatus=maritalStatusDf,
                      # Currently in military (milstat): 2, 3, ..
                      MilitaryC=milstatDf,
                      # Ever in military (service): 1, 2, ..
                      MilitaryE=serviceDf,
                      Depression=mdeDf,
                      SexIdent=sexIdentDf)

cbind(names(demoTotalLs))
demoTotalLs1 <- demoTotalLs[-10]

# Differences in valid rows: i = 3, 4, 7, 9
cbind(unlist(lapply(demoTotalLs1, nrow)),
      unlist(lapply(codeMeaningLs, nrow)))

# i <- 3
for(i in 1:length(demoTotalLs1)) {
    validRows <- codeMeaningLs[[i]]$code %in% demoTotalLs1[[i]]$Answers
    demoTotalLs1[[i]] <- cbind(demoTotalLs1[[i]], codeMeaningLs[[i]][validRows,])
}

table1 <- dplyr::bind_rows(demoTotalLs1)

table1 <- table1[,-4]
table1 <- table1[,c(4,1,2,3)]

table1 <- table1[c(NA,1:8,NA,9:10,NA,11:17,NA,18:21,NA,22:25,NA,26:36,NA,37:40,NA,41:47,NA,48:53,NA,54:55,NA,56:62),]
rownames(table1) <- 1:nrow(table1)

# # Mac code to copy to clipboard, then paste in Excel.
# clip <- pipe("pbcopy", "w")
# write.table(table1, file=clip, sep = '\t', row.names = FALSE, col.names=TRUE)
# close(clip)

table1New <- table1
table1New$perc <- round(table1$perc, digits = 1)
# Remove lines 63-65 (mde diagnosis is no demographic data)
table1New <- table1New[-(63:65),]

# Number of missing values in variable SexIdent:
sum(demoTotalLs1$Age$count) - sum(demoTotalLs1$SexIdent$count)
# ------------------------------------------------------------