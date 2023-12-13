if(kappa == "sameSampleSize") {
    
    maleFemale <- c(floor(length(which(riskData$sex==0))/requiredNEach), floor(length(which(riskData$sex==1))/requiredNEach))
    
    # With kappa = same sample size, it is always the overall smaller group (here: males) that will dictate the maximum number of repetitions, no matter what requiredNEach is.
    smaller <- 1
    
    # Pick floor(possibleSamples) many samples, each having requiredN many individuals
    pick <- rep(rep(1:maleFemale[smaller], each=requiredNEach), times=2)
    # Randomly select the required number from among men and women:
    set.seed(579)
    selectedMales <- sample(which(riskData$sex==0), size = maleFemale[smaller]*requiredNEach)
    set.seed(579)
    selectedFemales <- sample(which(riskData$sex==1), size = maleFemale[smaller]*requiredNEach)
    
    # riskData1 is already ordered by males (first 50%) and females (second 50%)
    riskData1 <- riskData[c(selectedMales, selectedFemales),]
    # dim(riskData) # e.g., > 399k
    # dim(riskData1) # e.g., > 368k, 31'000 individuals less.
    
    # Check that the expected order of males and females exists:
    if(!
    all(which(riskData1$sex==0) == 1:(nrow(riskData1)/2))) {
        stop("Check proportions test, kappa = same sample size.")
    }
    if(!
    all(which(riskData1$sex==1) == ((nrow(riskData1)/2+1)):nrow(riskData1))) {
        stop("Check proportions test, kappa = same sample size.")
    }
    
    # Things are in place now, to randomly assign males and females to exactly one group, so that outcome proportions can be compared.
    riskData1$pick <- pick
    # Produce a random order of these floor(possibleSamples) many samples (guarantee reproducibility, set.seed(259))
    set.seed(259)
    riskData1$pick[which(riskData1$sex==0)] <- sample(riskData1$pick[riskData1$sex==0])
    set.seed(259)
    riskData1$pick[which(riskData1$sex==1)] <- sample(riskData1$pick[riskData1$sex==1])
    
} else if(kappa == "unequalSampleSize") {
    
    maleFemale <- c(floor(length(which(riskData$sex==0))/requiredN1), floor(length(which(riskData$sex==1))/requiredN2))
    
    # It is not always the males which is the smaller group.
    idxSmallerGroup <- which(maleFemale == min(maleFemale))
    if(length(idxSmallerGroup)==2) {
        smaller <- 1
    } else {
        smaller <- idxSmallerGroup
    }
    
    # Pick floor(possibleSamples) many samples, each having requiredN many individuals
    pickMale <- rep(1:maleFemale[smaller], each=requiredN1)
    pickFemale <- rep(1:maleFemale[smaller], each=requiredN2)
    # Randomly select the required number from among men and women:
    set.seed(579)
    selectedMales <- sample(which(riskData$sex==0), size = maleFemale[smaller]*requiredN1)
    set.seed(579)
    selectedFemales <- sample(which(riskData$sex==1), size = maleFemale[smaller]*requiredN2)
    
    # riskData1 is already ordered by males (first 50%) and females (second 50%)
    riskData1 <- riskData[c(selectedMales, selectedFemales),]
    # dim(riskData) # e.g., > 399k
    # dim(riskData1) # e.g., > 368k, 31'000 individuals less.
    
    # Check (all indices in both groups differ by 1; that means they are in strict increasing, sequential order, as expected).
    if(!
    all(diff(which(riskData1$sex==0))==1)) {
        stop("Check proportions test, kappa = unequal sample size.")
    }
    if(!
    all(diff(which(riskData1$sex==1))==1)) {
        stop("Check proportions test, kappa = unequal sample size.")
    }
    
    # Things are in place now, to randomly assign males and females to exactly one group, so that outcome proportions can be compared.
    riskData1$pick <- c(pickMale, pickFemale)
    # Produce a random order of these floor(possibleSamples) many samples (guarantee reproducibility, set.seed(259))
    set.seed(259)
    riskData1$pick[which(riskData1$sex==0)] <- sample(riskData1$pick[riskData1$sex==0])
    set.seed(259)
    riskData1$pick[which(riskData1$sex==1)] <- sample(riskData1$pick[riskData1$sex==1])
}


propResLs <- list()
start <- Sys.time()
for(i in 1:maleFemale[smaller]) {
    
    tbl2x2 <- table(riskData1[riskData1$pick==i,c("sex", "ltmde")])
    
    # Use package rstatix, run prop_test, get z value.
    # ?rstatix::prop_test
    propResLs[[i]] <- rstatix::prop_test(x=DescTools::Rev(tbl2x2), correct = FALSE, alternative="greater", detailed=TRUE, conf.level = confLevel)
    # str(propRes)
    
}
stop <- Sys.time()
print(difftime(stop, start))

propRes <- data.frame(dplyr::bind_rows(propResLs))