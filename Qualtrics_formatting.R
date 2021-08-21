##Purpose: to format a csv file of questionnaire responses downloaded from Qualtrics for analysis
##See the "Qualtrics variable and factor names" document for more detailed information about variable names and scoring.
##Output:
##  Qualtrics: a dataframe containing demographic information, CBARQ raw scores, BAI raw scores, PANAS raw scores, followed by calculated factor averages, BIA, and PANAS totals
##  QualtricsExclusion: dataframe showing size of the dataset at each curation stage


setwd("/Volumes/MPHIL/CBARQ_R/Qualtrics")
Qualtrics = read.csv("Qualtrics_Data.csv")

package.list <- c("tidyverse", "xlsx", "dplyr", "missMDA", "FactoMineR", "cluster", "factoextra", "corrplot", "plyr", "BBmisc", "ggplot2", "stringr", "paletteer", "gt", "scales")
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(package.list, library, character.only = TRUE)

##Remove location data, IP address, time taken, etc
Qualtrics <- Qualtrics[-2, -c(1:6, 8:17, 19:20, 188, 190:218)]

##Rename columns (to have shorter names), and change scoring
Qualtrics$Finished <- revalue(Qualtrics$Finished, c("True"=1))
Qualtrics$Finished <- revalue(Qualtrics$Finished, c("False"=0))

colnames(Qualtrics) [2] <- "Eligible"
Qualtrics$Eligible <- revalue(Qualtrics$Eligible, c("No"=1))
Qualtrics$Eligible <- revalue(Qualtrics$Eligible, c("Yes"=0))

colnames(Qualtrics) [3] <- "Country"

colnames(Qualtrics) [4] <- "Name"

colnames(Qualtrics) [5] <- "Sex"

colnames(Qualtrics) [6] <- "Breed"

colnames(Qualtrics) [7] <- "AgeAtEvaluation"

colnames(Qualtrics) [8] <- "AgeAcquired"

colnames(Qualtrics) [9] <- "Weight"

colnames(Qualtrics) [10] <- "WhereAcquired"

colnames(Qualtrics) [11] <- "IsNeutered"
Qualtrics$IsNeutered <- revalue(Qualtrics$IsNeutered, c("No"=0))
Qualtrics$IsNeutered <- revalue(Qualtrics$IsNeutered, c("Yes"=1))
Qualtrics$IsNeutered <- revalue(Qualtrics$IsNeutered, c("Unsure/don't know"=2))

colnames(Qualtrics) [12] <- "NeuterAge"

colnames(Qualtrics) [13] <- "NeuteredWhy"

colnames(Qualtrics) [14] <- "HealthProblems"
Qualtrics$HealthProblems <- revalue(Qualtrics$HealthProblems, c("No"=0))
Qualtrics$HealthProblems <- revalue(Qualtrics$HealthProblems, c("Yes"=1))
Qualtrics$HealthProblems <- revalue(Qualtrics$HealthProblems, c("Unsure/don't know"=2))

colnames(Qualtrics) [15] <- "HealthDescription"

colnames(Qualtrics) [16] <- "Concerned"

colnames(Qualtrics) [17] <- "Roles"

colnames(Qualtrics) [18] <- "FirstOwned"
Qualtrics$FirstOwned <- revalue(Qualtrics$FirstOwned, c("No"=0))
Qualtrics$FirstOwned <- revalue(Qualtrics$FirstOwned, c("Yes"=1))

colnames(Qualtrics) [19] <- "OwnedAmt"

colnames(Qualtrics) [20] <- "AsChild"
Qualtrics$AsChild <- revalue(Qualtrics$AsChild, c("No"=0))
Qualtrics$AsChild <- revalue(Qualtrics$AsChild, c("Yes"=1))

colnames(Qualtrics) [21] <- "OtherPets"
Qualtrics$OtherPets <- revalue(Qualtrics$OtherPets, c("Yes"=0))
Qualtrics$OtherPets <- revalue(Qualtrics$OtherPets, c("No"=1))

colnames(Qualtrics) [22] <- "Pets"

colnames(Qualtrics) [23] <- "PetsText"

colnames(Qualtrics) [24] <- "OtherDogsAge"

QualtricsExclusion <- data.frame("Stage" = "Full dataset", "Individuals remaining" = nrow(Qualtrics))

##Exclude individuals ineligible for the study
Qualtrics <- subset(Qualtrics, Qualtrics$Eligible == "1")

QualtricsExclusion <- rbind(QualtricsExclusion, list("Exclude ineligible individuals", nrow(Qualtrics)))

##Exclude individuals with more than 15% missing data
Qualtrics = Qualtrics[which((rowMeans(!is.na(Qualtrics[c(1:32, 60, 79:87, 94:169)])) > 0.15) 
                            & (rowMeans(!Qualtrics[c(1:32, 60, 79:87, 94:169)] == "") > 0.15)
),]

QualtricsExclusion <- rbind(QualtricsExclusion, list("Exclude >15% missing data overall", nrow(Qualtrics)))

colnames(Qualtrics) [25] <- "tra1"
colnames(Qualtrics) [26] <- "tra2"
colnames(Qualtrics) [27] <- "tra3"
colnames(Qualtrics) [28]<- "tra4"
colnames(Qualtrics) [29] <- "tra5"
colnames(Qualtrics) [30] <- "tra6"
colnames(Qualtrics) [31] <- "tra7"
colnames(Qualtrics) [32] <- "tra8"

Qualtrics[, 25:32][Qualtrics[, 25:32]=="Never"] <- 0
Qualtrics[, 25:32][Qualtrics[, 25:32]=="Seldom"] <- 1
Qualtrics[, 25:32][Qualtrics[, 25:32]=="Sometimes"] <- 2
Qualtrics[, 25:32][Qualtrics[, 25:32]=="Usually"] <- 3
Qualtrics[, 25:32][Qualtrics[, 25:32]=="Always"] <- 4
Qualtrics[, 25:32][Qualtrics[, 25:32]== "Not Observed/Not Applicable"] <- NA

colnames(Qualtrics) [33] <- "agg9"
colnames(Qualtrics) [34] <- "agg10"
colnames(Qualtrics) [35] <- "agg11"
colnames(Qualtrics) [36] <- "agg12"
colnames(Qualtrics) [37] <- "agg13"
colnames(Qualtrics) [38] <- "agg14"
colnames(Qualtrics) [39] <- "agg15"
colnames(Qualtrics) [40] <- "agg16"
colnames(Qualtrics) [41] <- "agg17"
colnames(Qualtrics) [42] <- "agg18"
colnames(Qualtrics) [43] <- "agg19"
colnames(Qualtrics) [44] <- "agg20"
colnames(Qualtrics) [45] <- "agg21"
colnames(Qualtrics) [46] <- "agg22"
colnames(Qualtrics) [47] <- "agg23"
colnames(Qualtrics) [48] <- "agg24"
colnames(Qualtrics) [49] <- "agg25"
colnames(Qualtrics) [50] <- "agg26"
colnames(Qualtrics) [51] <- "agg27"
colnames(Qualtrics) [52] <- "agg28"
colnames(Qualtrics) [53] <- "agg29"
colnames(Qualtrics) [54] <- "agg30"
colnames(Qualtrics) [55] <- "agg31"
colnames(Qualtrics) [56] <- "agg32"
colnames(Qualtrics) [57] <- "agg33"
colnames(Qualtrics) [58] <- "agg34"
colnames(Qualtrics) [59] <- "agg35"

##Here, the questionnaire used a sliding scale that was positioned at 0 by default
Qualtrics[, 33:59][Qualtrics[, 33:59]== ""] <- 0
Qualtrics[, 33:59][Qualtrics[, 33:59]== "Not Applicable"] <- NA

colnames(Qualtrics) [60] <- "OtherAgg"

colnames(Qualtrics) [61] <- "fear36"
colnames(Qualtrics) [62] <- "fear37"
colnames(Qualtrics) [63] <- "fear38"
colnames(Qualtrics) [64] <- "fear39"
colnames(Qualtrics) [65] <- "fear40"
colnames(Qualtrics) [66] <- "fear41"
colnames(Qualtrics) [67] <- "fear42"
colnames(Qualtrics) [68] <- "fear43"
colnames(Qualtrics) [69] <- "fear44"
colnames(Qualtrics) [70] <- "fear45"
colnames(Qualtrics) [71] <- "fear46"
colnames(Qualtrics) [72] <- "fear47"
colnames(Qualtrics) [73] <- "fear48"
colnames(Qualtrics) [74] <- "fear49"
colnames(Qualtrics) [75] <- "fear50"
colnames(Qualtrics) [76] <- "fear51"
colnames(Qualtrics) [77] <- "fear52"
colnames(Qualtrics) [78] <- "fear53"

##Here, the questionnaire used a sliding scale that was positioned at 0 by default
Qualtrics[, 61:78][Qualtrics[, 61:78]== ""] <- 0
Qualtrics[, 61:78][Qualtrics[, 61:78]== "Not Applicable"] <- NA

colnames(Qualtrics) [79] <- "sep54"
colnames(Qualtrics) [80] <- "sep55"
colnames(Qualtrics) [81] <- "sep56"
colnames(Qualtrics) [82] <- "sep57"
colnames(Qualtrics) [83] <- "sep58"
colnames(Qualtrics) [84] <- "sep59"
colnames(Qualtrics) [85] <- "sep60"
colnames(Qualtrics) [86] <- "sep61"

Qualtrics[, 79:86][Qualtrics[, 79:86]=="Never"] <- 0
Qualtrics[, 79:86][Qualtrics[, 79:86]=="Seldom"] <- 1
Qualtrics[, 79:86][Qualtrics[, 79:86]=="Sometimes"] <- 2
Qualtrics[, 79:86][Qualtrics[, 79:86]=="Usually"] <- 3
Qualtrics[, 79:86][Qualtrics[, 79:86]=="Always"] <- 4
Qualtrics[, 79:86][Qualtrics[, 79:86]== "Not Observed/Not Applicable"] <- NA

colnames(Qualtrics) [87] <- "OtherFear"

colnames(Qualtrics) [88] <- "exc62"
colnames(Qualtrics) [89] <- "exc63"
colnames(Qualtrics) [90] <- "exc64"
colnames(Qualtrics) [91] <- "exc65"
colnames(Qualtrics) [92] <- "exc66"
colnames(Qualtrics) [93] <- "exc67"

##Here, the questionnaire used a sliding scale that was positioned at 0 by default
Qualtrics[, 88:93][Qualtrics[, 88:93]== ""] <- 0
Qualtrics[, 88:93][Qualtrics[, 88:93]== "Not Applicable"] <- NA

colnames(Qualtrics) [94] <- "OtherExcite"

colnames(Qualtrics) [95] <- "att68"
colnames(Qualtrics) [96] <- "att69"
colnames(Qualtrics) [97] <- "att70"
colnames(Qualtrics) [98] <- "att71"
colnames(Qualtrics) [99] <- "att72"
colnames(Qualtrics) [100] <- "att73"

Qualtrics[, 95:100][Qualtrics[, 95:100]=="Never"] <- 0
Qualtrics[, 95:100][Qualtrics[, 95:100]=="Seldom"] <- 1
Qualtrics[, 95:100][Qualtrics[, 95:100]=="Sometimes"] <- 2
Qualtrics[, 95:100][Qualtrics[, 95:100]=="Usually"] <- 3
Qualtrics[, 95:100][Qualtrics[, 95:100]=="Always"] <- 4
Qualtrics[, 95:100][Qualtrics[, 95:100]== "Not Observed/Not Applicable"] <- NA

colnames(Qualtrics) [101] <- "mis74"
colnames(Qualtrics) [102] <- "mis75"
colnames(Qualtrics) [103] <- "mis76"
colnames(Qualtrics) [104] <- "mis77"
colnames(Qualtrics) [105] <- "mis78"
colnames(Qualtrics) [106] <- "mis79"
colnames(Qualtrics) [107] <- "mis80"
colnames(Qualtrics) [108] <- "mis81"
colnames(Qualtrics) [109] <- "mis82"
colnames(Qualtrics) [110] <- "mis83"
colnames(Qualtrics) [111] <- "mis84"
colnames(Qualtrics) [112] <- "mis85"
colnames(Qualtrics) [113] <- "mis86"
colnames(Qualtrics) [114] <- "mis87"
colnames(Qualtrics) [115] <- "mis88"
colnames(Qualtrics) [116] <- "mis89"
colnames(Qualtrics) [117] <- "mis90"
colnames(Qualtrics) [118] <- "mis91"
colnames(Qualtrics) [119] <- "mis92"
colnames(Qualtrics) [120] <- "mis93"
colnames(Qualtrics) [121] <- "mis94"
colnames(Qualtrics) [122] <- "mis95"
colnames(Qualtrics) [123] <- "mis96"
colnames(Qualtrics) [124] <- "mis97"
colnames(Qualtrics) [125] <- "mis98"
colnames(Qualtrics) [126] <- "mis99"
colnames(Qualtrics) [127] <- "mis100"

Qualtrics[, 101:127][Qualtrics[, 101:127]=="Never"] <- 0
Qualtrics[, 101:127][Qualtrics[, 101:127]=="Seldom"] <- 1
Qualtrics[, 101:127][Qualtrics[, 101:127]=="Sometimes"] <- 2
Qualtrics[, 101:127][Qualtrics[, 101:127]=="Usually"] <- 3
Qualtrics[, 101:127][Qualtrics[, 101:127]=="Always"] <- 4
Qualtrics[, 101:127][Qualtrics[, 101:127]== "Not Observed/Not Applicable"] <- NA

colnames(Qualtrics) [128] <- "OtherMisc"

colnames(Qualtrics) [129] <- "BAI1"
colnames(Qualtrics) [130] <- "BAI2"
colnames(Qualtrics) [131] <- "BAI3"
colnames(Qualtrics) [132] <- "BAI4"
colnames(Qualtrics) [133] <- "BAI5"
colnames(Qualtrics) [134] <- "BAI6"
colnames(Qualtrics) [135] <- "BAI7"
colnames(Qualtrics) [136] <- "BAI8"
colnames(Qualtrics) [137] <- "BAI9"
colnames(Qualtrics) [138] <- "BAI10"
colnames(Qualtrics) [139] <- "BAI11"
colnames(Qualtrics) [140] <- "BAI12"
colnames(Qualtrics) [141] <- "BAI13"
colnames(Qualtrics) [142] <- "BAI14"
colnames(Qualtrics) [143] <- "BAI15"
colnames(Qualtrics) [144] <- "BAI16"
colnames(Qualtrics) [145] <- "BAI17"
colnames(Qualtrics) [146] <- "BAI18"
colnames(Qualtrics) [147] <- "BAI19"
colnames(Qualtrics) [148] <- "BAI20"
colnames(Qualtrics) [149] <- "BAI21"

Qualtrics[, 129:149][Qualtrics[, 129:149]=="Not at all"] <- 0
Qualtrics[, 129:149][Qualtrics[, 129:149]=="Mildly, but it didn't bother me much"] <- 1
Qualtrics[, 129:149][Qualtrics[, 129:149]=="Moderately - it wasn't pleasant at times"] <- 2
Qualtrics[, 129:149][Qualtrics[, 129:149]=="Severely - it bothered me a lot"] <- 3

colnames(Qualtrics) [150] <- "PANAS1"
colnames(Qualtrics) [151] <- "PANAS2"
colnames(Qualtrics) [152] <- "PANAS3"
colnames(Qualtrics) [153] <- "PANAS4"
colnames(Qualtrics) [154] <- "PANAS5"
colnames(Qualtrics) [155] <- "PANAS6"
colnames(Qualtrics) [156] <- "PANAS7"
colnames(Qualtrics) [157] <- "PANAS8"
colnames(Qualtrics) [158] <- "PANAS9"
colnames(Qualtrics) [159] <- "PANAS10"
colnames(Qualtrics) [160] <- "PANAS11"
colnames(Qualtrics) [161] <- "PANAS12"
colnames(Qualtrics) [162] <- "PANAS13"
colnames(Qualtrics) [163] <- "PANAS14"
colnames(Qualtrics) [164] <- "PANAS15"
colnames(Qualtrics) [165] <- "PANAS16"
colnames(Qualtrics) [166] <- "PANAS17"
colnames(Qualtrics) [167] <- "PANAS18"
colnames(Qualtrics) [168] <- "PANAS19"
colnames(Qualtrics) [169] <- "PANAS20"

Qualtrics[, 150:169][Qualtrics[, 150:169]=="Very slightly or not at all"] <- 1
Qualtrics[, 150:169][Qualtrics[, 150:169]=="A little"] <- 2
Qualtrics[, 150:169][Qualtrics[, 150:169]=="Moderately"] <- 3
Qualtrics[, 150:169][Qualtrics[, 150:169]=="Quite a bit"] <- 4
Qualtrics[, 150:169][Qualtrics[, 150:169]=="Extremely"] <- 5

colnames(Qualtrics) [170] <- "Email"

##Remove first row (Qualtrics writes the question in the first row)
Qualtrics <- Qualtrics[-1,]

##Exclude individuals with more than 15% missing data in owner personality section
Qualtrics = Qualtrics[which((rowMeans(!is.na(Qualtrics[, 129:169])) > 0.15) & (rowMeans(!Qualtrics[, 129:169] == "") > 0.15)),]

QualtricsExclusion <- rbind(QualtricsExclusion, list("Exclude >15% missing owner personality data", nrow(Qualtrics)))

rownames(Qualtrics) <- seq(length=nrow(Qualtrics))

##Dealing with age and weight variables. Convert age data to weeks to be in line with Serpell's dataset.

##AgeAtEvaluation
Qualtrics$AgeAtEvaluation <- stringr::str_remove(Qualtrics$AgeAtEvaluation, "mths")
Qualtrics$AgeAtEvaluation <- stringr::str_remove(Qualtrics$AgeAtEvaluation, "months")
Qualtrics$AgeAtEvaluation <- stringr::str_remove(Qualtrics$AgeAtEvaluation, "month")
Qualtrics$AgeAtEvaluation <- stringr::str_remove(Qualtrics$AgeAtEvaluation, "Month")
Qualtrics$AgeAtEvaluation <- stringr::str_remove(Qualtrics$AgeAtEvaluation, "Months")
Qualtrics$AgeAtEvaluation <- stringr::str_remove(Qualtrics$AgeAtEvaluation, "approximately")
Qualtrics$AgeAtEvaluation <- stringr::str_remove(Qualtrics$AgeAtEvaluation, "approx")
Qualtrics$AgeAtEvaluation <- stringr::str_remove(Qualtrics$AgeAtEvaluation, "Approximately")
Qualtrics$AgeAtEvaluation <- stringr::str_remove(Qualtrics$AgeAtEvaluation, "Approx")
Qualtrics$AgeAtEvaluation <-trimws(Qualtrics$AgeAtEvaluation)
##Identify values where people have written something
editAAE = Qualtrics[grep('[A-Za-z ]', Qualtrics$AgeAtEvaluation),]
##Edit these manually. Where age is in years, multiply by 12 for months
Qualtrics[3,7] <- 72
Qualtrics[62,7] <- 30
Qualtrics[79,7] <- 176
Qualtrics[95,7] <- 132
Qualtrics[113,7] <- 96
Qualtrics[128,7] <- 96
Qualtrics[156,7] <- 84
Qualtrics[160,7] <- 7
Qualtrics[170,7] <- 30
Qualtrics[184,7] <- 120
Qualtrics[198,7] <- 48

##Then multiply by 4 for age in weeks
Qualtrics$AgeAtEvaluation <- as.numeric(Qualtrics$AgeAtEvaluation) * 4

##AgeAcquired
Qualtrics$AgeAcquired <- stringr::str_remove(Qualtrics$AgeAcquired, "months")
Qualtrics$AgeAcquired <- stringr::str_remove(Qualtrics$AgeAcquired, "mths")
Qualtrics$AgeAcquired <- stringr::str_remove(Qualtrics$AgeAcquired, "month")
Qualtrics$AgeAcquired <- stringr::str_remove(Qualtrics$AgeAcquired, "Months")
Qualtrics$AgeAcquired <- stringr::str_remove(Qualtrics$AgeAcquired, "Month")
Qualtrics$AgeAcquired <- stringr::str_remove(Qualtrics$AgeAcquired, "approximately")
Qualtrics$AgeAcquired <- stringr::str_remove(Qualtrics$AgeAcquired, "approx")
Qualtrics$AgeAcquired <- stringr::str_remove(Qualtrics$AgeAcquired, "Approximately")
Qualtrics$AgeAcquired <- stringr::str_remove(Qualtrics$AgeAcquired, "Approx")
Qualtrics$AgeAcquired <-trimws(Qualtrics$AgeAcquired)
##Identify values where people have written something
editAA = Qualtrics[grep('[A-Za-z ]', Qualtrics$AgeAcquired),]
##Edit these manually. Where age is in years, multiply by 12 for months
Qualtrics[23,8] <- 2
Qualtrics[68,8] <- 36
Qualtrics[95,8] <- 120
Qualtrics[113,8] <- 60
Qualtrics[128,8] <- 36
Qualtrics[156,8] <- 14
Qualtrics[167,8] <- 2
Qualtrics[170,8] <- 18
Qualtrics[184,8] <- 2
Qualtrics[195,8] <- 0
Qualtrics[198,8] <- 12
Qualtrics[199,8] <- 1

##Then multiply by 4 for age in weeks
Qualtrics$AgeAcquired <- as.numeric(Qualtrics$AgeAcquired) * 4

##Weight
Qualtrics$Weight <- stringr::str_remove(Qualtrics$Weight, "lbs")
Qualtrics$Weight <- stringr::str_remove(Qualtrics$Weight, "lb")
Qualtrics$Weight <- stringr::str_remove(Qualtrics$Weight, "approximately")
Qualtrics$Weight <- stringr::str_remove(Qualtrics$Weight, "approx")
Qualtrics$Weight <- stringr::str_remove(Qualtrics$Weight, "Approximately")
Qualtrics$Weight <- stringr::str_remove(Qualtrics$Weight, "Approx")
Qualtrics$Weight <- stringr::str_remove(Qualtrics$Weight, "Pounds")
Qualtrics$Weight <- stringr::str_remove(Qualtrics$Weight, "pounds")
Qualtrics$Weight<-trimws(Qualtrics$Weight)
##Identify values where people have written something
editW = Qualtrics[grep('[A-Za-z ]', Qualtrics$Weight),]
##Edit these manually. Where weight is in kg, multiply by 2.2 for lb
Qualtrics[2, 9] <- 18
Qualtrics[5, 9] <- NA
Qualtrics[22, 9] <- NA
Qualtrics[151, 9] <- 29
Qualtrics[180, 9] <- NA
Qualtrics[184, 9] <- NA

##NeuterAge
Qualtrics$NeuterAge <- stringr::str_remove(Qualtrics$NeuterAge, "months")
Qualtrics$NeuterAge <- stringr::str_remove(Qualtrics$NeuterAge, "Months")
Qualtrics$NeuterAge <- stringr::str_remove(Qualtrics$NeuterAge, "mths")
Qualtrics$NeuterAge <- stringr::str_remove(Qualtrics$NeuterAge, "month")
Qualtrics$NeuterAge <- stringr::str_remove(Qualtrics$NeuterAge, "Month")
Qualtrics$NeuterAge <- stringr::str_remove(Qualtrics$NeuterAge, "approximately")
Qualtrics$NeuterAge <- stringr::str_remove(Qualtrics$NeuterAge, "approx")
Qualtrics$NeuterAge <- stringr::str_remove(Qualtrics$NeuterAge, "Approximately")
Qualtrics$NeuterAge <- stringr::str_remove(Qualtrics$NeuterAge, "Approx")
Qualtrics$NeuterAge<-trimws(Qualtrics$NeuterAge)
##Identify values where people have written something
editNA = Qualtrics[grep('[A-Za-z ]', Qualtrics$NeuterAge),]
##Edit these manually. Where age is in years, multiply by 12 for months
Qualtrics[2, 12] <- 5
Qualtrics[7, 12] <- NA
Qualtrics[10, 12] <- NA
Qualtrics[26, 12] <- NA
Qualtrics[34, 12] <- NA
Qualtrics[35, 12] <- NA
Qualtrics[37, 12] <- 6
Qualtrics[56, 12] <- NA
Qualtrics[61, 12] <- NA
Qualtrics[69, 12] <- NA
Qualtrics[84, 12] <- 6
Qualtrics[87, 12] <- NA
Qualtrics[95, 12] <- 36
Qualtrics[101, 12] <- NA
Qualtrics[109, 12] <- NA
Qualtrics[113, 12] <- 60
Qualtrics[128, 12] <- 36
Qualtrics[156, 12] <- 13
Qualtrics[164, 12] <- NA
Qualtrics[170, 12] <- 12

##Then multiply by 4 for age in weeks
Qualtrics$NeuterAge <- as.numeric(Qualtrics$NeuterAge) * 4


##Impute missing CBARQ data. Here the same method is used as in CBARQ_Curation
Qualtrics[, c(25:59, 61:86, 88:93, 95:127)] <-as.integer(unlist(Qualtrics[, c(25:59, 61:86, 88:93, 95:127)]))
impute = imputePCA(Qualtrics[, c(25:59, 61:86, 88:93, 95:127)])
Qualtrics[, c(25:59, 61:86, 88:93, 95:127)] <-as.data.frame(impute$completeObs)


##Calculate CBARQ factor averages using imputed data. First line is reversed scoring for items 5, 6, and 7
Qualtrics[, 29:31] <- (4 - Qualtrics[, 29:31])
Qualtrics$SDA = rowMeans(Qualtrics[, c(34:36, 39, 40, 42, 44, 45, 46, 52)])
Qualtrics$ODA = rowMeans(Qualtrics[, c(33, 37, 38, 41, 43, 49, 54, 55)])
Qualtrics$DDA = rowMeans(Qualtrics[, c(47, 48, 50, 53)])
Qualtrics$DDF = rowMeans(Qualtrics[, c(70, 71, 77, 78)])
Qualtrics$DR = rowMeans(Qualtrics[, 56:59])
Qualtrics$Train = rowMeans(Qualtrics[,25:32])
Qualtrics$Chase = rowMeans(Qualtrics[, c(51, 101:103)])
Qualtrics$SDF = rowMeans(Qualtrics[, c(61, 62, 64, 65)])
Qualtrics$NSF = rowMeans(Qualtrics[, c(63, 66, 67, 69, 71, 73)])
Qualtrics$SRP = rowMeans(Qualtrics[, 79:86])
Qualtrics$TS = rowMeans(Qualtrics[, c(68, 74:76)])
Qualtrics$Excite = rowMeans(Qualtrics[, 88:93])
Qualtrics$Attach = rowMeans(Qualtrics[, 95:100])
Qualtrics$Energy = rowMeans(Qualtrics[, 118:119])

##Impute missing BAI data and score BAI
Qualtrics[, 129:149] <-as.integer(unlist(Qualtrics[, 129:149]))
imputeBAI = imputePCA(Qualtrics[, 129:149])
Qualtrics[, 129:149] <-as.data.frame(imputeBAI$completeObs)
Qualtrics$BAITotal = rowSums(Qualtrics[, 129:149])

##Impute missing PANAS data and score PANAS
Qualtrics[, 150:169] <-as.integer(unlist(Qualtrics[, 150:169]))
imputePANAS = imputePCA(Qualtrics[, 150:169])
Qualtrics[, 150:169] <-as.data.frame(imputePANAS$completeObs)
Qualtrics$PANASPositive = rowSums(Qualtrics[,c(150, 152, 154, 158, 159, 161, 163, 165, 166, 168)])
Qualtrics$PANASNegative = rowSums(Qualtrics[, c(151, 153, 155:157, 160, 162, 164, 167, 169)])

Qualtrics$BreedGroup = "Modern"
Qualtrics$BreedGroup[Qualtrics$Breed %in% list("Basenji" ,
                                                         "Afghan Hound", 
                                                         "Samoyed",
                                                         "Saluki",
                                                         "Chow Chow",
                                                         "Canaan Dog",
                                                         "Shar Pei",
                                                         "Akita",
                                                         "Alaskan Malamute",
                                                         "Siberian Husky",
                                                         "American Eskimo Dog")] <- "Ancient"
Qualtrics$BreedGroup[Qualtrics$Breed %in% list("Wolf" ,
                                                         "Wolf hybrid", 
                                                         "Coyote",
                                                         "Dingo")] <- "Non-domestic"
Qualtrics$BreedGroup[Qualtrics$Breed == "Mixed Breed/Unknown"] <- "Unknown"

Qualtrics$GF <- rowMeans(Qualtrics[,c(174, 178, 179)])
Qualtrics$GA <- rowMeans(Qualtrics[,c(171, 172, 173, 175)])


##Then write the formatted dataset to directory
write.table(Qualtrics, file = "Formatted_Qualtrics.txt")






