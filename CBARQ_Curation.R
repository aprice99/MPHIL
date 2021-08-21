##Purpose: Prepare CBARQ data for analysis by excluding individuals, imputing missing data, and scaling data.
##Important output:
##  scaled2: curated dataset
##  ExclusionProcess: dataframe showing size of the dataset at each exclusion stage


setwd("/Volumes/MPHIL/CBARQ_R")
CBARQ = read.csv("CBARQ_data.csv")

##Load packages, install packages if necessary

package.list <- c("tidyverse", "dplyr", "missMDA", "FactoMineR", "cluster", "factoextra", "corrplot", "plyr", "BBmisc", "ggplot2", "stringr", "paletteer", "gt", "scales")
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(package.list, library, character.only = TRUE)

ExclusionProcess <- data.frame("Stage" = "Initial dataset", "n" = nrow(CBARQ))

##Remove trailing whitespace
CBARQ$breedid <-trimws(CBARQ$breedid)

##Exclude individuals who completed CBARQ multiple times
CBARQ2 <- arrange(CBARQ,dogid, date_completed)
CBARQ2 <- CBARQ[!duplicated(CBARQ$dogid),]

ExclusionProcess <- rbind(ExclusionProcess, list("Exclude duplicates", nrow(CBARQ2)))

##Exclude individuals and questions with >15% missing data
CBARQ2 = CBARQ2[which(rowMeans(!is.na(CBARQ2)) > 0.15), which(colMeans(!is.na(CBARQ2)) > 0.15)]

ExclusionProcess <- rbind(ExclusionProcess, list("Exclude >15% missing data", nrow(CBARQ2)))

##Exclude individuals with unknown age
CBARQ2 = subset(CBARQ2, AgeAtEvaluation != "unknown")

ExclusionProcess <- rbind(ExclusionProcess, list("Exclude unknown age", nrow(CBARQ2)))


##specialbreeds = subset(CBARQ2[, 1:123], breedid %in% c("Wolf", "Coyote", "Dingo", "Wolf hybrid"))

specialbreeds = subset(CBARQ2, breedid %in% c("Wolf", "Coyote", "Dingo", "Wolf hybrid"))

##Exclude countries with <100 individuals
##country = count(CBARQ2$Country)
##country = subset(country, freq>100)
##colnames(country) [1] <- "country"
##CBARQ2 = subset(CBARQ2, Country %in% country$country) 

##ExclusionProcess <- rbind(ExclusionProcess, list("Exclude countries <100 individuals", nrow(CBARQ2)))

##Exclude breeds with <50 individuals
breeds = count(CBARQ2$breedid)
colnames(breeds) [1] <- "breed"
lowbreeds = subset(breeds, freq>50)
CBARQ2 = subset(CBARQ2, breedid %in% lowbreeds$breed)

ExclusionProcess <- rbind(ExclusionProcess, list("Exclude breeds <50 individuals", nrow(CBARQ2)))


##Add wolf/dingo/coyote individuals back; they were excluded as only 5 of them
CBARQ2 <-rbind(CBARQ2, specialbreeds)

##Impute missing values
imputed = imputePCA(CBARQ2[, 23:122])
merged = cbind(CBARQ2[, c(1:22, 123)], as.data.frame(imputed$completeObs))

##Recalculate factor averages using imputed data. First line is reversed scoring for items 5, 6, and 7
merged[, 30:32] <- (4 - merged[, 30:32])
merged$SDA = rowMeans(merged[, c(35:37, 40, 41, 43, 45:47, 53)], na.rm = TRUE)
merged$ODA = rowMeans(merged[, c(34, 38, 39, 42, 44, 50, 55, 56)], na.rm = TRUE)
merged$DDA = rowMeans(merged[, c(48, 49, 51, 53)], na.rm = TRUE)
merged$DDF = rowMeans(merged[, c(70, 71, 77, 78)], na.rm = TRUE)
merged$DR = rowMeans(merged[, 57:60], na.rm = TRUE)
merged$Train = rowMeans(merged[,26:33], na.rm = TRUE)
merged$Chase = rowMeans(merged[, c(52, 99:101)], na.rm = TRUE)
merged$SDF = rowMeans(merged[, c(61, 62, 64, 65)], na.rm = TRUE)
merged$NSF = rowMeans(merged[, c(63, 66, 67, 69, 71, 73)], na.rm = TRUE)
merged$SRP = rowMeans(merged[, 79:86], na.rm = TRUE)
merged$TS = rowMeans(merged[, c(68, 74:76)], na.rm = TRUE)
merged$Excite = rowMeans(merged[, 87:92], na.rm = TRUE)
merged$Attach = rowMeans(merged[, 93:98], na.rm = TRUE)
merged$Energy = rowMeans(merged[, 116:117], na.rm = TRUE)

##Normalise data  
scaled =cbind(merged[, 1:123], normalize(merged[, 124:137]))
specialbreeds <- subset(scaled, breedid %in% c("Wolf", "Coyote", "Dingo", "Wolf hybrid"))

##Exclude individuals who are more than 4sd from the mean in weight, age, or any оf the 14 factors
##4sd due to large sample size
##SD calculated across breed (e.g. some breeds are bigger/smaller, or bred to be more/less inclined to chase)

CBARQ_Curated <- 
  scaled %>%
  group_by(breedid) %>%
  dplyr::filter(
    weight < (mean(weight) + 4*(sd(weight))) 
    & weight > (mean(weight) - 4*(sd(weight)))
    & abs(as.numeric(AgeAtEvaluation)) < mean(as.numeric(AgeAtEvaluation)) + 4*sd(as.numeric(AgeAtEvaluation))
    & abs(as.numeric(AgeAtEvaluation)) > mean(as.numeric(AgeAtEvaluation)) - 4*sd(as.numeric(AgeAtEvaluation))
    & abs(SDA) < 4*sd(SDA) 
    & abs(ODA) < 4*sd(ODA) 
    & abs(DDA) < 4*sd(DDA) 
    & abs(DDF) < 4*sd(DDF) 
    & abs(DR) < 4*sd(DR) 
    & abs(Train) < 4*sd(Train) 
    & abs(Chase) < 4*sd(Chase) 
    & abs(SDF) < 4*sd(SDF) 
    & abs(NSF) < 4*sd(NSF) 
    & abs(SRP) < 4*sd(SRP) 
    & abs(TS) < 4*sd(TS) 
    & abs(Excite) < 4*sd(Excite) 
    & abs(Attach) < 4*sd(Attach) 
    & abs(Energy) < 4*sd(Energy))

CBARQ_Curated <- rbind(CBARQ_Curated, specialbreeds)

ExclusionProcess <- rbind(ExclusionProcess, list("Exclude 4sd either side of mean", nrow(CBARQ_Curated)))

##Add clade info from ParkerBreeds
breeds = count(CBARQ_Curated$breedid)
colnames(breeds) [1] <- "breed"

setwd("~/Desktop/R/CBARQ/Tree")

ParkerBreeds = read.csv("Parker_breeds.csv")

##ParkerBreeds uses * to indicate that a breed is included in a clade with <50% bootstrap support
ParkerBreeds$Clade <- stringr::str_remove(ParkerBreeds$Clade, "\\*")
ParkerBreeds$Clade <-trimws(ParkerBreeds$Clade)

##Also, spaces in between two-word clades are inconsistent
ParkerBreeds$Clade <- stringr::str_remove(ParkerBreeds$Clade, "\\s")

##Manually correct spelling/etc in ParkerBreeds
ParkerBreeds[1,1] <- "Cocker Spaniel (American)"
ParkerBreeds[21,1] <- "Belgian Malinois"
ParkerBreeds[37,1] <- "Bullmastiff"
ParkerBreeds[137,1] <- "Shar Pei"
ParkerBreeds[50,1] <- "Coton de Tulear"
ParkerBreeds[42,1] <- "Curly-Coated Retriever"
ParkerBreeds[59,1] <- "Cocker Spaniel (English)"
ParkerBreeds[102,1] <- "Mastiff (English)"
ParkerBreeds[66,1] <- "Finnish Spitz"
ParkerBreeds[64,1] <- "Flat-Coated Retriever"
ParkerBreeds[74,1] <- "German Shepherd"
ParkerBreeds[168,1] <- "Wolf"
ParkerBreeds[91,1] <- "Australian Kelpie"
ParkerBreeds[106,1] <- "Schnauzer (Miniature)"
ParkerBreeds[114,1] <- "Otterhound"
ParkerBreeds[116,1] <- "Parson Russell Terrier"
ParkerBreeds[83,1] <- "Peruvian Inca Orchid"
ParkerBreeds[117,1] <- "Petit Basset Griffon Vendeen"
ParkerBreeds[120,1] <- "Pharaoh Hound"
ParkerBreeds[122,1] <- "Poodle (Miniature)"
ParkerBreeds[123,1] <- "Poodle (Standard)"
ParkerBreeds[124,1] <- "Poodle (Toy)"
ParkerBreeds[126,1] <- "Pug"
ParkerBreeds[146,1] <- "Staffordshire Bull Terrier"
ParkerBreeds[145,1] <- "Schnauzer (Standard)"
ParkerBreeds[148,1] <- "Swedish Vallhund"
ParkerBreeds[153,1] <- "Fox Terrier (Toy)"
ParkerBreeds[104,1] <- "Manchester Terrier (Toy)"
ParkerBreeds[157,1] <- "Fox Terrier (Wire/Wire-haired)"
ParkerBreeds[162,1] <- "Xoloitzquintle"
ParkerBreeds[163,1] <- "Xoloitzquintle (Miniature)"

CBARQ_Curated <- merge(x = CBARQ_Curated, y = ParkerBreeds, by.x = "breedid", by.y = "Breed", all.x = TRUE)[,c(1:137, 139)]

CBARQ_Curated$Clade[is.na(CBARQ_Curated$Clade)] <- "Unknown"
CBARQ_Curated$Clade[CBARQ_Curated$Clade == ""] <- "Unknown"

CBARQ_Curated$BreedGroup = "Modern"
CBARQ_Curated$BreedGroup[CBARQ_Curated$breedid %in% list("Basenji" ,
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
CBARQ_Curated$BreedGroup[CBARQ_Curated$breedid %in% list("Wolf" ,
                                                         "Wolf hybrid", 
                                                         "Coyote",
                                                         "Dingo")] <- "Non-domestic"
CBARQ_Curated$BreedGroup[CBARQ_Curated$breedid == "Mixed Breed/Unknown"] <- "Unknown"

setwd("/Volumes/MPHIL/CBARQ_R")
write.table(CBARQ_Curated, file = "CBARQ_Curated.txt")

