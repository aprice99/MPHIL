setwd("/Volumes/MPHIL/Genetic datasets/Plassais")

##setwd("~/Desktop/MDist")


package.list <- c("phangorn", "stringi", "ratematrix", "data.table", "phylotools")
new.packages <- package.list[!(package.list %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(package.list, library, character.only = TRUE)

library(ggtree)

NDist = read.table("/Volumes/MPHIL/CBARQ_R/Nei_SGD/NSD_noMAFtrim_ssCorrectionOverZero.txt", header = TRUE, row.names = 1)

Metadata = read.csv("Metadata.csv")
Metadata[217:231,2] <- "German Shepherd"
Metadata[265:291,2] <- "Wolf"
Metadata[375:376,2] <- "Poodle (Miniature)"
Metadata[377:379,2] <- "Schnauzer (Miniature)"
Metadata[487:490,2] <- "Poodle (Standard)"
Metadata[491:494,2] <- "Schnauzer (Standard)"
Metadata[510,2] <- "Poodle (Toy)"
Metadata[152:153,2] <- "Shar Pei"

NDist2 = NDist

##Already excluded breeds not represented in CBARQ_Curated data.

##Give each breed a unique 10-character name. PHYLIP requires each name to be 10 characters long,
##or padded out with blanks to the length of 10 characters. Some names here are shorter, while others
##are longer and not unique if only the last or first 10 characters are used. Easier to give new unique names.

set.seed(1)
mynames = stringi::stri_rand_strings(nrow(NDist2), 10, pattern = "[A-Z]")

NDist2$Breed <- rownames(NDist2)
rownames(NDist2)<- mynames
colnames(NDist2)[1:61]<- mynames
NDist2$PlassaisCode <- rownames(NDist2)


namekey = as.data.frame(cbind(mynames, NDist2[,62]))
namekey <- merge(x=namekey, y=Metadata, by.x = "V2", by.y = "Breed.CommonName")[,1:3]
colnames(namekey) <- list("BreedName", "MyCode", "Metadata_ID")

DistanceMatrix = as.dist(m=NDist2[,1:61], diag = TRUE, upper = TRUE)
writeDist(DistanceMatrix, file = "MDistPhylip", format = "phylip", upper = TRUE, diag = TRUE)

DistanceMatrixDomestic = as.dist(m=NDist2[-c(22, 60), c(1:21, 23:59, 61)], diag = TRUE, upper = TRUE)
writeDist(DistanceMatrixDomestic, file = "MDistPhylipDomestic", format = "phylip", upper = TRUE, diag = TRUE)

##use MDistPhylip as input for PHYLIP's neighbor program to build a neighbour-joining tree

##neighbor produces output of an unrooted tree called "outtree_neighbor" which is input for
##PHYLIP's retree program. This will be used to mid-root the tree

##retree produces output of an rooted tree called "outtree_retree" which is input for
##PHYLIP's kitsch program, along with MDistPhylip. This estimates branch lengths.

##kitsch produces output called "outtree_kitsch".

##Let's have a little look at the tree

library(tidyverse)
library(ggtree)
library(phytools)
library(phylotools)


########## Domestic vs non-domestic

tree = read.tree("outtree_kitsch")

tiplabels = cbind(NDist2[63], NDist2[62])
tree = sub.taxa.label(tree, tiplabels)

##Force the tree to be ultrametric in spite of rounding errors etc
um = is.ultrametric(tree)
if(um == FALSE)
{tree <- force.ultrametric(tree)}

plotTree(tree)
plot(tree, type = "fan")

plot.phylo(tree,
           tip.color =c(rep("blue", 2), rep("red", 3), rep("black",51), rep("red", 4),"black")
                   )


##Breed averages from CBARQ data

CBARQ_Curated = read.table("/Volumes/MPHIL/CBARQ_R/CBARQ_Curated.txt", header = TRUE, row.names = 1)

breedav = aggregate(CBARQ_Curated[, 124:137], by = list(breedid = CBARQ_Curated$breedid), mean, na.rm = TRUE)
BreedAverages = subset(breedav, breedav$breedid %in% NDist2$Breed)
rownames(BreedAverages)<- BreedAverages[,1]
BreedAverages <- BreedAverages[,-1]
BreedAverages[is.na(BreedAverages)] <- 0

BreedAverages$Regime = "Domestic"

BreedAverages$Regime[rownames(BreedAverages) %in% list("Wolf" ,
                                                       "Coyote")] <- "Non-domestic"

vectorfull <- setNames(BreedAverages$Regime,
                       rownames(BreedAverages))

BreedAverages2 <- as.matrix(BreedAverages[,1:14])

handle = ratematrixJointMCMC(data_BM = BreedAverages2, data_Mk = vectorfull, phy = tree, 
                             gen = 1000000, dir = tempdir()) 


mcmc <- readMCMC(handle)

computeESS(mcmc, 2)

plotRatematrix(mcmc)

##Interpreting the plots:
##The plots in the diagonal show evolutionary rates (variances) for each trait
##Upper‐diagonal plots show pairwise evolutionary covariation (covariances)
##The ellipses in the lower‐diagonal plots represent the 95% confidence interval 
##of each bivariate distribution for 50 randomly sampled R matrices from the posterior.

##Test the convergence of the chains using Gelman and Rubin (1992) potential
##scale factor analysis. This convergence test requires two or more independent 
##MCMC chains and compares the variance of parameter estimates between chains
##and within each chain.

##To interpret the results of the convergence check, A good convergence is achieved 
##when Gelman's R is close to 1 AND the ESS for all the parameters is large.

##Histograms can show the distribution of correlation among the traits

handle2 = ratematrixJointMCMC(data_BM = BreedAverages2, data_Mk = vectorfull, phy = tree, 
                              gen = 1000000, dir = tempdir())


mcmc2 <- readMCMC(handle2)


Rfactor <- checkConvergence(mcmc, mcmc2)
View(Rfactor)
View(Rfactor$ess)


merged.mcmc = mergePosterior(mcmc, mcmc2)
corr = extractCorrelation(merged.mcmc)

hist(corr$Domestic, xlim = c(-1,1), 
     main = "Domestic breeds",
     xlab = "Correlation among traits",
     col = "grey", 
     border = "white", 
     breaks = 20, 
     freq = FALSE)

hist(corr$'Non-domestic', xlim = c(-1,1), 
     main = "Non-domestic breeds",
     xlab = "Correlation among traits",
     col = "grey", 
     border = "white", 
     breaks = 20, 
     freq = FALSE)




##Compare overlap in evolutionary rates first (i.e. is there a difference in the rate of evolution?)

##It returns the proportion of overlap between regimes. When this proportion is less than 0.05 this 
##means that the posterior distribution of the elements of the evolutionary rate matrices does not 
##overlap more than 5%. This test statistics is NOT a p value!

plotRatematrix(merged.mcmc)


rates = testRatematrix(chain = merged.mcmc, 
                       median.test = FALSE,
                       ##     plot = TRUE,
                       par = "rates")

##Then compare overlap in evolutionary correlation (i.e. is there a difference in the pattern of evolution?)

correlations = testRatematrix(chain = merged.mcmc, 
                              median.test = FALSE,
                              ##     plot = TRUE,
                              par = "correlation")


cor2 = as.data.frame(correlations[[1]][["Regime Non-domestic x Domestic"]])
cor2[lower.tri(cor2)] <- t(cor2)[lower.tri(cor2)]
colnames(cor2) <- colnames(BreedAverages2)
rownames(cor2) <- colnames(BreedAverages2)
cor2 <- signif(cor2, 3)


###Modern vs ancient breeds (dogs only)

tree_domestic = read.tree("outtree_kitsch_domestic")

tiplabels_domestic = cbind(NDist2[63], NDist2[62])
tiplabels_domestic <- tiplabels_domestic[-c(22, 60),]

tree_domestic = sub.taxa.label(tree_domestic, tiplabels_domestic)

##Force the tree to be ultrametric in spite of rounding errors etc
um = is.ultrametric(tree_domestic)
if(um == FALSE)
{tree_domestic <- force.ultrametric(tree_domestic)}

plot(tree_domestic)
plot(tree_domestic, type = "fan")

##Breed averages from CBARQ data

BreedAverages_domestic = subset(BreedAverages, !(rownames(BreedAverages) %in% list("Wolf" ,
                                                                                 "Coyote")))
BreedAverages_domestic$Regime = "Modern"

BreedAverages_domestic$Regime[rownames(BreedAverages_domestic) %in% list("Basenji" ,
                                                       "Afghan Hound",
                                                       "Samoyed",
                                                       "Saluki",
                                                       "Canaan Dog",
                                                       "New Guinea Singing Dog",
                                                       "Chow Chow",
                                                       "Shar Pei",
                                                       "Akita",
                                                       "Alaskan Malamute",
                                                       "Siberian Husky",
                                                       "American Eskimo Dog")] <- "Ancient"


vectorfull_domestic <- setNames(BreedAverages_domestic$Regime,
                       rownames(BreedAverages_domestic))

BreedAverages_domestic <- as.matrix(BreedAverages_domestic[,1:14])

estimateTimeMCMC(data = BreedAverages_domestic, phy = tree_domestic, 
                 gen = 1000000)

handle_domestic = ratematrixJointMCMC(data_BM = BreedAverages_domestic, data_Mk = vectorfull_domestic, phy = tree_domestic, 
                             gen = 1000000, dir = tempdir()) 


mcmc_domestic <- readMCMC(handle_domestic)

computeESS(mcmc_domestic, 2)

plotRatematrix(mcmc_domestic)

##Test the convergence of the chains

##Histograms can show the distribution of correlation among the traits

handle2_domestic = ratematrixJointMCMC(data_BM = BreedAverages_domestic, data_Mk = vectorfull_domestic, phy = tree_domestic, 
                              gen = 1000000, dir = tempdir())


mcmc2_domestic <- readMCMC(handle2_domestic)


Rfactor_domestic <- checkConvergence(mcmc_domestic, mcmc2_domestic)
View(Rfactor_domestic)
View(Rfactor_domestic$ess)

merged.mcmc_domestic = mergePosterior(mcmc_domestic, mcmc2_domestic)
corr_domestic = extractCorrelation(merged.mcmc_domestic)

plotRatematrix(merged.mcmc_domestic)


hist(corr_domestic$Modern, xlim = c(-1,1), 
     main = "Modern breeds",
     xlab = "Correlation among traits",
     col = "grey", 
     border = "white", 
     breaks = 20, 
     freq = FALSE)

hist(corr_domestic$Ancient, xlim = c(-1,1), 
     main = "Ancient breeds",
     xlab = "Correlation among traits",
     col = "grey", 
     border = "white", 
     breaks = 20, 
     freq = FALSE)


##Compare overlap in evolutionary rates first (i.e. is there a difference in the rate of evolution?)

rates_domestic = testRatematrix(chain = merged.mcmc_domestic, 
                       median.test = FALSE,
                       ##     plot = TRUE,
                       par = "rates")

##Then compare overlap in evolutionary correlation (i.e. is there a difference in the pattern of evolution?)

correlations_domestic = testRatematrix(chain = merged.mcmc_domestic, 
                              median.test = FALSE,
                              ##     plot = TRUE,
                              par = "correlation")

cor2_domestic = as.data.frame(correlations_domestic[[1]][["Regime Ancient x Modern"]])
cor2_domestic[lower.tri(cor2_domestic)] <- t(cor2_domestic)[lower.tri(cor2_domestic)]
colnames(cor2_domestic) <- colnames(BreedAverages_domestic)
rownames(cor2_domestic) <- colnames(BreedAverages_domestic)
cor2_domestic <- signif(cor2_domestic, 3)
