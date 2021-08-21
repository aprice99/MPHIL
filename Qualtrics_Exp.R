setwd("/Volumes/MPHIL/CBARQ_R/Qualtrics")

Qualtrics = read.table("Formatted_Qualtrics.txt", header = TRUE)

library(FactoMineR)
library(factoextra)
library(rankMANOVA)
library(GPArotation)
library(psych)
library(car)
library(qtlmt)


##Internal reliability of CBARQ
qual_CBARQ_reliability = omega(Qualtrics[,c(25:59, 61:86, 88:93, 95:127)], nfactors = 14, fm = "pc")
qual_BAI_reliability = omega(Qualtrics[,129:149], nfactors = 1, fm = "pc")
qual_PANAS_reliability = omega(Qualtrics[,150:169], nfactors = 2, fm = "pc")

##Normality of dog personality traits?

shapiro.test(as.numeric(unlist(Qualtrics$SDA)))
shapiro.test(as.numeric(unlist(Qualtrics$ODA)))
shapiro.test(as.numeric(unlist(Qualtrics$DDA)))
shapiro.test(as.numeric(unlist(Qualtrics$DDF)))
shapiro.test(as.numeric(unlist(Qualtrics$DR)))
shapiro.test(as.numeric(unlist(Qualtrics$Train)))
shapiro.test(as.numeric(unlist(Qualtrics$Chase)))
shapiro.test(as.numeric(unlist(Qualtrics$SDF)))
shapiro.test(as.numeric(unlist(Qualtrics$NSF)))
shapiro.test(as.numeric(unlist(Qualtrics$SRP)))
shapiro.test(as.numeric(unlist(Qualtrics$TS)))
shapiro.test(as.numeric(unlist(Qualtrics$Excite)))
shapiro.test(as.numeric(unlist(Qualtrics$Attach)))
shapiro.test(as.numeric(unlist(Qualtrics$Energy)))

##Normality of owner personality traits:
shapiro.test(as.numeric(unlist(Qualtrics$BAITotal)))
shapiro.test(as.numeric(unlist(Qualtrics$PANASPositive)))
shapiro.test(as.numeric(unlist(Qualtrics$PANASNegative)))


##Correlations and then PCA

correlations = corr.test(Qualtrics[, 171:184], method = "spearman")

hM = as.data.frame(round(correlations$r,2))
hM[which((correlations$p < 0.05), arr.ind = TRUE)] <- paste0(hM[which((correlations$p < 0.05), arr.ind = TRUE)], "*")

hM = format(hM) 

heatmap.2(correlations$r, 
          main = "Spearman rank correlation between personality factors", 
          dendrogram = "row", 
          key = TRUE, 
          trace = "none", 
          density.info = "none",
          lmat = rbind(c(0,3),c(2,1),c(0,4)),
          lwid = c(1.5,4),
          lhei = c(1,4,1),
          cellnote = hM,
          notecol = "black"
)



heatmap(cor(Qualtrics[, 171:184], use = "complete.obs", method = "spearman"), main = "Spearman rank correlations between personality factors")

forpca = Qualtrics[,c(6, 171:184)]
pca = PCA(forpca, quali.sup = 1, graph = T)

##Colouring based on cos2
PCAColour = fviz_pca_var(pca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),  repel = TRUE, title = "PCA graph of the 14 personality factors")

##Describing the dimensions
description = dimdesc(pca, axes = c(1,2))
description$Dim.1

##Correlations between owner personality traits
correlations = corr.test(Qualtrics[, 185:187], method = "spearman")



heatmap.2(correlations$r, 
          main = "Spearman rank correlations between owner personality traits", 
          dendrogram = "row", 
          key = TRUE, 
          trace = "none", 
          density.info = "none",
          lmat = rbind(c(0,3),c(2,1),c(0,4)),
          lwid = c(0.25,1),
          lhei = c(0.5,4,1),
          cellnote = format(round(correlations$r, 2)),
          notecol = "black",
          margins = c(10, 15)
)

##Owner personality - correlation with dog personality

correlations = corr.test(y = Qualtrics[, 171:184], x = Qualtrics [, 185:187], method = "spearman")



heatmap.2(correlations$r, 
          main = "Spearman rank correlations between dog and owner personality", 
          dendrogram = "row", 
          key = TRUE, 
          trace = "none", 
          density.info = "none",
          lmat = rbind(c(0,3),c(2,1),c(0,4)),
          lwid = c(0.25,1),
          lhei = c(1,4,1),
          cellnote = hM,
          notecol = "black",
          margins = c(5, 15)
)


hM = as.data.frame(round(correlations$r,2))
hM[which((correlations$p < 0.05), arr.ind = TRUE)] <- paste0(hM[which((correlations$p < 0.05), arr.ind = TRUE)], "*")

hM = format(hM)

corr = cor(y = Qualtrics[, 171:184], x = Qualtrics [, 185:187], method = "spearman")

rownames(correlations$r) <- c("Anxiety", "Positive Affect", "Negative Affect")

heatmap(corr, main = "Spearman rank correlations between dog and owner personality traits")

##anxiety MANOVAs

##minimal anxiety (0 to 7)
##mild anxiety (8 to 15)
##moderate anxiety (16 to 25)
##severe anxiety (26 to 63)

Qualtrics$AnxietyLevel <- NA
Qualtrics$AnxietyLevel[Qualtrics$BAITotal < 8] <- "Minimal"
Qualtrics$AnxietyLevel[Qualtrics$BAITotal > 7 & Qualtrics$BAITotal < 16] <- "Mild"
Qualtrics$AnxietyLevel[Qualtrics$BAITotal > 15 & Qualtrics$BAITotal < 26] <- "Moderate"
Qualtrics$AnxietyLevel[Qualtrics$BAITotal > 25] <- "Severe"

anxmanova <- rankMANOVA(cbind(NSF, ODA, DR, SDA, SDF, DDF, TS, DDA, SRP, Chase, Attach, Excite, Energy, Train) ~ AnxietyLevel, 
                        data = Qualtrics,
                        iter = 1000)


summary(anxmanova)


##Owner personality - affect MANOVAs

##Greater negative vs positive affect

Qualtrics$Affect <- "Same"
Qualtrics$Affect[Qualtrics$PANASPositive > (Qualtrics$PANASNegative + 5)] <- "Positive"
Qualtrics$Affect[Qualtrics$PANASPositive < (Qualtrics$PANASNegative - 5)] <- "Negative"

affmanova <- rankMANOVA(cbind(NSF, ODA, DR, SDA, SDF, DDF, TS, DDA, SRP, Chase, Attach, Excite, Energy, Train) ~ Affect, 
                        data = Qualtrics,
                        iter = 1000)
summary(affmanova)



##Association

general_fear = as.data.frame(cbind(Qualtrics$NSF, Qualtrics$SDF, Qualtrics$DDF))
general_aggression = as.data.frame(cbind(Qualtrics$ODA, Qualtrics$SDA, Qualtrics$DDA, Qualtrics$DR))
ingroup = as.data.frame(cbind(Qualtrics$DR, Qualtrics$ODA))
outgroup = as.data.frame(cbind(Qualtrics$SDA, Qualtrics$DDA, Qualtrics$DDF))
human = as.data.frame(cbind(Qualtrics$SDA, Qualtrics$ODA, Qualtrics$SDF))
dog = as.data.frame(cbind(Qualtrics$DDA, Qualtrics$DDF, Qualtrics$DR))

Qualtrics$BAITotal_norm = normalize(Qualtrics$BAITotal)
Qualtrics$PANASPositive_norm = normalize(Qualtrics$PANASPositive)
Qualtrics$PANASNegative_norm = normalize(Qualtrics$PANASNegative)

owner_personality = as.data.frame(cbind(Qualtrics$BAITotal_norm, Qualtrics$PANASPositive_norm, Qualtrics$PANASNegative_norm))


maxcor_general_fear = maxCorGrid(general_fear, owner_personality, "spearman", standardize = FALSE)
permTest(x = general_fear, y = owner_personality, R = 1000, standardize = FALSE)

maxcor_general_aggression = maxCorGrid(general_aggression, owner_personality, "spearman", standardize = FALSE)
permTest(x = general_aggression, y = owner_personality, R = 1000, standardize = FALSE)

maxcor_ingroup = maxCorGrid(ingroup, owner_personality, "spearman", standardize = FALSE)
permTest(x = ingroup, y = owner_personality, R = 1000, standardize = FALSE)

maxcor_outgroup = maxCorGrid(outgroup, owner_personality, "spearman", standardize = FALSE)
permTest(x = outgroup, y = owner_personality, R = 1000, standardize = FALSE)

maxcor_human = maxCorGrid(human, owner_personality, "spearman", standardize = FALSE)
permTest(x = human, y = owner_personality, R = 1000, standardize = FALSE)

maxcor_dog = maxCorGrid(dog, owner_personality, "spearman", standardize = FALSE)
permTest(x = dog, y = owner_personality, R = 1000, standardize = FALSE)

##Multivariate multiple regression for dog and owner personality
##Ideally, human independent and dog dependent

dogowner = lm(cbind(NSF, ODA, DR, SDA, SDF, DDF, TS, DDA, SRP, Chase, Attach, Energy, Excite, Train)
              ~ BAITotal + PANASNegative + PANASPositive,
              data = Qualtrics
              )

dogowner2 = mStep(object = dogowner, 
                  scope = cbind(NSF, ODA, DR, SDA, SDF, DDF, TS, DDA, SRP, Chase, Attach, Energy, Excite, Train)
                  ~ BAITotal + PANASNegative + PANASPositive,
                  direction = "both",
                  trace = TRUE,
                  keep = TRUE, 
                  steps = 1000,
                  k = 2)

m = lm(formula = cbind(NSF, ODA, DR, SDA, SDF, DDF, TS, DDA, SRP, 
                       Chase, Attach, Energy, Excite, Train) ~ 1, data = Qualtrics)

lh.out <- linearHypothesis(dogowner, hypothesis.matrix = c("BAITotal = 0", "PANASNegative = 0", "PANASPositive = 0"))

