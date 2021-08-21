setwd("/Volumes/MPHIL/CBARQ_R")

library(FactoMineR)
library(factoextra)
library(ggpubr)
library(sampling)
library(ccaPP)
library(multcomp)
library(glmulti)
library(MASS)
library(rankMANOVA)

CBARQ_Curated <- read.table("CBARQ_Curated.txt", header = TRUE)

##Internal reliability:
CBARQ_reliability = omega(CBARQ_Curated[,24:123], nfactors = 14, fm = "pc")

##Test normality of the traits:
##Doesn't work bc sample is bigger than 5000 lol
##shapiro.test(CBARQ_Curated[,124])

##Visual methods...
##Q-Q plot
ggqqplot(CBARQ_Curated$SDA)
ggqqplot(CBARQ_Curated$ODA)
ggqqplot(CBARQ_Curated$DDA)
ggqqplot(CBARQ_Curated$DDF)
ggqqplot(CBARQ_Curated$DR)
ggqqplot(CBARQ_Curated$Train)
ggqqplot(CBARQ_Curated$Chase)
ggqqplot(CBARQ_Curated$SDF)
ggqqplot(CBARQ_Curated$NSF)
ggqqplot(CBARQ_Curated$SRP)
ggqqplot(CBARQ_Curated$Excite)
ggqqplot(CBARQ_Curated$Attach)
ggqqplot(CBARQ_Curated$Energy)



##Heatmap of correlations between traits

correlations = corr.test(CBARQ_Curated[, 124:137], method = "spearman")

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
          


##PCA
forpca = CBARQ_Curated[,c(3, 124:137)]
pca = PCA(forpca, quali.sup = 1, graph = T)

pca1 = PCA(CBARQ_Curated[,c(3, 124:137)], quali.sup = 1, graph = T)


randind = CBARQ_Curated %>%
  group_by(BreedGroup) %>%
  sample_n(10)

inds = subset(CBARQ_Curated, CBARQ_Curated$dogid %in% randind$dogid)


fviz_pca_ind(X =  pca, habillage = as.factor(CBARQ_Curated$BreedGroup),
             select.ind = list(name = c(as.character(rownames(inds)))),
             label = "none",
             title = "PCA graph of individuals",
             addEllipses = TRUE)

##Colouring based on cos2
PCAColour = fviz_pca_var(pca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),  repel = TRUE, title = "PCA graph of the 14 personality factors")

##Describing the dimensions
description = dimdesc(pca, axes = c(1,2))
description$Dim.1

##Separately for ancient, modern, and non-domestic breeds

correlations = corr.test((subset(CBARQ_Curated[, 124:137],CBARQ_Curated$BreedGroup == "Non-domestic")), method = "spearman")

hM = as.data.frame(round(correlations$r,2))
hM[which((correlations$p < 0.05), arr.ind = TRUE)] <- paste0(hM[which((correlations$p < 0.05), arr.ind = TRUE)], "*")

hM = format(hM) 

heatmap.2(correlations$r, 
          main = "Spearman rank correlation between personality factors in non-domestic breeds", 
          dendrogram = "row", 
          key = TRUE, 
          trace = "none", 
          density.info = "none",
          lmat = rbind(c(0,3),c(2,1),c(0,4)),
          lwid = c(1,4),
          lhei = c(1,4,1),
          cellnote = hM,
          notecol = "black"
)


heatmap(cor((subset(CBARQ_Curated[, 124:137],CBARQ_Curated$BreedGroup == "Ancient")), method ="spearman",  use = "complete.obs"), main = "Spearman rank correlation between personality factors in ancient breeds")
heatmap(cor((subset(CBARQ_Curated[, 124:137],CBARQ_Curated$BreedGroup == "Modern")),  method ="spearman", use = "complete.obs"), main = "Spearman rank correlation between factors in modern breeds")
heatmap(cor((subset(CBARQ_Curated[, 124:137],CBARQ_Curated$BreedGroup == "Non-domestic")),  method ="spearman", use = "complete.obs"), main = "Spearman rank correlation between factors in non-domestic breeds")

##Separate PCAs. Order: ancient, modern, non-domestic 

forancient = subset(CBARQ_Curated, CBARQ_Curated$BreedGroup == "Ancient")
forancient = forancient[,c(3, 124:137)] 
pcaancient = PCA(forancient, quali.sup = 1, graph = T)
fviz_pca_var(pcaancient, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),  repel = TRUE, title = "PCA graph of the 14 personality factors in ancient breeds")
descriptionancient = dimdesc(pcaancient, axes = c(1,2))


formodern = subset(CBARQ_Curated, CBARQ_Curated$BreedGroup == "Modern")
formodern = formodern[,c(3, 124:137)] 
pcamodern = PCA(formodern, quali.sup = 1, graph = T)
fviz_pca_var(pcamodern, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),  repel = TRUE, title = "PCA graph of the 14 personality factors in modern breeds")
descriptionmodern = dimdesc(pcamodern, axes = c(1,2))


fornd = subset(CBARQ_Curated, CBARQ_Curated$BreedGroup == "Non-domestic")
fornd = fornd[,c(3, 124:137)] 
pcand = PCA(fornd, quali.sup = 1, graph = T)
fviz_pca_var(pcand, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),  repel = TRUE, title = "PCA graph of the 14 personality factors in non-domestic breeds")
descriptionnd = dimdesc(pcand, axes = c(1,2))

##One-way MANOVA for the breed groups

breedgroupman <- rankMANOVA(cbind(NSF, ODA, DR, SDA, SDF, DDF, TS, DDA, SRP, Chase, Attach, Excite, Energy, Train) ~ BreedGroup, 
                            data = CBARQ_Curated
                            , iter = 1000)
summary(breedgroupman)
pairwise = pairwise(breedgroupman, type = "Tukey"
         , uni = TRUE
         )
colnames(breedgroupman$Descriptive) <- trimws(colnames(breedgroupman$Descriptive))

##Visualising the manova results

ggbarplot(data = breedgroupman$Descriptive, 
          x = "BreedGroup", 
          y = "NSF", 
          fill = "BreedGroup",
          title = "Non-Social Fear",
          xlab = "Breed Group",
          ylab = "Unweighted treatment effect",
          legend = "right"
          ) + 
  
  easy_center_title() +
  
  geom_signif( 
    y_position = c(0.55, 0.65, 0.75),
    xmin = c(1, 2, 1),
    xmax = c(2, 3, 3),
    annotation = c("**", "***", "***"),
    tip_length = 0.2
)

ggbarplot(data = breedgroupman$Descriptive, 
          x = "BreedGroup", 
          y = "ODA", 
          fill = "BreedGroup",
          title = "Owner-Directed Aggression",
          xlab = "Breed Group",
          ylab = "Unweighted treatment effect",
          legend = "right") + 
  
  easy_center_title() +
  
  geom_signif( 
    y_position = c(0.55, 0.65, 0.75),
    xmin = c(1, 2, 1),
    xmax = c(2, 3, 3),
      annotation = c("NS", "NS", "NS"),
    tip_length = 1.5
  )

ggbarplot(data = breedgroupman$Descriptive, 
          x = "BreedGroup", 
          y = "DR", 
          fill = "BreedGroup",
          title = "Dog Rivalry",
          xlab = "Breed Group",
          ylab = "Unweighted treatment effect",
          legend = "right") + 
  
  easy_center_title() +
  
  geom_signif( 
    y_position = c(0.55, 0.65, 0.75),
    xmin = c(1, 2, 1),
    xmax = c(2, 3, 3),
    annotation = c("*", "***", "***"),
    tip_length = 0.2
  )


ggbarplot(data = breedgroupman$Descriptive, 
          x = "BreedGroup", 
          y = "SDA", 
          fill = "BreedGroup",
          title = "Stranger-Directed Aggression",
          xlab = "Breed Group",
          ylab = "Unweighted treatment effect",
          legend = "right") + 
  
  easy_center_title() +
  
  geom_signif( 
    y_position = c(0.6, 0.65, 0.75),
    xmin = c(1, 2, 1),
    xmax = c(2, 3, 3),
    annotation = c("***", "***", "NS"),
    tip_length = 0.2
  )

ggbarplot(data = breedgroupman$Descriptive, 
          x = "BreedGroup", 
          y = "SDF", 
          fill = "BreedGroup",
          title = "Stranger-Directed Fear",
          xlab = "Breed Group",
          ylab = "Unweighted treatment effect",
          legend = "right") + 
  
  easy_center_title() +
  
  geom_signif( 
    y_position = c(0.55, 0.7, 0.75),
    xmin = c(1, 2, 1),
    xmax = c(2, 3, 3),
    annotation = c("***", "***", "***"),
    tip_length = 0.2
  )

ggbarplot(data = breedgroupman$Descriptive, 
          x = "BreedGroup", 
          y = "DDF", 
          fill = "BreedGroup",
          title = "Dog-Directed Fear",
          xlab = "Breed Group",
          ylab = "Unweighted treatment effect",
          legend = "right") + 
  
  easy_center_title() +
  
  geom_signif( 
    y_position = c(0.55, 0.65, 0.75),
    xmin = c(1, 2, 1),
    xmax = c(2, 3, 3),
    annotation = c("***", "***", "***"),
    tip_length = 0.2
  )

ggbarplot(data = breedgroupman$Descriptive, 
          x = "BreedGroup", 
          y = "TS", 
          fill = "BreedGroup",
          title = "Touch Sensitivity",
          xlab = "Breed Group",
          ylab = "Unweighted treatment effect",
          legend = "right") + 
  
  easy_center_title() +
  
  geom_signif( 
    y_position = c(0.6, 0.65, 0.75),
    xmin = c(1, 2, 1),
    xmax = c(2, 3, 3),
    annotation = c("***", "***", "NS"),
    tip_length = 0.2
  )

ggbarplot(data = breedgroupman$Descriptive, 
          x = "BreedGroup", 
          y = "DDA", 
          fill = "BreedGroup",
          title = "Dog-Directed Aggression",
          xlab = "Breed Group",
          ylab = "Unweighted treatment effect",
          legend = "right") + 
  
  easy_center_title() +
  
  geom_signif( 
    y_position = c(0.55, 0.65, 0.75),
    xmin = c(1, 2, 1),
    xmax = c(2, 3, 3),
    annotation = c("NS", "NS", "NS"),
    tip_length = 1.5
  )

ggbarplot(data = breedgroupman$Descriptive, 
          x = "BreedGroup", 
          y = "SRP", 
          fill = "BreedGroup",
          title = "Separation-Related Problems",
          xlab = "Breed Group",
          ylab = "Unweighted treatment effect",
          legend = "right") + 
  
  easy_center_title() +
  
  geom_signif( 
    y_position = c(0.55, 0.65, 0.75),
    xmin = c(1, 2, 1),
    xmax = c(2, 3, 3),
    annotation = c("NS", "***", "**"),
    tip_length = 0.2
  )

ggbarplot(data = breedgroupman$Descriptive, 
          x = "BreedGroup", 
          y = "Chase", 
          fill = "BreedGroup",
          title = "Chasing",
          xlab = "Breed Group",
          ylab = "Unweighted treatment effect",
          legend = "right") + 
  
  easy_center_title() +
  
  geom_signif( 
    y_position = c(0.55, 0.68, 0.75),
    xmin = c(1, 2, 1),
    xmax = c(2, 3, 3),
    annotation = c("***", "***", "***"),
    tip_length = 0.1
  )

ggbarplot(data = breedgroupman$Descriptive, 
          x = "BreedGroup", 
          y = "Attach", 
          fill = "BreedGroup",
          title = "Attachment and Attention-Seeking",
          xlab = "Breed Group",
          ylab = "Unweighted treatment effect",
          legend = "right") + 
  
  easy_center_title() +
  
  geom_signif( 
    y_position = c(0.55, 0.65, 0.75),
    xmin = c(1, 2, 1),
    xmax = c(2, 3, 3),
    annotation = c("NS", "**", "**"),
    tip_length = 0.2
  )

ggbarplot(data = breedgroupman$Descriptive, 
          x = "BreedGroup", 
          y = "Excite", 
          fill = "BreedGroup",
          title = "Excitability",
          xlab = "Breed Group",
          ylab = "Unweighted treatment effect",
          legend = "right") + 
  
  easy_center_title() +
  
  geom_signif( 
    y_position = c(0.65, 0.65, 0.75),
    xmin = c(1, 2.1, 1),
    xmax = c(1.9, 3, 3),
    annotation = c("***", "***", "**"),
    tip_length = 0.2
  )

ggbarplot(data = breedgroupman$Descriptive, 
          x = "BreedGroup", 
          y = "Energy", 
          fill = "BreedGroup",
          title = "Energy",
          xlab = "Breed Group",
          ylab = "Unweighted treatment effect",
          legend = "right") + 
  
  easy_center_title() +
  
  geom_signif( 
    y_position = c(0.55, 0.65, 0.75),
    xmin = c(1, 2, 1),
    xmax = c(2, 3, 3),
    annotation = c("NS", "***", "***"),
    tip_length = 0.2
  )

ggbarplot(data = breedgroupman$Descriptive, 
          x = "BreedGroup", 
          y = "Train", 
          fill = "BreedGroup",
          title = "Trainability",
          xlab = "Breed Group",
          ylab = "Unweighted treatment effect",
          legend = "right") + 
  
  easy_center_title() +
  
  geom_signif( 
    y_position = c(0.55, 0.65, 0.75),
    xmin = c(1, 2, 1),
    xmax = c(2, 3, 3),
    annotation = c("NS", "NS", "NS"),
    tip_length = 1
  )

##Robust maximum association: ingroup vs outgroup

modern = subset(CBARQ_Curated, CBARQ_Curated$BreedGroup == "Modern")
ancient = subset(CBARQ_Curated, CBARQ_Curated$BreedGroup == "Ancient")
non_dom = subset(CBARQ_Curated, CBARQ_Curated$BreedGroup == "Non-domestic")

ingroup = as.data.frame(cbind(CBARQ_Curated$DR, CBARQ_Curated$ODA))
outgroup = as.data.frame(cbind(CBARQ_Curated$SDA, CBARQ_Curated$DDA, CBARQ_Curated$DDF))

cca_in_out = ccaGrid(x = ingroup, y = outgroup, k = 1, 
                     method = "spearman", maxiter = 10, 
                     maxalter = 10, nGrid = 25, tol = 1e-06,
                     standardize = TRUE, fallback = FALSE) 

##Or the max association measure?
maxcor= maxCorGrid(ingroup, outgroup, "spearman")
permTest(x = ingroup, y = outgroup, R = 1000)

ingroup_modern = as.data.frame(cbind(modern$DR, modern$ODA))
outgroup_modern = as.data.frame(cbind(modern$SDA, modern$DDA, modern$DDF))
maxcor_modern= maxCorGrid(ingroup_modern, outgroup_modern, "spearman")
permTest(x = ingroup_modern, y = outgroup_modern, R = 1000)

ingroup_ancient = as.data.frame(cbind(ancient$DR, ancient$ODA))
outgroup_ancient = as.data.frame(cbind(ancient$SDA, ancient$DDA, ancient$DDF))
maxcor_ancient= maxCorGrid(ingroup_ancient, outgroup_ancient, "spearman")
permTest(x = ingroup_ancient, y = outgroup_ancient, R = 1000)

ingroup_non_dom = as.data.frame(cbind(non_dom$DR, non_dom$ODA))
outgroup_non_dom = as.data.frame(cbind(non_dom$SDA, non_dom$DDA, non_dom$DDF))
maxcor_non_dom= maxCorGrid(ingroup_non_dom, outgroup_non_dom, "spearman")
permTest(x = ingroup_non_dom, y = outgroup_non_dom, R = 100)

##Robust maximum association: human vs dog

human = cbind(CBARQ_Curated$SDA, CBARQ_Curated$ODA, CBARQ_Curated$SDF)
dog = cbind(CBARQ_Curated$DDA, CBARQ_Curated$DDF, CBARQ_Curated$DR)

maxcor= maxCorGrid(human, dog, "spearman")

human_modern = as.data.frame(cbind(modern$SDA, modern$ODA, modern$SDF))
dog_modern = as.data.frame(cbind(modern$DDA, modern$DDF, modern$DR))
maxcor_modern_hd = maxCorGrid(human_modern, dog_modern, "spearman")
permTest(x = human_modern, y = dog_modern, R = 1000)

human_ancient = as.data.frame(cbind(ancient$SDA, ancient$ODA, ancient$SDF))
dog_ancient = as.data.frame(cbind(ancient$DDA, ancient$DDF, ancient$DR))
maxcor_ancient_hd = maxCorGrid(human_ancient, dog_ancient, "spearman")
permTest(x = human_ancient, y = dog_ancient, R = 1000)

human_non_dom = as.data.frame(cbind(non_dom$SDA, non_dom$ODA, non_dom$SDF))
dog_non_dom = as.data.frame(cbind(non_dom$DDA, non_dom$DDF, non_dom$DR))
maxcor_non_dom_hd = maxCorGrid(human_non_dom, dog_non_dom, "spearman")
permTest(x = human_non_dom, y = dog_non_dom, R = 1000)

##Robust maximum association: fear vs aggression

fear_modern = as.data.frame(cbind(modern$NSF, modern$SDF, modern$DDF))
aggression_modern = as.data.frame(cbind(modern$ODA, modern$DR, modern$SDA, modern$DDA))
maxcor_modern_fa = maxCorGrid(fear_modern, aggression_modern, "spearman")
permTest(x = fear_modern, y = aggression_modern, R = 1000)

fear_ancient = as.data.frame(cbind(ancient$NSF, ancient$SDF, ancient$DDF))
aggression_ancient = as.data.frame(cbind(ancient$ODA, ancient$DR, ancient$SDA, ancient$DDA))
maxcor_ancient_fa = maxCorGrid(fear_ancient, aggression_ancient, "spearman")
permTest(x = fear_ancient, y = aggression_ancient, R = 1000)

fear_non_dom = as.data.frame(cbind(non_dom$NSF, non_dom$SDF, non_dom$DDF))
aggression_non_dom = as.data.frame(cbind(non_dom$ODA, non_dom$DR, non_dom$SDA, non_dom$DDA))
maxcor_non_dom_fa = maxCorGrid(fear_non_dom, aggression_non_dom, "spearman")
permTest(x = fear_non_dom, y = aggression_non_dom, R = 1000)


##Model selection for Attachment and Attention Seeking
##more info here: https://hal.inrae.fr/hal-02669176/document

##all_attach = glmulti(y = Attach ~ ODA*SDA*DR*DDA*NSF*SDF*DDA*TS*Chase*Excite*SRP*Train*Energy,
##       level = 1,
##       data = CBARQ_Curated,
##       method = "g",
##      imm = 0.5,
##      report = TRUE)


modern_attach = glmulti(y = Attach ~ ODA*SDA*DR*DDA*NSF*SDF*DDA*TS*Chase*Excite*SRP*Train*Energy,
                     level = 1,
                     data = modern,
                     method = "g",
                     imm = 0.5,
                     report = TRUE)

m = glm(Attach ~ 1 + ODA + SDA + DR + DDA + NSF + SDF + Chase + Excite + SRP + Train + Energy, data = modern)

ancient_attach = glmulti(y = Attach ~ ODA*SDA*DR*DDA*NSF*SDF*DDA*TS*Chase*Excite*SRP*Train*Energy,
                        level = 1,
                        data = ancient,
                        method = "g",
                        imm = 0.5,
                        report = TRUE)

m = glm(Attach ~ 1 + ODA + SDA + Chase + Excite + SRP + Energy, data = ancient)

non_dom_attach = glmulti(y = Attach ~ ODA*SDA*DR*DDA*NSF*SDF*DDA*TS*Chase*Excite*SRP*Train*Energy,
                        level = 1,
                        data = non_dom,
                        method = "g",
                        imm = 0.5,
                        report = TRUE)

m = glm(Attach ~ 1 + ODA + SDA + NSF + SDF + Chase + Excite + SRP, data = non_dom)



##Ignore for now - this is colouring the PCA in and plotting individuals on it
fviz_pca_ind(pca,
             geom.ind = "point", # show points only (but not "text")
             col.ind = CBARQ_Curated$breedid, # color by groups
             legend.title = "Groups"
)


##Neutering
neuterman <- rankMANOVA(cbind(NSF, ODA, DR, SDA, SDF, DDF, TS, DDA, SRP, Chase, Attach, Excite, Energy, Train) ~ isneutered, 
                            data = CBARQ_Curated
                            , iter = 1000)
summary(neuterman)
pairwise(neuterman, type = "Tukey"
                    , uni = TRUE
)

acquireman <- rankMANOVA(cbind(NSF, ODA, DR, SDA, SDF, DDF, TS, DDA, SRP, Chase, Attach, Excite, Energy, Train) ~ whereacquired, 
                        data = CBARQ_Curated
                        , iter = 1000)
summary(acquireman)
pairwise(acquireman, type = "Tukey"
         , uni = TRUE
)
