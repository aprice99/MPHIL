setwd("/Volumes/MPHIL/CBARQ_R/Heritability")

library(NAM)
library(dplyr)
library(sampling)
library(plyr)
library(Pstat)

Metadata = read.csv("Metadata.csv")
Metadata[217:231,2] <- "German Shepherd"
Metadata[265:291,2] <- "Wolf"
Metadata[375:376,2] <- "Poodle (Miniature)"
Metadata[377:379,2] <- "Schnauzer (Miniature)"
Metadata[487:490,2] <- "Poodle (Standard)"
Metadata[491:494,2] <- "Schnauzer (Standard)"
Metadata[510,2] <- "Poodle (Toy)"
Metadata[152:153,2] <- "Shar Pei"

Qualtrics = read.table("/Volumes/MPHIL/CBARQ_R/Qualtrics/Formatted_Qualtrics.txt", header = TRUE, row.names = 1)

Qualtrics <- subset(Qualtrics, !(Qualtrics$Breed == ""))

##This section is to make a file describing which breeds to keep (for use in plink)

breedslist = plyr::count(Qualtrics$Breed)

All = read.csv("GRM.csv", header = FALSE)

keep = as.data.frame(All$V1)
colnames(keep) <- "keep_id"
abcdef = subset(Metadata, Metadata$Breed.CommonName %in% breedslist$x)[1]
keep = subset(keep, keep$keep_id %in% abcdef$Name_ID)
keep$col2 = keep$keep_id

write.table(keep, 
            file = "/Users/anap/Desktop/plink_mac_20210606/keeplist.txt", 
            row.names = FALSE, 
            col.names = FALSE,
            quote = FALSE)

##In plink, calculate GRM once I've dropped the breeds not in keeplist

##Read in this new GRM

GRM = read.csv("GRM_Keep.csv", header = FALSE)


rownames(GRM)<- GRM[,1]
colnames(GRM)[-1]<- GRM[,1]

GRM <- GRM[-1]
GRM[upper.tri(GRM)] <- t(GRM)[upper.tri(GRM)]

individuals = as.data.frame(rownames(GRM))

BehTraits = subset(Metadata, Metadata$Name_ID %in% individuals$`rownames(GRM)`)[-(3:14)]

##Random sampling without replacement

mysample = subset(Qualtrics, Qualtrics$Breed %in% BehTraits$Breed.CommonName)
mysample <- mysample[order(mysample$Breed),]

set.seed(1)

strata <- sampling::strata(mysample,
                           "Breed",
                           as.numeric(unlist((plyr::count(BehTraits$Breed.CommonName)[2]))),
                           "srswr",
                           description = TRUE
)

personality = getdata(mysample, strata)[,1:190]

personality$Weight <- as.numeric(personality$Weight)

personality <- personality[ order(personality$Breed), ]
BehTraits <- BehTraits[ order(BehTraits$Breed.CommonName), ]

personality <- cbind(personality, BehTraits$Name_ID)
colnames(personality)[191] <- "Name_ID"

dog_personality = personality[,c(170:183, 191)]

rownames(dog_personality) = dog_personality[,15]
dog_personality = dog_personality[,-15]

owner_personality = personality[,c(184:186, 191)]

rownames(owner_personality) = owner_personality[,4]
owner_personality = owner_personality[,-4]


##Where weight is NA, replace with breed average from Qualtrics dataset
Qualtrics$Weight <- as.numeric(Qualtrics$Weight)
breed_means = aggregate(Qualtrics$Weight, by = list(Qualtrics$Breed), mean, na.rm = TRUE)
body_size = as.data.frame(cbind(personality$Breed, personality$Weight, personality$Name_ID))
body_size$V2 <- as.numeric(body_size$V2)

colnames(body_size)[1] <- "Breed"
colnames(breed_means)[1] <- "Breed"

body_size = body_size %>% inner_join(breed_means, by= "Breed") 

body_size$V2 = coalesce(body_size$V2, body_size$x)
body_size <- body_size[1:3]
colnames(body_size)[2] <- "Weight"
colnames(body_size)[3] <- "Name_ID"
rownames(body_size) <-  body_size$Name_ID

owner_personality <- cbind(owner_personality, body_size$Weight)


## y = trait, K = kinship matrix (nxn)


reml_output_no_owner = reml(y = as.matrix(dog_personality), X = as.matrix(body_size[2]), K = as.matrix(GRM))

reml_output_owner = reml(y = as.matrix(dog_personality), X = as.matrix(owner_personality), K = as.matrix(GRM))

for_plot = as.data.frame(cbind(colnames(dog_personality), reml_output_no_owner$VC$h2))
colnames(for_plot) = c("Trait", "Heritability")
for_plot$Condition = "Without owner personality"
for_plot2 = as.data.frame(cbind(colnames(dog_personality), reml_output_owner$VC$h2, "With owner personality"))
colnames(for_plot2) = c("Trait", "Heritability", "Condition")
for_plot <- rbind(for_plot, for_plot2)

for_plot$Heritability <- as.numeric(for_plot$Heritability)

ggplot(for_plot, 
       aes(x = Trait, y = Heritability, fill = Condition))+
  geom_col(position = "dodge")+
  ggtitle("Heritability with and without owner personality as a covariate")+
  xlab("Dog personality trait")+
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0,0.8))+
  easy_center_title()

##Qst-Fst analysis

common = plyr::count(Qualtrics$Breed)
common <- subset(common, common$freq > 5)
common <- common[-12,]

QualCommon <-subset(Qualtrics, Qualtrics$Breed %in% common$x)


##Estimating Qst
QST = Pst(data = Qualtrics[,c(6, 171:184)],
          ci = 1,
          boot = 100,
          pe = 0.95)

QST_common = Pst(data = QualCommon[,c(6, 171:184)],
          ci = 1,
          boot = 100,
          pe = 0.95)

BootQST = BootPst(data = Qualtrics[,c(6, 171:184)],
                  va = 1,
          opt = "ci",
          boot = 10,
          pe = 0.95)

##Pretty pictures
library(ggplot2)

colnames(QST_common)[3:4] <- c("LowBound", "UpBound")



ggplot(data = QST_common,
       aes(x = Quant_Varia, y = Pst_Values, ymin = LowBound, ymax = UpBound)) +
  labs(y = "Pst (approximation for Qst)", x = "Personality", title = "Pst and Fst comparisons") +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
  coord_flip()+
  easy_center_title() +
  geom_hline(yintercept = 0.19, linetype = "dotted") 

trace = TracePst(data = Qualtrics[,c(6, 171:184)],va=0,ci=1,boot=1000,pe=0.95,Fst=-1,Pw=0,Rp=0,Ri=0,xm=2,pts=30)



