##Purpose: initial exploration of the CBARQ dataset after it has been curated by CBARQ_Curation

##Figure output:
##  breedrep1: bar chart of top 10 breeds in raw data
##  breedrep2: bar chart of top 10 breeds in curated data
##  claderep: bar chart of top 10 clades in curated data
##  neutergraph: stacked histogram of neuter age, separated by sex
##  neutergraphsd: stacked histogram of neuter age, separated by sex, only visualising dogs with neuter age 4sd either side of the mean
##  FACTORNAMEdist: e.g. SDAdist, ODAdist, etc. Histogram visualising score distributions for each factor
##  factorcortable: table visualising correlations between the 14 behavioural factors. Darker red = stronger correlation.
##  CorTrain: correlations of trainability only with the 14 behavioural factors
##  pca: variable correlation plot of 14 behavioural factors
##  PCAColour: variable correlation plot of 14 behavioural factors, coloured by cos2
##  PCABar: bar chart visualising each factor's cos2 to dimensions 1 and 2
##  cos2table: table visualising each factor's cos2 to each of the 5 dimensions. Darker red = greater cos2.
##  description: describes each factor. e.g. description$Dim5 gives the correlation of each factor to the dimension and the p value.

##Plotting top 10 breed representation. breedrep1 is in raw CBARQ data, breedrep2 is after curation

setwd("~/Desktop/R/CBARQ")

breedrep1 <- ggplot(slice_max(count(CBARQ$breedid), n=11, freq), aes(x=reorder(x, -freq), y=freq)) +
  geom_col()+
  geom_text(aes(label = freq), vjust = -0.5)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  labs(y="Frequency", x="Breed")


breedrep2 <- ggplot(slice_max(count(CBARQ_Curated$breedid), n=10, freq), aes(x=reorder(x, -freq), y=freq)) +
  geom_col()+
  geom_text(aes(label = freq), vjust = -0.5)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  labs(y="Frequency", x="Breed")


##Breed overlap between genetic and personality data (in raw data)
CBreeds = dplyr::count(CBARQ, breedid)
PBreeds = dplyr::count(ParkerBreeds, Breed)
Both = subset(CBreeds, (breedid %in% PBreeds$Breed))
OnlyC = subset(CBreeds, !(breedid %in% PBreeds$Breed))
OnlyP = subset(PBreeds, !(Breed %in% CBreeds$breedid))
##Note - not all the "breeds" in ParkerBreeds/PBreeds/OnlyP are breeds. 8 are summary columns

##Clade top 10 in curated data

claderep <- ggplot(slice_max(count(CBARQ_Curated$Clade), n=11, freq), aes(x=reorder(x, -freq), y=freq)) +
  geom_col()+
  geom_text(aes(label = freq), vjust = -0.5)+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  labs(y="Frequency", x="Clade")


##Clade representation in raw vs curated data

RawClades = dplyr::count(ParkerBreeds, Clade)
RawClades <- subset(RawClades, Clade !="")
CurClades = dplyr::count(ungroup(CBARQ_Curated), Clade)
CurClades <- subset(CurClades, Clade !="")
BothClades = subset(CurClades, (Clade %in% RawClades$Clade))
OnlyRaw = subset(RawClades, !(Clade %in% CurClades$Clade))

##Sex differences in age

wilcox_test(as.numeric(unlist(subset(CBARQ_Curated, CBARQ_Curated[4] == "male")[22])), as.numeric(unlist(subset(CBARQ_Curated, CBARQ_Curated[4] == "female")[22])))

CBARQ_Curated %>%
  wilcox_test(AgeAtEvaluation ~ sex) %>%
  add_significance()

CBARQ_Curated %>% wilcox_effsize(AgeAtEvaluation ~ sex)

##Investigating neutered/not neutered

##Count neuter status
count(CBARQ_Curated$isneutered=="yes")
##Count number of dogs neutered under 24 weeks
nrow(subset(subset(CBARQ_Curated, isneutered=="yes"), neuterage<24))
##Count number of dogs neutered at 24 weeks or over
nrow(subset(subset(CBARQ_Curated, isneutered=="yes"), neuterage>23))
##Count number of dogs neutered but with no neuter age provided
nrow(subset(subset(CBARQ_Curated, isneutered=="yes"), is.na(neuterage)))

##Visualising. All dogs.

neuterdata = subset(CBARQ_Curated, isneutered == "yes")
                      
neutergraph <- ggplot(neuterdata, aes(x=neuterage, color = sex))+
  geom_histogram(fill="white", alpha = 0.5, position="stack", breaks = seq(min(neuterdata$neuterage, na.rm = TRUE), max(neuterdata$neuterage, na.rm = TRUE), by = 10))+
  theme(legend.position = "top")


##Visualising. 4sd either side of the mean.

neuterdatasd = subset(CBARQ_Curated, isneutered == "yes" & !is.na(neuterage) & neuterage < (mean(neuterdata$neuterage, na.rm=TRUE)+3*sd(neuterdata$neuterage, na.rm=TRUE)) & neuterage > (mean(neuterdata$neuterage, na.rm=TRUE)-3*sd(neuterdata$neuterage, na.rm=TRUE)))

neutergraphsd <- ggplot(neuterdatasd, aes(x=neuterage, color = sex))+
  geom_histogram(fill="white", alpha = 0.5, position="stack", breaks = seq(min(neuterdatasd$neuterage, na.rm = TRUE), max(neuterdatasd$neuterage, na.rm = TRUE), by = 10))+
  theme(legend.position = "top")


##Effect of sex and age at evaluation on neuter status (i.e. are dogs of a particular sex or a particular age more likely to have been neutered)

neutersexage = CBARQ_Curated
neutersexage$isneutered <- as.integer(as.character(neutersexage$isneutered)=="yes")
neutersexage$AgeAtEvaluation <-as.integer(neutersexage$AgeAtEvaluation)
neuter.manova1 = aov(isneutered ~ sex*AgeAtEvaluation, data = neutersexage)

summary(neuter.manova1)

##Unpaired t test, sex differences in neuter status
t.test(isneutered ~ sex, neutersexage)
##Unpaired t test, sex differences in neuter age in dogs that have been neutered:
t.test(neuterage ~ sex, subset(neutersexage, isneutered == "1"))

##Effect of breed and sex on neuter status:
neuter.manova2 = aov(isneutered ~ sex*breedid, data = neutersexage)
summary(neuter.manova2)
##5 most neutered breeds
head(arrange(aggregate(neutersexage$isneutered, by=list(neutersexage$breedid), mean), desc(x)), n=5)
##5 least neutered breeds
tail(arrange(aggregate(neutersexage$isneutered, by=list(neutersexage$breedid), mean), desc(x)), n=5)
##5 oldest neutered breeds
head(arrange(aggregate(subset(neutersexage, isneutered == "1")$neuterage, by=list(subset(neutersexage, isneutered == "1")$breedid), mean, na.rm = TRUE), desc(x)), n=5)
##5 youngest neutered breeds
tail(arrange(aggregate(subset(neutersexage, isneutered == "1")$neuterage, by=list(subset(neutersexage, isneutered == "1")$breedid), mean, na.rm = TRUE), desc(x)), n=5)

##% of individuals of a breed neutered
breedneuter = aggregate(neutersexage$isneutered, by=list(neutersexage$breedid), mean)

view(aggregate(neutersexage$isneutered, by=list(neutersexage$whereacquired), mean))

summary(aov(isneutered ~ breedid*whereacquired, data = neutersexage))

##Effect of clade and sex on neuter status
neuter.manova3 = aov(isneutered ~ sex*Clade, data = neutersexage)
summary(neuter.manova3)

##5 most neutered clades
head(arrange(aggregate(neutersexage$isneutered, by=list(neutersexage$Clade), mean), desc(x)), n=5)
##5 least neutered clades
tail(arrange(aggregate(neutersexage$isneutered, by=list(neutersexage$Clade), mean), desc(x)), n=5)
##5 oldest neutered clades
head(arrange(aggregate(subset(neutersexage, isneutered == "1")$neuterage, by=list(subset(neutersexage, isneutered == "1")$Clade), mean, na.rm = TRUE), desc(x)), n=5)
##5 youngest neutered clades
tail(arrange(aggregate(subset(neutersexage, isneutered == "1")$neuterage, by=list(subset(neutersexage, isneutered == "1")$Clade), mean, na.rm = TRUE), desc(x)), n=5)



##Are neutered dogs different to non-neutered dogs on any of the factors - MANOVA 
##Factor scores: 
## CBARQ_Curated$SDA, CBARQ_Curated$Train, CBARQ_Curated$ODA, CBARQ_Curated$DDA, CBARQ_Curated$DR, CBARQ_Curated$DDF, CBARQ_Curated$SDF, CBARQ_Curated$NSF, CBARQ_Curated$TS, CBARQ_Curated$SRP, CBARQ_Curated$Excite, CBARQ_Curated$Attach, CBARQ_Curated$Chase, CBARQ_Curated$Energy

neuter.manova = manova(cbind(neutersexage$SDA, neutersexage$Train, neutersexage$ODA, neutersexage$DDA, neutersexage$DR, neutersexage$DDF, neutersexage$SDF, neutersexage$NSF, neutersexage$TS, neutersexage$SRP, neutersexage$Excite, neutersexage$Attach, neutersexage$Chase, neutersexage$Energy
) ~ isneutered, data = neutersexage)

summary.aov(neuter.manova)
summary.manova(neuter.manova)



