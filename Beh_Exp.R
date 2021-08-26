setwd("/Volumes/MPHIL/Beh_Data")

library(rstatix)
library(ggpubr)

BehData = read.csv("Beh_task.csv")

Qualtrics = read.table("/Volumes/MPHIL/CBARQ_R/Qualtrics/Formatted_Qualtrics.txt", header = TRUE, row.names = 1)

##Merge behavioural data with Qualtrics data, based on owner email and dog name (this does 
##assume that each owner names their pets with unique names)

BehData$PPT <- trimws(BehData$PPT)
BehData$PPT <- tolower(BehData$PPT)
BehData$Dog <- trimws(BehData$Dog)


Qualtrics$Email<- trimws(Qualtrics$Email)
Qualtrics$Email<- tolower(Qualtrics$Email)
Qualtrics$Name<- trimws(Qualtrics$Name)

##BehMerged = merge(x = BehData, y = Qualtrics, by.x = c("Dog", "PPT"), by.y = c("Name", "Email"))

BehMerged = dplyr::inner_join(x = BehData, y = Qualtrics, by = c("Dog" = "Name", "PPT" = "Email"))

BehMerged[21,129] <- "Chow Chow"
BehMerged[21,310] <- "Ancient"

write.table(BehMerged, file = "Beh_and_Qualtrics.txt")


BehMerged = read.table("Beh_and_Qualtrics.txt", header = TRUE)


##Age
ggplot(BehMerged, aes(x=AgeAtEvaluation/48, color = Sex))+
  geom_histogram(fill="white", alpha = 0.5, position="stack",
                 ## breaks = seq(min(Qualtrics$AgeAtEvaluation, na.rm = TRUE), max(Qualtrics$AgeAtEvaluation, na.rm = TRUE), by = 10)
  ) +
  labs(y = "Frequency", x = "Age in years", title = "Stacked histogram showing age of dogs taking part in behavioural testing")

boxplot(as.numeric(BehMerged$AgeAtEvaluation/48), main = "Age of dogs taking part in behavioural testing", ylab = "Age in years", xlab = "All individuals")


complete = subset(BehMerged, BehMerged$Aborted == 0)
complete = complete[-2,]

##ANOVA

for_anova = as.data.frame(complete$L_Total)
colnames(for_anova)[1] <- "Score"
for_anova$Condition <- "Local Enhancement"

for_anova <- rbind(for_anova, data.frame(Score = complete$P_Total, Condition = "Pointing"))
for_anova <- rbind(for_anova, data.frame(Score = complete$C_Total, Condition = "Cross-Pointing"))

condition_anova <- aov(Score ~ Condition, data = for_anova)
condition_anova2 <- anova_test(Score ~ Condition, data = for_anova)

summary(condition_anova)
TukeyHSD(condition_anova)

ggboxplot(x = "Condition", 
          y = "Score", 
          data = for_anova,
          title = "Group Performance") +
  easy_center_title() +
  geom_signif(comparisons = list(c("Local Enhancement", "Pointing"), c("Local Enhancement", "Cross-Pointing"), c("Pointing", "Cross-Pointing")),
              y_position = c(10.5, 11, 10.1),
              map_signif_level = TRUE)

##Learning

for_pointing = as.data.frame(complete$P1_Result)
colnames(for_pointing)[1] <- "Result"
for_pointing$TrialNumber <- 1
for_pointing = cbind(for_pointing, complete$GF)
colnames(for_pointing)[3] <- "GF"
for_pointing = cbind(for_pointing, complete$GA)
colnames(for_pointing)[4] <- "GA"
for_pointing = cbind(for_pointing, complete$Train)
colnames(for_pointing)[5] <- "Train"

for_pointing <- rbind(for_pointing, data.frame(Result = complete$P2_Result, TrialNumber = 2, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_pointing <- rbind(for_pointing, data.frame(Result = complete$P3_Result, TrialNumber = 3, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_pointing <- rbind(for_pointing, data.frame(Result = complete$P4_Result, TrialNumber = 4, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_pointing <- rbind(for_pointing, data.frame(Result = complete$P5_Result, TrialNumber = 5, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_pointing <- rbind(for_pointing, data.frame(Result = complete$P6_Result, TrialNumber = 6, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_pointing <- rbind(for_pointing, data.frame(Result = complete$P7_Result, TrialNumber = 7, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_pointing <- rbind(for_pointing, data.frame(Result = complete$P8_Result, TrialNumber = 8, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_pointing <- rbind(for_pointing, data.frame(Result = complete$P9_Result, TrialNumber = 9, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_pointing <- rbind(for_pointing, data.frame(Result = complete$P10_Result, TrialNumber = 10, GF = complete$GF, GA = complete$GA, Train = complete$Train))

for_pointing$Result[for_pointing$Result == "n"] <- 0 
for_pointing$Result <- as.numeric(for_pointing$Result)

pointing = glm(Result ~ TrialNumber + GF + GA + Train, data = for_pointing, family = binomial())
summary(pointing)

linearHypothesis(pointing, hypothesis.matrix = c("TrialNumber = 0"))

for_cross_point = as.data.frame(complete$C1_Result)
colnames(for_cross_point)[1] <- "Result"
for_cross_point$TrialNumber <- 1
for_cross_point = cbind(for_cross_point, complete$GF)
colnames(for_cross_point)[3] <- "GF"
for_cross_point = cbind(for_cross_point, complete$GA)
colnames(for_cross_point)[4] <- "GA"
for_cross_point = cbind(for_cross_point, complete$Train)
colnames(for_cross_point)[5] <- "Train"

for_cross_point <- rbind(for_cross_point, data.frame(Result = complete$C2_Result, TrialNumber = 2, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_cross_point <- rbind(for_cross_point, data.frame(Result = complete$C3_Result, TrialNumber = 3, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_cross_point <- rbind(for_cross_point, data.frame(Result = complete$C4_Result, TrialNumber = 4, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_cross_point <- rbind(for_cross_point, data.frame(Result = complete$C5_Result, TrialNumber = 5, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_cross_point <- rbind(for_cross_point, data.frame(Result = complete$C6_Result, TrialNumber = 6, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_cross_point <- rbind(for_cross_point, data.frame(Result = complete$C7_Result, TrialNumber = 7, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_cross_point <- rbind(for_cross_point, data.frame(Result = complete$C8_Result, TrialNumber = 8, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_cross_point <- rbind(for_cross_point, data.frame(Result = complete$C9_Result, TrialNumber = 9, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_cross_point <- rbind(for_cross_point, data.frame(Result = complete$C10_Result, TrialNumber = 10, GF = complete$GF, GA = complete$GA, Train = complete$Train))

for_cross_point$Result[for_cross_point$Result == "n"] <- 0 
for_cross_point$Result <- as.numeric(for_cross_point$Result)

cross_point = glm(Result ~ TrialNumber + GA + GF + Train, data = for_cross_point, family = binomial())
summary(cross_point)



for_local = as.data.frame(complete$P1_Result)
colnames(for_local)[1] <- "Result"
for_local$TrialNumber <- 1
for_local = cbind(for_local, complete$GF)
colnames(for_local)[3] <- "GF"
for_local = cbind(for_local, complete$GA)
colnames(for_local)[4] <- "GA"
for_local = cbind(for_local, complete$Train)
colnames(for_local)[5] <- "Train"

for_local <- rbind(for_local, data.frame(Result = complete$P2_Result, TrialNumber = 2, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_local <- rbind(for_local, data.frame(Result = complete$P3_Result, TrialNumber = 3, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_local <- rbind(for_local, data.frame(Result = complete$P4_Result, TrialNumber = 4, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_local <- rbind(for_local, data.frame(Result = complete$P5_Result, TrialNumber = 5, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_local <- rbind(for_local, data.frame(Result = complete$P6_Result, TrialNumber = 6, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_local <- rbind(for_local, data.frame(Result = complete$P7_Result, TrialNumber = 7, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_local <- rbind(for_local, data.frame(Result = complete$P8_Result, TrialNumber = 8, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_local <- rbind(for_local, data.frame(Result = complete$P9_Result, TrialNumber = 9, GF = complete$GF, GA = complete$GA, Train = complete$Train))
for_local <- rbind(for_local, data.frame(Result = complete$P10_Result, TrialNumber = 10, GF = complete$GF, GA = complete$GA, Train = complete$Train))

for_local$Result[for_local$Result == "n"] <- 0 
for_local$Result <- as.numeric(for_local$Result)

local_enhancement = glm(Result ~ TrialNumber + GA + GF + Train, data = for_local, family = binomial())
summary(local_enhancement)
linearHypothesis(local_enhancement, hypothesis.matrix = c("TrialNumber = 0"))

##Training trials?

complete$Av_Total = (complete$P_Total + complete$C_Total + complete$L_Total)/3

cor.test(complete$T_Total, complete$Av_Total)
cor.test(complete$T_Total, complete$P_Total)
cor.test(complete$T_Total, complete$C_Total)
cor.test(complete$T_Total, complete$L_Total)

m = glm(Aborted ~ 1 + T_Total, data = BehMerged, family = binomial)
summary(m)

##Participating?

participation = rankMANOVA(cbind(NSF, ODA, DR, SDA, SDF, DDF, TS, DDA, SRP, Chase, Attach, Excite, Energy, Train) ~ Aborted, 
                           data = BehMerged,
                           iter = 1000)
summary(participation)


participation_model = glmulti(y = Aborted ~ ODA*SDA*DR*DDA*NSF*SDF*DDA*TS*Chase*Excite*SRP*Train*Energy*Attach,
                        level = 1,
                        data = BehMerged,
                        method = "g",
                        imm = 0.5,
                        report = TRUE,
                        family = binomial)

m = glm(Aborted ~ 1 + ODA + TS + Train, data = BehMerged)
summary(m)

##Order effects

for_order = as.data.frame(complete$P_Total)
colnames(for_order)[1] <- "Score"
for_order$Condition <- "Pointing"
for_order$Order <- complete$P_Position
for_order$Weather <- complete$Weather
for_order$Individuals <- rownames(complete)

for_order <- rbind(for_order, data.frame(Score = complete$C_Total,
                                         Condition = "Cross-Pointing",
                                         Order = complete$C._Position,
                                         Weather = complete$Weather,
                                         Individuals = rownames(complete)))

for_order <- rbind(for_order, data.frame(Score = complete$L_Total,
                                         Condition = "Local Enhancement",
                                         Order = complete$L_Position,
                                         Weather = complete$Weather,
                                         Individuals = rownames(complete)))

for_order$Weather[for_order$Weather == "Rain"] <- "Cloud"

order_anova <- anova_test(data = for_order, Score ~ Condition*Order*Weather, wid = Individuals)
get_anova_table(order_anova)

ggboxplot(x = "Order", 
          y = "Score", 
          data = for_order,
          title = "Group Performance",
          xlab = "Order of Cue") +
  easy_center_title() +
  geom_signif(comparisons = list(c("1", "2"), c("1", "3"), c("2", "3")),
              y_position = c(10.5, 11, 10.1),
              map_signif_level = TRUE)

##glm for order effects
for_order_glm = as.data.frame(complete$P_Total)
colnames(for_order_glm)[1] <- "Score"
for_order_glm$P_dummy <- 1
for_order_glm$L_dummy <- 0
for_order_glm$Order <- complete$P_Position

for_order_glm <- rbind(for_order_glm, data.frame(Score = complete$L_Total,
                                         P_dummy = 0,
                                         L_dummy = 1,
                                         Order = complete$L_Position))

for_order_glm <- rbind(for_order_glm, data.frame(Score = complete$C_Total,
                                                 P_dummy = 0,
                                                 L_dummy = 0,
                                                 Order = complete$C._Position))

order_glm = glm(Score ~ 1 + L_dummy + P_dummy + Order, data = for_order_glm)
summary(order_glm)

##Dog personality and performance - pointing

cor.test(x = complete$P_Total, y = complete$SDA)
cor.test(x = complete$P_Total, y = complete$ODA)
cor.test(x = complete$P_Total, y = complete$DDA)
cor.test(x = complete$P_Total, y = complete$DDF)
cor.test(x = complete$P_Total, y = complete$DR)
cor.test(x = complete$P_Total, y = complete$Train)
cor.test(x = complete$P_Total, y = complete$Chase)
cor.test(x = complete$P_Total, y = complete$SDF)
cor.test(x = complete$P_Total, y = complete$NSF)
cor.test(x = complete$P_Total, y = complete$SRP)
cor.test(x = complete$P_Total, y = complete$TS)
cor.test(x = complete$P_Total, y = complete$Excite)
cor.test(x = complete$P_Total, y = complete$Attach)
cor.test(x = complete$P_Total, y = complete$Energy)


##Owner personality and performance
cor.test(x = complete$P_Total, y = complete$BAITotal)
cor.test(x = complete$P_Total, y = complete$PANASNegative)
cor.test(x = complete$P_Total, y = complete$PANASPositive)

##Cross-pointing

cor.test(x = complete$C_Total, y = complete$SDA)
cor.test(x = complete$C_Total, y = complete$ODA)
cor.test(x = complete$C_Total, y = complete$DDA)
cor.test(x = complete$C_Total, y = complete$DDF)
cor.test(x = complete$C_Total, y = complete$DR)
cor.test(x = complete$C_Total, y = complete$Train)
cor.test(x = complete$C_Total, y = complete$Chase)
cor.test(x = complete$C_Total, y = complete$SDF)
cor.test(x = complete$C_Total, y = complete$NSF)
cor.test(x = complete$C_Total, y = complete$SRP)
cor.test(x = complete$C_Total, y = complete$TS)
cor.test(x = complete$C_Total, y = complete$Excite)
cor.test(x = complete$C_Total, y = complete$Attach)
cor.test(x = complete$C_Total, y = complete$Energy)


##Owner personality and performance
cor.test(x = complete$C_Total, y = complete$BAITotal)
cor.test(x = complete$C_Total, y = complete$PANASNegative)
cor.test(x = complete$C_Total, y = complete$PANASPositive)

##Local enhancement
cor.test(x = complete$L_Total, y = complete$SDA)
cor.test(x = complete$L_Total, y = complete$ODA)
cor.test(x = complete$L_Total, y = complete$DDA)
cor.test(x = complete$L_Total, y = complete$DDF)
cor.test(x = complete$L_Total, y = complete$DR)
cor.test(x = complete$L_Total, y = complete$Train)
cor.test(x = complete$L_Total, y = complete$Chase)
cor.test(x = complete$L_Total, y = complete$SDF)
cor.test(x = complete$L_Total, y = complete$NSF)
cor.test(x = complete$L_Total, y = complete$SRP)
cor.test(x = complete$L_Total, y = complete$TS)
cor.test(x = complete$L_Total, y = complete$Excite)
cor.test(x = complete$L_Total, y = complete$Attach)
cor.test(x = complete$L_Total, y = complete$Energy)


##Owner personality and performance
cor.test(x = complete$L_Total, y = complete$BAITotal)
cor.test(x = complete$L_Total, y = complete$PANASNegative)
cor.test(x = complete$L_Total, y = complete$PANASPositive)

