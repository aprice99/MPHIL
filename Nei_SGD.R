setwd("/Volumes/MPHIL/CBARQ_R/Nei_SGD")

library(data.table)
options(datatable.WhenJisSymbolThenCallingScope=TRUE)
library(plyr)

nomaf <- fread("/home/guy/Dropbox/CambridgeTeaching/students/AlexPrice/Tree/722g.990.SNP.INDEL.chrAll.dropUnder7x.infoFMissing.biallelicSNPs.Qual20.Missing1pc.indep-pairwise-500-5-033.maf01.freqCalc.frq.table.gz")
#nomaf <- fread("/home/guy/Dropbox/CambridgeTeaching/students/AlexPrice/Tree/722g.990.SNP.INDEL.chrAll.dropUnder7x.infoFMissing.biallelicSNPs.Qual20.Missing1pc.indep-pairwise-500-5-033.freqCalc.frq.table.gz")

individuals <- fread("populations.txt")

populations <- plyr::count(individuals$population)

CBARQ_Curated = read.table("/Volumes/MPHIL/CBARQ_R/CBARQ_Curated.txt", header = TRUE, row.names = 1)

##Drop breeds with only 1 representative
populations <- subset(populations, populations$freq > 1) 
nomaf2 <- nomaf
idx <- (subset(colnames(nomaf2), colnames(nomaf2) %in% populations$x))
nomaf2 <- dplyr::select(nomaf2, idx)

##Drop breeds not represented in CBARQ
colnames(nomaf2) <- stringr::str_replace_all(colnames(nomaf2), "_", " ")
colnames(nomaf2) <- stringr::str_replace(colnames(nomaf2), "-", " ")

populations$x <- stringr::str_replace_all(populations$x, "_", " ")
populations$x <- stringr::str_replace(populations$x, "-", " ")

idx2 <- (subset(colnames(nomaf2), colnames(nomaf2) %in% CBARQ_Curated$breedid))
nomaf3 <- dplyr::select(nomaf2, idx2)

##Manually check the breeds that aren't overlapping - spelling?
edit <- subset(colnames(nomaf2), !(colnames(nomaf2) %in% colnames(nomaf3)))
colnames(nomaf2)[22] <- "Shar Pei"
colnames(nomaf2)[29] <- "Cocker Spaniel (English)"
colnames(nomaf2)[34] <- "Flat-Coated Retriver"
colnames(nomaf2)[36] <- "German Shepherd"
colnames(nomaf2)[53] <- "Poodle (Miniature)"
colnames(nomaf2)[54] <- "Schnauzer (Miniature)"
colnames(nomaf2)[61] <- "Pointer"
colnames(nomaf2)[76] <- "Poodle (Standard)"
colnames(nomaf2)[77] <- "Schnauzer (Standard)"
colnames(nomaf2)[78] <- "Tibetan Mastiff"
colnames(nomaf2)[93] <- "Xoloitzquintle"

idx2 <- (subset(colnames(nomaf2), colnames(nomaf2) %in% CBARQ_Curated$breedid))
nomaf3 <- dplyr::select(nomaf2, idx2)

populations[22,1] <- "Shar Pei"
populations[29,1] <- "Cocker Spaniel (English)"
populations[34,1] <- "Flat-Coated Retriver"
populations[36,1] <- "German Shepherd"
populations[52,1] <- "Poodle (Miniature)"
populations[53,1] <- "Schnauzer (Miniature)"
populations[61,1] <- "Pointer"
populations[76,1] <- "Poodle (Standard)"
populations[77,1] <- "Schnauzer (Standard)"
populations[94,1] <- "Xoloitzquintle"
populations[78,1] <- "Tibetan Mastiff"


##Make the matrix
NSD = data.frame(matrix(ncol = 61, nrow = 61))
colnames(NSD) <- c(colnames(nomaf3))
rownames(NSD) <- colnames(NSD)



##Start calculating NSD!

 n = 1

  while(n<62)
{      
      print(n)
      m = (n+1)

      while(m <62)    
{  

##(colnames(nomaf3[,..n]))

snps = as.data.frame(dplyr::filter(nomaf3[,c(..n,..m)], (nomaf3[,..n] > -1) | (nomaf3[,..m] > -1))) 

## Population and itself

nx = as.numeric(subset(populations, populations$x == colnames(snps[1]))[2]) * 2 
Jx = sum(((nx * ((snps[1]^2) + ((1 - snps[1])^2))) - 1)/(nx - 1))/nrow(snps) 

Jy = sum(((nx * ((snps[1]^2) + ((1 - snps[1])^2))) - 1)/(nx - 1))/nrow(snps) 

Jxy = sum(((snps[1])*(snps[1])) + ((1-snps[1])*(1-snps[1])))/nrow(snps)
Dst = -log((Jxy/((Jx * Jy)^0.5)))



NSD[n,n] <- max(Dst, 0.0) 

##Population and another 

##Calculate Jx:

#number of individuals:
nx = as.numeric(subset(populations, populations$x == colnames(snps[1]))[2]) * 2 

#number of loci:

Jx = sum(((nx * ((snps[1]^2) + ((1 - snps[1])^2))) - 1)/(nx - 1))/nrow(snps) 

##Calculate Jy:

ny = as.numeric(subset(populations, populations$x == colnames(snps[2]))[2]) * 2 

Jy = sum(((ny * ((snps[2]^2) + ((1 - snps[2])^2))) - 1)/(ny - 1))/nrow(snps) 

##Calculate Jxy:

Jxy = sum(((snps[1])*(snps[2])) + ((1-snps[1])*(1-snps[2])))/nrow(snps) 
 
Dst = -log((Jxy/((Jx * Jy)^0.5)))

NSD[n,m] <- max(Dst, 0.0) 
NSD[m,n] <- max(Dst, 0.0) 
m <- (m+1)
}
n <- (n + 1)
}
 
 n=61
 nx = as.numeric(subset(populations, populations$x == colnames(snps[1]))[2]) * 2 
 Jx = sum(((nx * ((snps[1]^2) + ((1 - snps[1])^2))) - 1)/(nx - 1))/nrow(snps) 

 Jy = sum(((nx * ((snps[1]^2) + ((1 - snps[1])^2))) - 1)/(nx - 1))/nrow(snps)

 Jxy = sum(((snps[1])*(snps[1])) + ((1-snps[1])*(1-snps[1])))/nrow(snps) 
 Dst = -log((Jxy/((Jx * Jy)^0.5)))
 
 NSD[n,n] <- max(Dst, 0.0) 
 

 
 

write.table(NSD, file = "NSD_noMAFtrim_ssCorrectionOverZero.txt")



