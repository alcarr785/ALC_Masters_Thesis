library(adegenet)
library(hierfstat)
library(pegas)
library(tidyverse)
library(dunn.test)
library(reshape2)
library(pheatmap)
#Genetix file created with PGDSpider
RISG <- read.genetix('RISG.gtx')

#Getting pairwise Fst values with Heirfstat

###### Fst From Confidence Intervals
Fst_Hier <- as.data.frame(pairwise.WCfst(RISG))
Boot_Hier <- boot.ppfst(RISG, nboot=20000)
write.csv(Fst_Hier, "Fst_Hier.csv")
write.csv(as.data.frame(Boot_Hier$ll), "Boot_ll.csv")
write.csv(as.data.frame(Boot_Hier$ul), "Boot_ul.csv")
Fst_Hier <- as.matrix(Fst_Hier)

#P-Values for Fst
Fst_mean <- (Boot_Hier$ul + Boot_Hier$ll)/2
Fst_Est <- Fst_Hier-Fst_mean
Fst_SE <- (Boot_Hier$ul - Boot_Hier$ll)/(2*1.96)
Fst_z <- Fst_Est/Fst_SE
Fst_pval <- 2*(1-pnorm(abs(Fst_z)))
# plotting the correlation heatmap
pheatmap(Fst_matrix, display_numbers = T, cellwidth=40, cellheight=40, main="Pairwise FST")

#Changing values in Fis 
RISG_Fis <- RISG_fis %>%
  mutate(FID = recode(FID, FI="CL1", NEH="CL2")) %>%
  mutate(Type = ifelse(FID == "CL1" | FID == "CL2", "Commercial", "Wild")) %>%
  mutate(Heterozygosity = (RISG_fis$`N(NM)`-RISG_fis$`O(HOM)`)/RISG_fis$`N(NM)`*100)
ggplot(RISG_Fis, aes(x=FID, y = F, fill = Type))+
  geom_boxplot()+
  ylim(-1, 1)+
  labs(x = "Lineage", y=bquote(F[IS]))+
  ggtitle(bquote(F[IS]*" per Population"))
ggplot(RISG_Fis, aes(x=FID, y = Heterozygosity, fill = Type))+
  geom_boxplot()+
  ylim(0, 100)+
  labs(x = "Lineage", y="Percent of Heterozygous Loci (%)")+
  ggtitle("Heterozygosity per Population")

#Statistical test for Fis
shapiro.test(RISG_Fis$F)
kruskal.test(RISG_Fis$F ~ RISG_Fis$FID)
dunn.test(RISG_Fis$F, g = RISG_Fis$FID, method  = "bonferroni")

#Statistical test for Heterozygosity
shapiro.test(RISG_Fis$Heterozygosity)
kruskal.test(RISG_Fis$Heterozygosity ~ RISG_Fis$FID)
dunn.test(RISG_Fis$Heterozygosity, g = RISG_Fis$FID, method  = "bonferroni")


means_dplyr <- RISG_Fis %>%
  group_by(FID) %>%
  summarise(Heterozygosity = median(Heterozygosity), Fis = median(F))

