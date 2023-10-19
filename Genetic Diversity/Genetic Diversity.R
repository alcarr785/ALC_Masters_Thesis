library(adegenet)
library(hierfstat)
#Genetix file created with PGDSpider
RISG <- read.genetix('RISG.gtx')
#Getting pairwise Fst values
RISG_fst <- pairwise.WCfst(RISG)
RISG_fst <- as.data.frame(RISG_fst)
write.csv(RISG_fst, "FST.csv")
RISG_stats <- basic.stats(RISG)
Heterozygosity <- merge(as.data.frame(colMeans(RISG_stats$Hs)), 
                        as.data.frame(colMeans(RISG_stats$Ho)),
                        by ='row.names', all=TRUE)
write.csv(Heterozygosity, "Heterozygosity.csv")
                
