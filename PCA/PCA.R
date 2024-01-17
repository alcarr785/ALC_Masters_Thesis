#PCA Analysis
library(tidyverse)
library(ggplot2)
pca <- read_table2("RISG.wild.eigenvec", col_names = FALSE)
eigenval <- scan("RISG.wild.eigenval")

# set names
names(pca)[1] <- "Population"
names(pca)[2] <- "Indv"
names(pca)[3:ncol(pca)] <- paste0("PC", 1:(ncol(pca)-2))


pca <- as.tibble(data.frame(pca, Population))
pca <- pca %>% mutate(Population = recode(Population, FI = 'CL1', NEH = 'CL2')) %>%
  mutate(Type = ifelse(Population == "CL1" | Population == "CL2", "Commercial", "Wild"))
pve <- data.frame(PC = 1:20, pve = eigenval/sum(eigenval)*100)
a <- ggplot(pve, aes(PC, pve)) + geom_bar(stat = "identity")
a + ylab("Percentage variance explained") + theme_light()

b <- ggplot(pca, aes(PC1, PC2, col = Population)) + geom_point(size = 3, aes(shape = Type))
b <- b + coord_equal() + theme_light() + scale_color_brewer(palette="Set1")
b <- b + xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) + ylab(paste0("PC2 (", signif(pve$pve[2], 3), "%)"))+ ggtitle("PC1 vs. PC2 All Populations")
plot(b)
c <- ggplot(pca, aes(PC1, PC2, col = Population)) + geom_point(size = 3)
c <- c + coord_equal() + theme_light() + scale_color_manual(palette="Set1", limits = factor(3:6))
c <- c + xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) + ylab(paste0("PC2 (", signif(pve$pve[2], 3), "%)"))+ ggtitle("PC1 vs. PC2 Wild Populations")
plot(c)

