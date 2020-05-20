# Introduction ----
# This script uses a mock dataset to demonstrate some of the key concepts in gene clustering
# figures generated in this script are used in the lecture on gene modules (lecture 10)

# make a mock datasets of 5 genes expressed across 6 conditions ----
# gene E serves as 'weird' gene that throws off sample clustering to highlight the need for spearman vs pearson correlation
geneA <- c(1,3,1,3,1,3)
geneB <- c(3,1,3,1,3,1)
geneC <- c(11,14,11,14,11,14)
geneD <- c(14,11,14,11,14,11)
geneE <- c(1,3,1,14,11,14)

modDemo <- rbind(geneA, geneB, geneC, geneD, geneE)
colnames(modDemo) <- c("day1", "night1", "day2", "night2", "day3", "night3")
modDemo

# plot gene expression -----
# discuss this plot.  which genes should be grouped together?  
# revisit this plot when heatmap is made
modDemo.df <- as_tibble(modDemo, rownames="geneID")
modDemo.df <- pivot_longer(modDemo.df,
                           cols=day1:night3,
                           names_to="time",
                           values_to="expression")

ggplot(modDemo.df) +
  aes(x=fct_inorder(time), y=expression, color=geneID) +
  geom_point() +
  geom_line(aes(group=geneID)) +
  theme_bw()

# clustering  ----
# cluster rows (genes) and columns (samples) and cut the resulting tree to generate modules
clustGenes <- hclust(as.dist(1-cor(t(modDemo), method="pearson")), method="complete") 
clustSamples <- hclust(as.dist(1-cor(modDemo, method="spearman")), method="complete") 
module.assign <- cutree(clustGenes, k=2)
module.assign

# create heatmap ----
# toggle scale from 'row' to 'none' to highlight the difference and how this relates to the line plot above
heatmap.2(modDemo, 
          Rowv=as.dendrogram(clustGenes), 
          Colv=as.dendrogram(clustSamples), 
          col=myheatcolors2, scale='none',
          density.info="none", trace="none",  
          colsep=c(1:6),rowsep=c(1:4),sepcolor="white",
          cexRow=1, cexCol=1, margins=c(8,20)) 
