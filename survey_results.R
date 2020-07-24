# This script produces an animated plot for visualizing changes in student performance before and after the DIYtranscriptomics course.

# load packages
library(tidyverse)
library(ggthemes)
library(reshape2)
library(gganimate)
library(hrbrthemes)

# read in text file containing anonymized responses to the survey 
survey <- read_tsv("DIY_survey_responses_2020.txt") 
# colnames(survey[6:25]) # take a look at the questions.

# rename columns to give each survey question a short title that will play nice with plotting
colnames(survey) [4] <- "terminal/bash"
colnames(survey) [5] <- "command line tools"
colnames(survey) [6] <- "RNAseq software"
colnames(survey) [7] <- "fastq vs fasta"
colnames(survey) [8] <- "read alignment"
colnames(survey) [9] <- "working in R/RStudio"
colnames(survey) [10] <- "reading/understanding an R script"
colnames(survey) [11] <- "R vs bioconductor"
colnames(survey) [12] <- "RPKM vs FPKM vs TPM"
colnames(survey) [13] <- "Where to find reference files"
colnames(survey) [14] <- "Tidyverse tools"
colnames(survey) [15] <- "datamatrix vs dataframe"
colnames(survey) [16] <- "ggplot2 for plotting"
colnames(survey) [17] <- "understand PCA plot"
colnames(survey) [18] <- "Gene Ontology"
colnames(survey) [19] <- "GSEA and MSigDB"
colnames(survey) [20] <- "accessing public RNAseq data"
colnames(survey) [21] <- "interactive dataviz"
colnames(survey) [22] <- "making Rmarkdown docs"
colnames(survey) [23] <- "project documentation"

# now format for plotting
melted <- as_tibble(melt(survey))
melted$time <-as.factor(melted$time)
melted$time <- relevel(melted$time, "start")


# plot
melted %>%
  ggplot(aes(fct_rev(variable), value, frame_vars(time))) + 
  geom_jitter(width = 0.1) +
  theme(axis.text.x = element_text(vjust= 1,hjust = 0.5)) +
  coord_flip() +
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
               geom = "crossbar", width = 0.6, color="red") +
  labs(title="Skills self-assessment at course {closest_state}", x="", y = "Self-assessed competency (1-5 scale)",
       subtitle = "PennVet RNAseq course (DIYtranscriptomics.com)",
       caption=str_wrap("Students were asked to score their level of confidence for 20 different skills, with 1 being 'absolutely none' and 5 being 'very confident'", width = 60)) +
  theme_ipsum_rc() +
  facet_wrap(~format) +
  transition_states(time) +
  ease_aes('linear')


