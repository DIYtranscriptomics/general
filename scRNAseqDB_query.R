library(ggplot2)
library(googlesheets4)
library(tidyverse)
library(lubridate)
library(reshape2)
library(hrbrthemes)

# read in data directly from google sheet
# reference for the paper describing this database is at: https://doi.org/10.1101/742304
scRNAseq <- read_sheet("https://docs.google.com/spreadsheets/d/1En7-UV0k0laDiIfjFkdn7dggyR7jIk3WH8QgXaMOZF0/edit#gid=0")


# tidy up the data a bit for plotting
scRNAseq <- scRNAseq %>%
  mutate(year = lubridate::year(Date),
         month = lubridate::month(Date),
         day = lubridate::day(Date)) %>% 
  rename(Reported_cells_total = `Reported cells total`)

# scatter and box plot showing increase in total cells profiled over time.
# points colored by technology
ggplot(scRNAseq, aes(x=factor(year), y=Reported_cells_total)) +
  geom_boxplot(trim = FALSE, show.legend = FALSE, outlier.shape = NA) +
  geom_jitter(trim = FALSE, show.legend = FALSE, size =3,
             aes(color=Technique, text = paste("Symbol:", Tissue))) +
  scale_y_log10() +
  labs(y="Total Cells Profiled", x = "Date",
       title="The landscape of single cell sequencing experiments",
       caption="data source: bit.ly/singleCellDB_data \nscript source: bit.ly/singleCellDB_script") +
  theme(legend.position = 'none') +
  theme_ipsum_rc(axis_title_size = 18,
                 axis_title_face = "plain",
                 axis_title_just = "c",
                 caption_size = 14)

# filter this list based on tissue type
# combine 'Small intestine' and 'colon' to be just 'Intestine'
scRNAseq_filtered <- scRNAseq %>%
  filter(Tissue %in% c("Brain", "Embryo", "Culture", "Lung", "Kidney", "Tumor", 
                       "Blood", "Bone marrow", "Eye", "Pancreas", "Intestine", 
                       "Colon", "Small intestine", "Skin", "Spleen", "Lymph node", 
                       "Liver", "Heart", "Thymus", "Placenta", "Organoid", "Whole organism")) %>% 
  mutate(Tissue = replace(Tissue, Tissue == "Small intestine", "Intestine")) %>% 
  mutate(Tissue = replace(Tissue, Tissue == "Colon", "Intestine"))
  
# Produce bar plot of showing count of different tissues profiled
ggplot(scRNAseq_filtered, aes(x=fct_infreq(Tissue), fill=Tissue)) +
  geom_bar(show.legend = FALSE) +
  coord_flip() +
  labs(y="Total number of experiments", x = "Tissue type",
       title="Tissues profiled by scRNAseq",
       caption="data source: bit.ly/singleCellDB_data \nscript source: bit.ly/singleCellDB_script") +
  theme_ipsum_rc(axis_title_size = 18,
                 axis_title_face = "plain",
                 axis_title_just = "c",
                 caption_size = 14)


