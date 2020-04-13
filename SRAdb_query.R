library(ggplot2)
library(tidyverse)
library(lubridate)
library(hrbrthemes)

# read in data from SRA
sra <- read_csv("https://www.ncbi.nlm.nih.gov/Traces/sra/sra_stat.cgi")

# tidy up a bit
sra <- sra %>%
  mutate(date = mdy(date),
         year = lubridate::year(date),
         month = lubridate::month(date),
         day = lubridate::day(date))

#plot as filled area with some annotations 
ggplot(sra, aes(x=date, y=bases)) +
  geom_area() +
  labs(y="Total bases", x = "Date",
       title="Growth of the Sequence Read Archive (SRA)",
       subtitle="Data source: bit.ly/SRA_query",
       caption=paste0("produced on ", Sys.time())) +
  #scale_y_log10() +
  annotate(geom="text",x=as.Date("2015-01-01"), y=1.1e+16, label="Illumina \n NextSeq",fontface="bold") +
  annotate("segment", x = as.Date("2015-01-01"), xend = as.Date("2015-01-01"), y = 9e+15, yend =3e+15, colour = "black", size=1, alpha=0.6, arrow=arrow()) +
  annotate(geom="text",x=as.Date("2010-01-01"), y=1.1e+16, label="Illumina \n HiSeq 2000",fontface="bold") +
  annotate("segment", x = as.Date("2010-01-01"), xend = as.Date("2010-06-01"), y = 9e+15, yend =1e+15, colour = "black", size=1, alpha=0.6, arrow=arrow()) +
  annotate(geom="text",x=as.Date("2011-06-01"), y=1.1e+16, label="Illumina \n MiSeq",fontface="bold") +
  annotate("segment", x = as.Date("2011-06-01"), xend = as.Date("2011-01-01"), y = 9e+15, yend =1e+15, colour = "black", size=1, alpha=0.6, arrow=arrow()) +
  annotate(geom="text",x=as.Date("2017-01-01"), y=2.1e+16, label="Illumina \n NovaSeq",fontface="bold") +
  annotate("segment", x = as.Date("2017-01-01"), xend = as.Date("2017-01-01"), y = 1.8e+16, yend =1e+16, colour = "black", size=1, alpha=0.6, arrow=arrow()) +
  theme_ipsum_rc()

