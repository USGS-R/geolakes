## stacked bar plot where number of sites are plotted by site type and characteristic type

library(dplyr)
library(ggplot2)

data <- read.csv("inst/extdata/wqp_sites_records_groupedby_sitetype.csv")

data_sites <- data %>%  
  mutate(percentSites = (numSites/totalNumUniqueSites)*100)

char_cols <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
               '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#8dd3c7',
               '#b15928','#bebada','#fb8072','#80b1d3','#fdb462')

ggplot(data_sites, aes(x = siteType, 
                       y = percentSites, 
                       fill = characteristicType)) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  theme_classic() + 
  scale_fill_manual(values = char_cols) 