## stacked bar plot where number of records are plotted by site type and characteristic type

library(plyr)
library(dplyr)
library(ggplot2)

data <- read.csv("inst/extdata/wqp_site_chars_all.csv")

summary_big <- data %>% 
  group_by(siteType) %>% 
  summarize(totalRecords = sum(numRecords)) %>% 
  filter(totalRecords >= 10000000)

summary_small <- data %>% 
  group_by(siteType) %>% 
  summarize(totalRecords = sum(numRecords)) %>% 
  filter(totalRecords < 10000000) 

data_big <- data %>% 
  filter(siteType %in% summary_big$siteType)

data_small <- data %>% 
  filter(siteType %in% summary_small$siteType)

# data <- data %>% 
#   mutate(isLarge = factor(ifelse(siteType %in% data_big$siteType, 
#                                  "Greater than 25 million records",
#                                  "Less than 25 million records"))) 

char_cols <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
               '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#8dd3c7',
               '#b15928','#bebada','#fb8072','#80b1d3','#fdb462')

big <- ggplot(data_big, aes(x = siteType,
                            y = numRecords, 
                            fill = characteristicType)) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  theme_classic() + 
  scale_fill_manual(values = char_cols)


small <- ggplot(data_small, aes(x = siteType,
                              y = numRecords, 
                              fill = characteristicType)) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  theme_classic() + 
  scale_fill_manual(values = char_cols)

big
small