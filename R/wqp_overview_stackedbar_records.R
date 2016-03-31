## stacked bar plot where number of records are plotted by site type and characteristic type

library(dplyr)
library(ggplot2)
library(gridExtra)

data <- read.csv("inst/extdata/wqp_sites_records_groupedby_sitetype.csv")

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

char_cols <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
               '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#8dd3c7',
               '#b15928','#bebada','#fb8072','#80b1d3','#fdb462')

big <- ggplot(data_big, aes(x = siteType,
                            y = numRecords, 
                            fill = characteristicType)) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  theme_classic() + 
  scale_fill_manual(values = char_cols) + 
  ggtitle("Greater than 10 million records")


small <- ggplot(data_small, aes(x = siteType,
                              y = numRecords, 
                              fill = characteristicType)) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  theme_classic() + 
  scale_fill_manual(values = char_cols) + 
  ggtitle("Less than 10 million records")


get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(big)
big <- big + theme(legend.position="none")
small <- small + theme(legend.position="none")

grid.arrange(big, legend, small, ncol = 2, nrow = 2,
             layout_matrix = rbind(c(1,2), c(3,2)),
             widths = c(4,2))
