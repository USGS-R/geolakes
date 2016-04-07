# spark lines

library(dplyr)
library(ggplot2)
library(gtable)

records_data <- read.csv("inst/extdata/wqp_sites_records_groupedby_chartype.csv")

# created csv using dataRetrieval_headrequest.R (inside dataRetrieval package)
data <- read.csv('inst/extdata/wqp_temporal_charTypes.csv', stringsAsFactors = FALSE)

# group characteristic types
data <- data %>% 
  rowwise() %>% 
  mutate(year =  as.numeric(format(as.Date(startDate), "%Y"))) %>% 
  mutate(display_charType = switch(characteristicType,
                                   Biological = "Biological",
                                   Information = "Information",
                                   `Inorganics, Major, Metals` = "Inorganics",    
                                   `Inorganics, Major, Non-metals` = "Inorganics", 
                                   `Inorganics, Minor, Metals` = "Inorganics",
                                   `Inorganics, Minor, Non-metals` = "Inorganics",
                                   Microbiological = "Microbiological",
                                   Nutrient = "Nutrient",
                                   `Organics, Other` = "Organics",              
                                   `Organics, PCBs` = "Organics",
                                   `Organics, Pesticide` = "Organics",
                                   Physical = "Physical",                     
                                   `Population/Community` = "Population/Community",
                                   Radiochemical = "Radiochemical",
                                   Sediment = "Sediment",                    
                                   `Stable Isotopes` = "Stable Isotopes",
                                   Toxicity = "Toxicity")) %>% 
  filter(year >= 1950) %>%
  filter(year < 2016) %>% 
  group_by(display_charType, year) %>% 
  summarize_each(funs = 'sum', c(numSites, numResults)) 

# get bar chart for number of records
records_plot <- createRecordsBarchart(records_data) +
  guides(fill = guide_legend(title = NULL, nrow = 1, ncol = 6))
tmp <- ggplot_gtable(ggplot_build(records_plot))
leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
records_plot_legend <- tmp$grobs[[leg]]

records_plot <- records_plot +
  theme(legend.position = "none",  
        plot.title = element_blank())

# order the char types for temporal data, the same as the number of records data
records_data_parsed <- records_plot$data 
charType_order <- unlist(lapply(strsplit(rev(levels(records_data_parsed$charTypeLabels)), split = "\n"),'[', 1))
data <- data %>% 
  mutate(display_charType = factor(display_charType, levels = charType_order, ordered = TRUE))

# create the spark lines
x_axis_values <- seq(1950, 2010, by=20)
x_axis_labels <- c(paste("\n", x_axis_values[1]), "\n", "\n", "\n 2015")
sparklines <- ggplot(data = data, aes(x = year, y = numResults)) +
  geom_line() +
  facet_grid(display_charType ~ ., scales = 'free_y') + 
  scale_x_continuous(breaks = x_axis_values, labels = x_axis_labels) +
  theme_bw() + 
  theme(axis.line = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(margin = margin(b = -1, unit = 'line'),
                                   face = 'italic', size = 8, hjust = 0),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_blank(),
        plot.margin=unit(c(0.6,1.4,0.6,0.2),"lines")) #top, right, bottom, left
 
# put the bar chart and spark lines together
g <- ggplotGrob(records_plot)
g <- gtable_add_cols(g, unit(5,"cm"))
g <- gtable_add_grob(g, ggplotGrob(sparklines),
                     t = 2, l=ncol(g), b=4, r=ncol(g))
g <- gtable_add_rows(g, unit(1.5,"cm"))
g <- gtable_add_grob(g, records_plot_legend,
                     t=7, l=4, b=7, r=4)
grid.newpage()
grid.draw(g)




