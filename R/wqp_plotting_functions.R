
# change site and characteristic types for the plot labels
displayChars <- function(characteristicType){
  switch(characteristicType,
         Biological = "Biological",
         Information = "Information",
         `Inorganics, Major, Metals` = "Inorganics",    
         `Inorganics, Major, Non-metals` = "Inorganics", 
         `Inorganics, Minor, Metals` = "Inorganics",
         `Inorganics, Minor, Non-metals` = "Inorganics",
         Microbiological = "Microbiological",
         Nutrient = "Nutrient",
         `Not Assigned` = "NA",
         `Organics, Other` = "Organics",              
         `Organics, PCBs` = "Organics",
         `Organics, Pesticide` = "Organics",
         Physical = "Physical",                     
         `Population/Community` = "Population/Community",
         Radiochemical = "Radiochemical",
         Sediment = "Sediment",                    
         `Stable Isotopes` = "Stable Isotopes",
         Toxicity = "Toxicity",
         Total = "Total")
}

displaySites <- function(siteType){
  switch(siteType,
         `Aggregate groundwater use` = "Groundwater",
         `Aggregate surface-water-use` = "Other", 
         Atmosphere = "Other",                 
         Estuary = "Marine",
         Facility = "Facility",                  
         Glacier = "Other",                     
         `Lake, Reservoir, Impoundment` = "Lake",
         Land = "Other", 
         `Not Assigned` = "NA",
         Ocean = "Marine",                      
         Spring = "Groundwater",                     
         Stream = "Stream",                   
         Subsurface = "Groundwater",                 
         Well = "Groundwater",                       
         Wetland = "Other")
}

## horizontal stacked bar plot where number of records are plotted by site type and characteristic type
createRecordsBarchart <- function(results_data, type = "percent"){
  library(dplyr)
  library(ggplot2)
  
  data_newcategories <- results_data %>% 
    rowwise() %>% 
    mutate(display_siteType = displaySites(siteType)) %>% 
    mutate(display_charType = displayChars(characteristicType)) %>% 
    ungroup() %>% #end the rowwise function
    filter(display_siteType != "NA")
  
  totalnumResults <- data_newcategories %>% 
    group_by(display_charType) %>% 
    summarize(totalnumResults = sum(numResults)) %>% 
    ungroup()
  
  numResults_siteType <- data_newcategories %>% 
    group_by(display_charType, display_siteType) %>% 
    summarize(numResults_siteType = sum(numResults)) %>% 
    ungroup()
  
  data <- numResults_siteType %>% 
    left_join(totalnumResults, by='display_charType') %>% 
    mutate(percentResults = (numResults_siteType/totalnumResults)*100) %>% 
    mutate(charTypeLabels = paste0(display_charType, "\n (", totalnumResults, ")"))
  
  site_order <- c('Facility', 'Groundwater', 'Lake', 'Marine', 'Stream', 'Other')
  data_order <- arrange(data, totalnumResults)
  char_order <- unique(data_order$charTypeLabels)
  
  data <- data %>% 
    mutate(display_siteType = factor(display_siteType, levels = site_order, ordered = TRUE)) %>% 
    mutate(charTypeLabels = factor(charTypeLabels, levels = char_order, ordered = TRUE)) %>% 
    arrange(display_siteType) #otherwise, the bars show up in the wrong order (but legend correctly)
  
  site_cols <- c('#1f78b4','#33a02c','#fb9a99',
                 '#6a3d9a','#80b1d3','#ff7f00')
  
  if(type == "percent"){
    
    # percent plot
    records_plot <- ggplot(data, aes(x = charTypeLabels, 
                                     y = percentResults, 
                                     fill = display_siteType)) + 
      ggtitle('Distribution of WQP Records by Site Types and Characteristic Groups') +
      ylab('Percent Site Type') + xlab('Characteristic Group') +
      geom_bar(stat="identity") + 
      coord_flip() + 
      theme_classic() + 
      scale_fill_manual(values = site_cols, name = "Site Type") +
      theme(axis.text.y = element_text(size = 8)) 
    
  } else if(type == "absolute"){
    # absolute number of records plot
    
    records_plot <- ggplot(data, aes(x = charTypeLabels, 
                                     y = numResults_siteType, 
                                     fill = display_siteType)) + 
      ggtitle('Distribution of WQP Records by Site Types and Characteristic Groups') +
      ylab('Number of Records') + xlab('Characteristic Group') +
      geom_bar(stat="identity") + 
      coord_flip() + 
      theme_classic() + 
      scale_fill_manual(values = site_cols, name = "Site Type") +
      theme(axis.text.y = element_text(size = 8)) 
  }
  
  return(records_plot)
}

# spark lines + barchart
plotSparklinesBarchart <- function(startYr = 1950, endYr = as.numeric(format(Sys.time(), "%Y")),
                                   allCountsFile = 'data/wqp_database_counts.csv'){
  
  library(dplyr)
  library(ggplot2)
  library(gtable)
  library(grid)
  
  temporal_data <- read.csv(allCountsFile, stringsAsFactors = FALSE)
  # replace NA values w/ 0
  temporal_data$numSites[is.na(temporal_data$numSites)] <- 0
  temporal_data$numResults[is.na(temporal_data$numResults)] <- 0
    
  total_bar <- temporal_data %>% 
    group_by(siteType) %>% 
    summarize(numSites = sum(numSites),
              numResults = sum(numResults)) %>% 
    mutate(characteristicType = "Total") %>% 
    select(characteristicType, everything()) %>% 
    ungroup()
  
  results_data <- temporal_data %>% 
    select(-startDate, -endDate) %>% 
    filter(characteristicType != "Not Assigned") %>% 
    rbind(total_bar)
  
  # group characteristic types
  data <- temporal_data %>% 
    rowwise() %>% 
    mutate(year = as.numeric(format(as.Date(startDate), "%Y"))) %>% 
    mutate(display_charType = displayChars(characteristicType)) %>% 
    ungroup() %>% #end the rowwise function
    filter(year >= startYr) %>%
    filter(year < endYr) %>%
    filter(display_charType != "NA") %>% 
    group_by(display_charType, year) %>% 
    summarize_each(funs = 'sum', c(numSites, numResults)) %>% 
    ungroup()
  
  total_sparkline <- data %>% 
    group_by(year) %>% 
    summarize(numSites = sum(numSites), 
              numResults = sum(numResults)) %>% 
    mutate(display_charType = "Total") %>% 
    select(display_charType, everything()) %>% 
    ungroup()
  
  data <- rbind(data, total_sparkline)
  
  # get bar chart for number of records
  records_plot <- createRecordsBarchart(results_data) +
    guides(fill = guide_legend(title = NULL, nrow = 1, ncol = 6))
  tmp <- ggplot_gtable(ggplot_build(records_plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  records_plot_legend <- tmp$grobs[[leg]]
  
  records_plot <- records_plot +
    theme(legend.position = "none",  
          plot.title = element_blank())
  
  # order the char types for temporal data, the same as the number of records data
  results_data_parsed <- records_plot$data 
  charType_order <- unlist(lapply(strsplit(rev(levels(results_data_parsed$charTypeLabels)), split = "\n"),'[', 1))
  charType_order <- c("Total", charType_order)
  data <- data %>% 
    mutate(display_charType = factor(display_charType, levels = charType_order, ordered = TRUE))
  
  # create the spark lines
  x_axis_values <- seq(startYr, endYr - (endYr %% 10), by=10)
  x_axis_labels <- c(paste("\n", x_axis_values))
  sparklines <- ggplot(data = data, aes(x = year, y = numResults)) +
    geom_line() +
    facet_grid(display_charType ~ ., scales = 'free_y') + 
    scale_x_continuous(breaks = x_axis_values, labels = x_axis_labels) +
    theme_bw() + 
    theme(axis.line = element_blank(), 
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(margin = margin(b = -1, unit = 'line'), 
                                     face = 'italic', size = 8, hjust = 0, 
                                     vjust = 0, angle = 45),
          axis.title.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(), 
          strip.background = element_blank(),
          strip.text = element_blank(),
          plot.margin=unit(c(0.6,1.4,0.9,0.2),"lines")) #top, right, bottom, left
   
  # put the bar chart and spark lines together
  g <- ggplotGrob(records_plot)
  g <- gtable_add_cols(g, unit(5,"cm"))
  g <- gtable_add_grob(g, ggplotGrob(sparklines),
                       t=1, l=ncol(g), b=5, r=ncol(g))
  g <- gtable_add_rows(g, unit(1.5,"cm"))
  g <- gtable_add_grob(g, records_plot_legend,
                       t=7, l=4, b=7, r=4)
  
  grid.newpage()
  png('figures/records_w_sparklines.png', width = 600)
  grid.draw(g)
  dev.off()
}


