## stacked bar plot where number of records are plotted by site type and characteristic type

createRecordsBarchart <- function(records_data, type = "percent"){
  library(dplyr)
  library(ggplot2)
  
  data_newcategories <- records_data %>% 
    rowwise() %>% 
    mutate(display_siteType = displaySites(siteType)) %>% 
    mutate(display_charType = displayChars(characteristicType))
  
  totalnumResults <- data_newcategories %>% 
    group_by(display_charType) %>% 
    summarize(totalnumResults = sum(numResults))
  
  numResults_siteType <- data_newcategories %>% 
    group_by(display_charType, display_siteType) %>% 
    summarize(numResults_siteType = sum(numResults))
  
  data <- numResults_siteType %>% 
    left_join(totalnumResults, by='display_charType') %>% 
    mutate(percentRecords = (numResults_siteType/totalnumResults)*100) %>% 
    mutate(charTypeLabels = paste0(display_charType, "\n (", totalnumResults, ")")) %>% 
    ungroup()
  
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
                                     y = percentRecords, 
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
