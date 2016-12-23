### functions for retrieving WQP counts and creating the bar chart + sparklines ###

# To run everything, select all lines of code and click "Run" OR source this file.

# allow some of the queries to fail to account for possible network hiccups
retryWQP <- function(..., retries=3){
  
  safeWQP = function(...){
    result = tryCatch({
      readWQPdata(...)
    }, error = function(e) {
      if(e$message == 'Operation was aborted by an application callback'){
        stop(e)
      }
      return(NULL)
    })
    return(result)
  }
  retry = 1
  while (retry < retries){
    result = safeWQP(...)
    if (!is.null(result)){
      retry = retries
    } else {
      message('query failed, retrying')
      retry = retry+1
    }
  }
  return(result)
}

# setup the dataRetrieval function call + format the data it returns
createCountDF <- function(char_type, site_type, start_date, end_date){
  #format dates for WQP call
  start_date <- format(start_date, format = '%Y-%m-%d')
  end_date <- format(end_date, format = '%Y-%m-%d')
  
  d <- retryWQP(characteristicType=char_type, siteType = site_type,
                startDate = start_date, endDate = end_date,
                querySummary = TRUE, retries = 5)
  
  if(is.null(d$`total-site-count`)){ d$`total-site-count` <- NA }
  if(is.null(d$`total-result-count`)){ d$`total-result-count` <- NA }
  
  df <- data.frame(characteristicType = char_type,
                   siteType = site_type,
                   startDate = start_date, 
                   endDate = end_date,
                   numSites = as.numeric(d$`total-site-count`),
                   numResults = as.numeric(d$`total-result-count`),
                   stringsAsFactors = FALSE)
  return(df)
}

# counts of sites and records for each characteristic group, site type, and year in the range specified
getAllRecordsCounts <- function(startYr = 1950, endYr = as.numeric(format(Sys.time(), "%Y")), 
                                charTypes = c('Physical', 'Inorganics, Major, Metals', 'Inorganics, Major, Non-metals', 
                                              'Inorganics, Minor, Metals', 'Inorganics, Minor, Non-metals', 'Not Assigned',
                                              'Nutrient', 'Organics, Other', 'Organics, PCBs', 'Organics, Pesticide', 
                                              'Microbiological', 'Biological', 'Information', 'Sediment', 'Radiochemical', 
                                              'Stable Isotopes', 'Population/Community', 'Toxicity'), 
                                siteTypes = c('Aggregate groundwater use', 'Aggregate surface-water-use', 'Atmosphere', 'Estuary', 
                                              'Facility', 'Glacier', 'Lake, Reservoir, Impoundment', 'Land', 'Not Assigned', 
                                              'Ocean', 'Spring', 'Stream', 'Subsurface', 'Well', 'Wetland'),
                                allCountsFile = 'data/wqp_database_counts.csv',
                                forceRun = FALSE){

  if(file.exists(allCountsFile) && !forceRun){
    return()
  }
  
  char_types <- charTypes
  site_types <- siteTypes
  
  start_dates <- seq(as.Date(paste0(startYr, '-01-01')), 
                     as.Date(paste0(endYr, '-01-01')),
                     by = 'year')
  
  query_combinations <- expand.grid(char_types = char_types, site_types = site_types, 
                                    start_dates = start_dates, stringsAsFactors = FALSE) %>% 
    mutate(end_dates = as.Date(format(start_dates, "%Y-12-31"))) %>% 
    arrange(char_types, site_types)
  
  if(!dir.exists('cache')){dir.create('cache')}
  
  for(i in 1:nrow(query_combinations)){
    char_type <- query_combinations$char_types[i]
    site_type <- query_combinations$site_types[i]
    start_date <- query_combinations$start_dates[i]
    end_date <- query_combinations$end_dates[i]
    
    if(char_type != "Population/Community"){
      sitefile <- paste("counts", char_type, site_type, sep = "_")
    } else {
      sitefile <- paste("counts", "Population-Community", site_type, sep = "_")
    }
    sitefile <- paste0(sitefile, '.csv')
    sitefilepath <- file.path('cache', sitefile)
    
    new_site <- i == 1 || site_type != query_combinations$site_types[i-1]
    if(new_site){
      counts_df <- data.frame()
    }
    
    if(file.exists(sitefilepath) && !forceRun){
      next
    } 
    
    counts_df_i <- createCountDF(char_type, site_type, start_date, end_date)
    counts_df <- rbind(counts_df, counts_df_i)
    
    lastOfSiteType <- site_type != query_combinations$site_types[i+1] || i == nrow(query_combinations)
    if(lastOfSiteType){
      write.csv(counts_df, sitefilepath, row.names = FALSE)
    }
    
    print(paste(char_type, site_type, format(start_date, '%Y'), sep=" >>> "))
  }
  
  n_unique <- query_combinations %>% 
    select(char_types, site_types) %>% 
    unique() %>% nrow()
  
  files <- file.path('cache', list.files('cache', pattern = "counts"))
  if(length(files) == n_unique){
    counts_all_list <- lapply(files, read.csv, stringsAsFactors = FALSE)
    counts_all_df <- do.call(rbind, counts_all_list)
    
    if(!dir.exists('data')){dir.create('data')}
    write.csv(counts_all_df, allCountsFile, row.names = FALSE)
  }
  
}

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
    mutate(charTypeLabels = paste0(display_charType, "\n (n=", prettyNum(totalnumResults, 
                                                                         big.mark=",", 
                                                                         scientific=FALSE,
                                                                         preserve.width="none"), ")"))
  
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
  
  if(!dir.exists('figures')){dir.create('figures')}
  
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
                       t=2, l=ncol(g), b=8, r=ncol(g))
  g <- gtable_add_rows(g, unit(1.5,"cm"))
  g <- gtable_add_grob(g, records_plot_legend,
                       t=11, l=4, b=11, r=4)
  
  grid.newpage()
  png('figures/records_w_sparklines.png', width = 600)
  grid.draw(g)
  dev.off()
}


## Workflow (using the functions)

# 1. Create a csv file of the WQP counts. 

  # This example is querying data for the last 10 years for Nutrient &  Toxicity characteristic types only, 
  # and will take about 12 minutes. If you keep the defaults (1950, and all characteristic types), it 
  # will take a little over 10 hours. You will see each completed query printed to the console to help 
  # you follow the WQP query process. These queries cache individual files (each site type, characteristic 
  # type combination is one file). Once all files are cached, the function will create a single CSV file 
  # stored in data/. If the function is aborted, you can just rerun getAllRecordsCounts() and it will skip 
  # the queries that already have stored files in cache/.

library(dataRetrieval)
library(dplyr)
library(httr)

getAllRecordsCounts(startYr = 2006, charTypes = c('Nutrient', 'Toxicity'))

# 2. Create the bar chart and sparklines figure.

  # Once the data/wqp_database_counts.csv file exists, you can use the plotting functions. This function will 
  # create the the barchart with sparklines figure and save it as a PNG in figures/. 

library(dplyr)
library(ggplot2)
library(gtable)
library(grid)

plotSparklinesBarchart(startYr = 2006)

# You should be able to see the new folders (cache, data, figures) in your working directory:
getwd()
