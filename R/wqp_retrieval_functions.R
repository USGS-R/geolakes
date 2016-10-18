# functions for retrieving WQP counts
# counts of sites and records for each characteristic group, site type, and year

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

getAllRecordsCounts <- function(startYr = 1950, endYr = as.numeric(format(Sys.time(), "%Y")), 
                                allCountsFile = 'data/wqp_database_counts.csv',
                                forceRun = FALSE){
  
  library(dataRetrieval)
  library(dplyr)
  library(httr)

  if(file.exists(allCountsFile) && !forceRun){
    return()
  }
  
  char_types <- c('Physical', 'Inorganics, Major, Metals', 'Inorganics, Major, Non-metals', 
                  'Inorganics, Minor, Metals', 'Inorganics, Minor, Non-metals', 'Not Assigned',
                  'Nutrient', 'Organics, Other', 'Organics, PCBs', 'Organics, Pesticide', 
                  'Microbiological', 'Biological', 'Information', 'Sediment', 'Radiochemical', 
                  'Stable Isotopes', 'Population/Community', 'Toxicity')
  
  site_types <- c('Aggregate groundwater use', 'Aggregate surface-water-use', 'Atmosphere', 'Estuary', 
                  'Facility', 'Glacier', 'Lake, Reservoir, Impoundment', 'Land', 'Not Assigned', 
                  'Ocean', 'Spring', 'Stream', 'Subsurface', 'Well', 'Wetland')
  
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
