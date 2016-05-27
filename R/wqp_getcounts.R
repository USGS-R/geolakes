# getting counts of sites and records for each characteristic group, site type, and year

library(dataRetrieval)
library(httr)

char_types <- c('Physical', 'Inorganics, Major, Metals', 'Inorganics, Major, Non-metals', 
                'Inorganics, Minor, Metals', 'Inorganics, Minor, Non-metals', 'Not Assigned',
                'Nutrient', 'Organics, Other', 'Organics, PCBs', 'Organics, Pesticide', 
                'Microbiological', 'Biological', 'Information', 'Sediment', 'Radiochemical', 
                'Stable Isotopes', 'Population/Community', 'Toxicity')

site_types <- c('Aggregate groundwater use', 'Aggregate surface-water-use', 'Atmosphere', 'Estuary', 
                'Facility', 'Glacier', 'Lake, Reservoir, Impoundment', 'Land', 'Not Assigned', 
                'Ocean', 'Spring', 'Stream', 'Subsurface', 'Well', 'Wetland')

start_dates <- seq(as.Date('1800-01-01'), 
                   as.Date(paste0(format(Sys.Date(), "%Y"), '-01-01')),
                   by = 'year')
add_yr <- seq(tail(start_dates,1), length = 2, by = '1 year')[2]
end_dates <- c(start_dates[-1], add_yr) -1 # end_dates are December 31st of following yr
dates_df <- data.frame(start_date = start_dates, end_date = end_dates, stringsAsFactors = FALSE)

createCountDF <- function(char_type, site_type, start_date, end_date){
  #format dates for WQP call
  start_date <- format(start_date, format = '%m-%d-%Y')
  end_date <- format(end_date, format = '%m-%d-%Y')
  
  d <- retryWQP(characteristicType=char_type, siteType = site_type,
                startDate = start_date, endDate = end_date, querySummary = TRUE,
                retries = 5)
  
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

retryWQP <- function(..., retries=3){
  
  safeWQP = function(...){
    result = tryCatch({
      readWQPdata(...)
    }, error = function(e) {
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

tmp <- tempdir()

counts_all_list <- lapply(char_types, dates_df = dates_df, tmp = tmp,
                          FUN = function(char_type, dates_df, tmp){
                            counts_char_list <- lapply(site_types, char_type = char_type, dates_df = dates_df,
                                                       FUN = function(site_type, char_type, dates_df){
                                                         counts_site_list <- apply(dates_df, MARGIN = 1, char_type = char_type, site_type = site_type,
                                                                                   FUN = function(date_vec, char_type, site_type){
                                                                                     print(paste0(char_type, ": ", site_type, ", ", format(as.Date(date_vec[1]), "%Y")))
                                                                                     return(createCountDF(char_type, site_type, 
                                                                                                          date_vec['start_date'], 
                                                                                                          date_vec['end_date']))
                                                                                   })
                                                         counts_site_df <- do.call(rbind, counts_site_list)
                                                         write.csv(counts_site_df, paste(tmp, "counts", "_", char_type, 
                                                                                         "_", site_type, ".csv"))
                                                         return(counts_site_df)
                                                       })
                            counts_char_df <- do.call(rbind, counts_char_list)
                            return(counts_char_df)
                          })


files <- file.path(tmp, list.files(tmp, pattern = "counts"))
counts_all_list <- lapply(files, read.csv, stringsAsFactors = FALSE)

counts_all_df <- do.call(rbind, counts_all_list)

# write.csv(counts_all_df, 'wqp_temporal_test.csv', row.names = FALSE)
