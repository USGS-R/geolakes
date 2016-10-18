get_all_us_secchi = function(outfile, characteristicNames, siteTypes, stateCode,
                             startDateLo='1980-01-01', startDateHi='2016-01-01'){
  
  metadata <- NULL
  secchi <- NULL
  for(state in stateCode){
    meta.state <- whatWQPsites(stateCode=state, 
                             characteristicName=characteristicNames, 
                             siteType=siteTypes)
    
    secchi.state <-  readWQPdataPaged(startDateLo = startDateLo, 
                                       startDateHi = startDateHi, 
                                       stateCode=state, 
                                       characteristicName=characteristicNames, 
                                       siteType=siteTypes)
    
    metadata <- bind_rows(metadata, meta.state)
    secchi <- bind_rows(secchi, secchi.state)
  }
  
  save(secchi, metadata, file=outfile)
  
}

readWQPdataPaged = function(..., startDateLo='1950-01-01', startDateHi='2020-01-01', stride='365 days'){
  
  params = list(..., startDateLo=startDateLo, startDateHi=startDateHi)
  
  if(!('startDateLo' %in% names(params))){
    params$startDateLo = '1900-01-01'
  }
  
  if(!('startDateHi' %in% names(params))){
    params$startDateHi = format(Sys.Date(), '%Y-%m-%d')
  }
  
  starts = seq(as.POSIXct(params$startDateLo), as.POSIXct(params$startDateHi), by=stride)
  
  if(length(starts) < 2){
    ends   = as.POSIXct(params$startDateHi)
  } else {
    ends   = c(starts[2:(length(starts))], as.POSIXct(params$startDateHi))
  }
  
  
  starts = format(starts, '%Y-%m-%d')
  ends   = format(ends, '%Y-%m-%d')
  
  out = list()
  
  for(i in 1:(length(starts)-1)){
    cat('Downloading ', starts[i], ' to ', ends[i], '\n')
    params$startDateLo = starts[i]
    params$startDateHi = ends[i]
    
    out[[i]] = do.call(retryWQP, params)
  }
  
  return(do.call(rbind, out))
}

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

library(dplyr)
library(dataRetrieval)
library(rgeos)
library(rgdal)
library(dplyr)
library(dataRetrieval)
library(RColorBrewer)

characteristicNames = c("Depth, Secchi disk depth", "Depth, Secchi disk depth (choice list)", "Secchi Reading Condition (choice list)", "Secchi depth", "Water transparency, Secchi disc")
siteTypes = "Lake, Reservoir, Impoundment"

# To get the full data set used to produce the figure in the text:
# get_all_us_secchi(outfile="all_secchi_usa.RData", 
#                   characteristicNames, 
#                   siteTypes, 
#                   stateCode = dataRetrieval::stateCd$STATE[1:50],
#                   '1980-01-01', '2016-01-01')

# To get the small subset to test the workflow:
get_all_us_secchi(outfile="sub_secchi_usa.RData", 
                  "Depth, Secchi disk depth", 
                  siteTypes, 
                  dataRetrieval::stateCd$STATE[1:2],
                  '2015-01-01', '2016-01-01')


infile <- "sub_secchi_usa.RData"
