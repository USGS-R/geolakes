get_us_secchi = function(outfile, 
                         characteristicNames, 
                         siteTypes, 
                         stateCode,
                         startDateLo='1980-01-01', 
                         startDateHi='2016-01-01',
                         stride = "10 years"){
  
  unit.map <- data.frame(units=c('m','in','ft','cm', NA), 
                         convert = c(1,0.0254,0.3048,0.01, NA), 
                         stringsAsFactors = FALSE)
  metadata <- NULL
  secchi <- NULL
  
  for(state in stateCode){
    secchi.state <-  readWQPdataPaged(startDateLo = startDateLo, 
                                       startDateHi = startDateHi, 
                                       statecode=state, 
                                       characteristicName=characteristicNames, 
                                       siteType=siteTypes,
                                      stride=stride)
    if(!is.null(secchi.state)){
      secchi.state <- secchi.state %>% 
        left_join(unit.map, by='units') %>% 
        mutate(secchi=value*convert,
               StateCode = as.character(StateCode)) %>% 
        filter(!is.na(secchi)) %>% 
        select(Date, wqx.id, secchi, dec_lat_va, dec_lon_va, StateCode)
      
      secchi <- bind_rows(secchi, secchi.state)
    }
  }
  
  saveRDS(secchi, file=outfile)
  
}

readWQPdataPaged = function(..., 
                            startDateLo='1950-01-01', 
                            startDateHi='2020-01-01', 
                            stride='5 years'){
  
  params = list(..., startDateLo=startDateLo, startDateHi=startDateHi)
  
  if(!('startDateLo' %in% names(params))){
    params$startDateLo = '1900-01-01'
  }
  
  if(!('startDateHi' %in% names(params))){
    params$startDateHi = format(Sys.Date(), '%Y-%m-%d')
  }
  
  if('statecode' %in% names(params)){
    params$statecode = paste0("US:",params$statecode)
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
  
  for(i in seq_along(starts)){
    cat('Downloading ', starts[i], ' to ', ends[i], '\n')
    params$startDateLo = starts[i]
    params$startDateHi = ends[i]
    
    if(starts[[i]] != ends[[i]]){
      chunk.call = do.call(retryWQP, params)
    
      if(!is.null(chunk.call)){
        meta <- attr(chunk.call, "siteInfo") %>%
          select(MonitoringLocationIdentifier, dec_lat_va, dec_lon_va, StateCode)
        
        chunk.call <- rename(chunk.call, 
                             Date=ActivityStartDate, 
                             value=ResultMeasureValue, 
                             units=ResultMeasure.MeasureUnitCode) %>% 
          left_join(meta, by="MonitoringLocationIdentifier") %>%
          rename(wqx.id=MonitoringLocationIdentifier) %>%
          select(Date, value, units, wqx.id, dec_lat_va, dec_lon_va, StateCode)
        
        out[[i]] <- chunk.call       
      }
    }
  }
  
  if(length(out) > 0){
    out.data <- bind_rows(out)
    
  } else {
    out.data <- data.frame(Date = numeric(),
                           value=numeric(),
                           units = character(),
                           wqx.id = character(),
                           dec_lat_va = numeric(),
                           dec_lon_va = numeric(),
                           StateCode = character())
  }
  
  return(out.data)

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
library(lubridate)
library(ggplot2)

characteristicNames = c("Depth, Secchi disk depth", "Depth, Secchi disk depth (choice list)", "Secchi Reading Condition (choice list)", "Secchi depth", "Water transparency, Secchi disc")
siteTypes = "Lake, Reservoir, Impoundment"

lower.48 <- stateCd$STATE[1:50]
lower.48 <- lower.48[!(stateCd$STUSAB[1:50] %in% c("AL","HI"))]
# To get the full data set used to produce the figure in the text:
# get_us_secchi(outfile="all_secchi_usa.rds",
#                   characteristicNames,
#                   siteTypes,
#                   stateCode = lower.48,
#                   '1980-01-01', '2016-01-01')

# To get the small subset to test the workflow:
get_us_secchi(outfile="sub_secchi_WI_MN.rds",
                characteristicNames,
                siteTypes,
                stateCode = c("01","12","27","55"), #AL, FL, WI, and MN
                '2000-01-01', '2016-01-01')

infile <- "sub_secchi_WI_MN.rds"
secchi.data <- readRDS(infile)

regions <- data.frame(STATE_NAME = c('Montana', 'Wyoming', 'Idaho', 'Washington', 'Oregon', 'California', 'Nevada',
                                     'Arizona', 'New Mexico', 'Colorado', 'Utah'), group='west', stringsAsFactors = FALSE) %>% 
  rbind(data.frame(STATE_NAME=c('Ohio', 'Indiana', 'Illinois', 'Wisconsin', 'Missouri', 'Iowa', 'Minnesota', 'Kansas',
                                'Nebraska', 'South Dakota', 'North Dakota', 'Michigan'), group='midwest', stringsAsFactors = FALSE)) %>% 
  rbind(data.frame(STATE_NAME=c('Maine', 'New Hampshire', 'Vermont', 'Massachusetts', 'Rhode Island', 'Connecticut', 
                                'New Jersey', 'Pennsylvania', 'New York'), group='northeast', stringsAsFactors = FALSE)) %>% 
  rbind(data.frame(STATE_NAME=c('Florida', 'Georgia', 'Louisiana', 'Arkansas', 'Oklahoma', 'Texas', 'South Carolina', 
                                'North Carolina', 'Virginia', 'Kentucky', 'Tennessee', 'West Virginia', 'Maryland', 
                                'Delaware', 'Alabama', 'Mississippi'), group='south', stringsAsFactors = FALSE))

secchi.data.1 <- left_join(secchi.data, stateCd, by=c("StateCode" = "STATE")) %>%
  left_join(regions, by="STATE_NAME") %>%
  mutate(week = lubridate::week(Date)) %>% 
  filter(week > 13 & week < 47) %>%
  group_by(week, group) %>%
  summarize(med = median(secchi, na.rm=TRUE), 
            q25 = quantile(secchi, na.rm=TRUE, probs = .25),
            q75 = quantile(secchi, na.rm=TRUE, probs = .75))

secchi.plat <- ggplot(data=secchi.data.1) +
  geom_line(aes(x=week, y=med, color=group), size=2) +
  ylab("Secchi depth (m)") +
  xlab("Week of the year") +
  theme_bw()

