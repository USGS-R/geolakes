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
  
  if(all(stateCode == "All")){
    secchi.state <-  readWQPdataPaged(startDateLo = startDateLo, 
                                      startDateHi = startDateHi, 
                                      characteristicName=characteristicNames, 
                                      siteType=siteTypes,
                                      stride=stride)
    secchi.state <- secchi.state %>% 
      left_join(unit.map, by='units') %>% 
      mutate(secchi=value*convert) %>% 
      filter(!is.na(secchi)) 
    
    secchi <- secchi.state
  } else {
  
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
          mutate(secchi=value*convert) %>% 
          filter(!is.na(secchi)) 
        
        secchi <- bind_rows(secchi, secchi.state)
      }
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
      chunk.call = do.call(readWQPdata, params)
    
      if(!is.null(chunk.call)){
        meta <- attr(chunk.call, "siteInfo") %>%
          select(MonitoringLocationIdentifier, dec_lat_va, dec_lon_va, StateCode)
        
        chunk.call <- rename(chunk.call, 
                             Date=ActivityStartDate, 
                             value=ResultMeasureValue, 
                             units=ResultMeasure.MeasureUnitCode) %>% 
          left_join(meta, by="MonitoringLocationIdentifier") %>%
          rename(wqx.id=MonitoringLocationIdentifier) %>%
          mutate(StateCode = as.character(StateCode),
                 ActivityTypeCode = as.character(ActivityTypeCode),
                 ActivityMediaName = as.character(ActivityMediaName),
                 ResultStatusIdentifier = as.character(ResultStatusIdentifier)) %>%
          select(Date, value, units, wqx.id, 
                 dec_lat_va, dec_lon_va, 
                 StateCode, 
                 ActivityTypeCode,ActivityMediaName,
                 ResultStatusIdentifier)
        
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
                           StateCode = character(),
                           ActivityTypeCode = character(),
                           ActivityMediaName = character(),
                           ResultStatusIdentifier = character())
  }
  
  return(out.data)

}



library(dplyr)
library(dataRetrieval)
library(lubridate)
library(ggplot2)

characteristicNames = c("Depth, Secchi disk depth", 
                        "Depth, Secchi disk depth (choice list)", 
                        "Secchi Reading Condition (choice list)", 
                        "Secchi depth", 
                        "Water transparency, Secchi disc")
siteTypes = "Lake, Reservoir, Impoundment"

# To get the full data set used to produce the figure in the text:
# This takes roughly 15 minutes
# get_us_secchi(outfile="all_secchi_usa_long.rds",
#               characteristicNames,
#               siteTypes,
#               stateCode = "All",
#               startDateLo = '1900-01-01',
#               startDateHi = '2016-01-01',
#               stride = "20 years")

# To get the small subset to test the workflow:
# This takes roughly 3 minutes
get_us_secchi(outfile="sub_secchi_AL_MN.rds",
              characteristicNames,
              siteTypes,
              stateCode = c("01","27"), #AL and MN
              startDateLo = '2000-01-01', 
              startDateHi = '2016-01-01',
              stride = "20 years")

infile <- "all_secchi_usa_long.rds"
secchi.data <- readRDS(infile)

regions <- data.frame(STATE_NAME = c('Montana', 'Wyoming', 'Idaho', 'Washington', 'Oregon', 'California', 'Nevada',
                                     'Arizona', 'New Mexico', 'Colorado', 'Utah'), group='West', stringsAsFactors = FALSE) %>% 
  rbind(data.frame(STATE_NAME=c('Ohio', 'Indiana', 'Illinois', 'Wisconsin', 'Missouri', 'Iowa', 'Minnesota', 'Kansas',
                                'Nebraska', 'South Dakota', 'North Dakota', 'Michigan'), group='Midwest', stringsAsFactors = FALSE)) %>% 
  rbind(data.frame(STATE_NAME=c('Maine', 'New Hampshire', 'Vermont', 'Massachusetts', 'Rhode Island', 'Connecticut', 
                                'New Jersey', 'Pennsylvania', 'New York'), group='Northeast', stringsAsFactors = FALSE)) %>% 
  rbind(data.frame(STATE_NAME=c('Florida', 'Georgia', 'Louisiana', 'Arkansas', 'Oklahoma', 'Texas', 'South Carolina', 
                                'North Carolina', 'Virginia', 'Kentucky', 'Tennessee', 'West Virginia', 'Maryland', 
                                'Delaware', 'Alabama', 'Mississippi'), group='South', stringsAsFactors = FALSE))

secchi.filtered <- secchi.data %>%
  filter(ActivityTypeCode %in% c("Field Msr/Obs","Sample-Routine",
                                 "Field Msr/Obs-Portable Data Logger",
                                 "Sample-Composite Without Parents",
                                 "Sample-Field Split","Sample-Other")) %>%
  filter(ActivityMediaName %in% c("Water","Other","Habitat")) %>%
  filter(!is.na(StateCode)) %>%
  left_join(stateCd, by=c("StateCode" = "STATE")) %>%
  left_join(regions, by="STATE_NAME") %>%
  filter(!is.na(group)) %>%
  mutate(week = lubridate::week(Date)) 

group_by(secchi.filtered, group) %>% summarize(nSites = length(unique(wqx.id)))
table(select(secchi.filtered, group))

secchi.data.1 <- secchi.filtered %>% 
  filter(week > 13 & week < 47) %>%
  group_by(week, group) %>%
  summarize(med = median(secchi, na.rm=TRUE)) %>%
  filter(!is.na(group)) 

secchi.plot <- ggplot(data=secchi.data.1) +
  geom_line(aes(x=week, y=med, color=group), size=2) +
  scale_color_manual(values=c(West = '#283044', 
                              South = '#F7CB65',
                              Northeast='#2c7fb8', 
                              Midwest = '#7FCDBB')) +
  ylab("Secchi depth (m)") +
  xlab("") +
  theme_bw() + 
  theme(legend.title = element_blank()) +
  scale_x_continuous(labels = c("May","Jul", "Sep", "Nov"),
                     breaks = c(17.14286, 25.85714, 34.71429, 43.42857))

ggsave(plot = secchi.plot, filename = "secchi.png")