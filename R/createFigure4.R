### functions for retrieving WQP secchi data and creating a  ###

# To run everything, select all lines of code and click "Run" OR source this file.

# get secchi data from WQP and save as .RDS file
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
  
  params <- list(startDateLo = startDateLo, 
                 startDateHi = startDateHi, 
                 characteristicName=characteristicNames, 
                 stride=stride)
  if(siteTypes != "All"){
    params <- append(params, list(siteType = siteTypes))
  }
  
  fetch_bind <- function(secchi, params){
    secchi.state <-  do.call(readWQPdataPaged, params)
    
    if(!is.null(secchi.state)){
      secchi.state <- secchi.state %>% 
        left_join(unit.map, by='units') %>% 
        mutate(secchi=value*convert) 
      
      secchi <- bind_rows(secchi, secchi.state)
    }
    return(secchi)
  }
  if(all(stateCode == "All")){
    secchi <- fetch_bind(secchi, params)
  } else {
    for(state in stateCode){
      secchi <- fetch_bind(secchi, params = append(params, list(statecode = state)))
    }
  }
  
  saveRDS(secchi, file=outfile)
  
}

# incrementally query WQP using the stride arg (called from get_us_secchi)
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
          select(MonitoringLocationIdentifier, dec_lat_va, dec_lon_va, StateCode, MonitoringLocationTypeName)
        
        chunk.call <- rename(chunk.call, 
                             Date=ActivityStartDate, 
                             value=ResultMeasureValue, 
                             units=ResultMeasure.MeasureUnitCode) %>% 
          left_join(meta, by="MonitoringLocationIdentifier") %>%
          rename(wqx.id=MonitoringLocationIdentifier) %>%
          mutate(StateCode = as.character(StateCode),
                 ActivityTypeCode = as.character(ActivityTypeCode),
                 ActivityMediaName = as.character(ActivityMediaName),
                 ResultStatusIdentifier = as.character(ResultStatusIdentifier),
                 MonitoringLocationTypeName = as.character(MonitoringLocationTypeName)) %>%
          select(Date, value, units, wqx.id, 
                 dec_lat_va, dec_lon_va, 
                 StateCode, 
                 ActivityTypeCode,ActivityMediaName,
                 ResultStatusIdentifier, MonitoringLocationTypeName)
        
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

## Workflow (using the functions)

library(dplyr)
library(dataRetrieval)
library(lubridate)
library(ggplot2)
library(maps)
library(grid)
library(gridExtra)

# 1. Download secchi data for all of US or subset

characteristicNames = c("Depth, Secchi disk depth", 
                        "Depth, Secchi disk depth (choice list)", 
                        "Secchi Reading Condition (choice list)", 
                        "Secchi depth", 
                        "Water transparency, Secchi disc")

# To get the full data set used to produce the figure in the text:
# This takes roughly 1 hour to complete:
# get_us_secchi(outfile="all_secchi_usa.rds",
#               characteristicNames,
#               siteTypes = "Lake, Reservoir, Impoundment",
#               stateCode = "All",
#               startDateLo = '1900-01-01',
#               startDateHi = '2016-01-01',
#               stride = "20 years")

# To get the small subset to test the workflow:
# This takes roughly 3 minutes
get_us_secchi(outfile="sub_secchi_AL_MN.rds",
              characteristicNames,
              siteTypes = "Lake, Reservoir, Impoundment",
              stateCode = c("01","27"), #AL and MN
              startDateLo = '2000-01-01',
              startDateHi = '2016-01-01',
              stride = "20 years")
 

infile <- "datasets/all_secchi_usa.rds"
secchi.data <- readRDS(infile)
# 2. Group states into regions and get only df with necessary info

use.types <- c('Lake','Reservoir', 'Lake, Reservoir, Impoundment', 'Great Lake')

regions <- data.frame(STATE_NAME = c('Montana', 'Wyoming', 'Idaho', 'Washington', 'Oregon', 'California', 'Nevada',
                                     'Arizona', 'New Mexico', 'Colorado', 'Utah'), group='West', stringsAsFactors = FALSE) %>%
  rbind(data.frame(STATE_NAME=c('Ohio', 'Indiana', 'Illinois', 'Wisconsin', 'Missouri', 'Iowa', 'Minnesota', 'Kansas',
                                'Nebraska', 'South Dakota', 'North Dakota', 'Michigan'), group='Midwest', stringsAsFactors = FALSE)) %>%
  rbind(data.frame(STATE_NAME=c('Maine', 'New Hampshire', 'Vermont', 'Massachusetts', 'Rhode Island', 'Connecticut',
                                'New Jersey', 'Pennsylvania', 'New York'), group='Northeast', stringsAsFactors = FALSE)) %>%
  rbind(data.frame(STATE_NAME=c('Florida', 'Georgia', 'Louisiana', 'Arkansas', 'Oklahoma', 'Texas', 'South Carolina',
                                'North Carolina', 'Virginia', 'Kentucky', 'Tennessee', 'West Virginia', 'Maryland',
                                'Delaware', 'Alabama', 'Mississippi'), group='South', stringsAsFactors = FALSE))
regions$region <- tolower(regions$STATE_NAME)
regions <- rename(regions, area = group)

unit.map <- data.frame(units=c('m','in','ft','cm',"mm","mi", NA), 
                       convert = c(1,0.0254,0.3048,0.01, 0.001, 1609.34, NA), 
                       stringsAsFactors = FALSE)

is.dup <- secchi.data %>% filter(MonitoringLocationTypeName %in% use.types, wqx.id %in% wqx.pip) %>% 
  select(Date, dec_lat_va, dec_lon_va, value) %>% duplicated
non.dups <- secchi.data %>% filter(MonitoringLocationTypeName %in% use.types, wqx.id %in% wqx.pip) %>% .[!is.dup, ] 
non.qas <- non.dups %>% 
  filter(ActivityTypeCode %in% c("Field Msr/Obs","Sample-Routine",
                                 "Field Msr/Obs-Portable Data Logger",
                                 "Sample-Composite Without Parents",
                                 "Sample-Field Split","Sample-Other")) %>%
  filter(ActivityMediaName %in% c("Water","Other","Habitat"))



non.qas$units <- gsub(" ", "", non.qas$units)
secchi.plot <- non.qas %>%
  filter(units %in% unit.map$units) %>%
  select(-convert, -secchi) %>%
  left_join(unit.map, by="units") %>% filter(!is.na(units)) %>%
  mutate(secchi = value*convert) %>%
  mutate(secchi = abs(secchi)) %>% 
  rename(STATE = StateCode) %>% 
  left_join(stateCd, by=c("STATE")) %>% 
  left_join(regions, by="STATE_NAME") %>%
  mutate(week = lubridate::week(Date)) %>%
  filter(!is.na(area)) %>% filter(secchi > 0, secchi < 46) %>% 
  filter(week > 13 & week < 47) %>%
  group_by(week, area) %>%
  summarize(med = median(secchi, na.rm=TRUE),
            q25 = quantile(secchi, na.rm=TRUE, probs = .25),
            q75 = quantile(secchi, na.rm=TRUE, probs = .75)) 


# 3. Plot secchi over time for each region and add a map to show regions

col.scheme <- c(West = '#283044',
                South = '#F7CB65',
                Northeast='#2c7fb8',
                Midwest = '#7FCDBB')

secchi.plot <- ggplot(data=secchi.data.plot) +
  geom_line(aes(x=week, y=med, color=area), size=1) +
  scale_color_manual(values=col.scheme) +
  ylab("Secchi depth (m)") +
  xlab("") +
  theme_bw() +
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(size = 1, color="black")) +
  expand_limits(y = 0) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,5.5)) +
  scale_x_continuous(labels = c("May","Jul", "Sep", "Nov"),
                     breaks = c(17.14286, 25.85714, 34.71429, 43.42857))

all_states <- map_data("state")
fill.states <- left_join(all_states, regions, by="region")

map.legend <- ggplot(data=fill.states) +
  geom_polygon(aes(x=long, y=lat, group = group, fill=area),
               colour="white", size = 0.1) +
  coord_map(proj='bonne', param=45) +
  theme_bw() +
  theme(legend.position="none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        text = element_blank(),
        line = element_blank(),
        panel.margin = unit(c(0, 0, 0, 0), "cm"),
        plot.background = element_rect(fill = "transparent",colour = NA))  +
  scale_fill_manual(values = col.scheme) +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_continuous(expand = c(0,0))

g2 = ggplotGrob(map.legend)

final.plot <- secchi.plot +
  annotation_custom(grob = g2,
                    xmin=30, xmax=52,
                    ymin=4, ymax=5.5)
final.plot
ggsave(plot = final.plot, filename = "figures/secchi.pdf", width = 3.74, height = 4.14)

