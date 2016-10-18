# library(dataRetrieval)
# library(TeachingDemos)
# library(lubridate)
# library(maptools)
# library(rgeos)
# library(dplyr)
# library(maps)
# library(zyp)
# library(sp)

FIGURE_all_us_secchi = function(infile){
  
  load(infile)
  
  #secchi = readRDS('scratch/all_secchi_usa.rds')
  secchi$year = year(secchi$Date)
  secchi$month = month(secchi$Date)
  secchi$week  = week(secchi$Date)
  secchi$week2  = 2*floor(week(secchi$Date)/2)
  
  #metadata = readRDS('scratch/all_secchi_metadata.rds')
  metadata$STATE = NULL
  
  ### use maps package data to split up secchi observations by state
  usa <- map("state", fill = TRUE)
  IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
  usa <- map2SpatialPolygons(usa, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  state_names = names(usa)
  
  lat = metadata$LatitudeMeasure
  lon = metadata$LongitudeMeasure
  
  not_na = which(!is.na(lat) & !is.na(lon))
  
  xy = cbind(lon, lat)
  
  pts = SpatialPoints(xy[not_na,], proj4string=CRS(proj4string(usa)))
  
  states = rep(NA, length(lat))
  
  states[not_na] = state_names[over(pts, usa, fn = NULL, returnList = FALSE)]
  
  metadata$statename = states
  
  # statecounts = sort(table(metadata$statename))
  # tokeep = statecounts#[statecounts > 1e4]
  # 
  # for(i in 1:length(tokeep)){
  #   
  #   statesites = subset(metadata, statename == names(tokeep[i]))
  #   statesecchi = subset(secchi, wqx.id %in% statesites$wqx.id)
  #   if(nrow(statesecchi) < 1000){
  #     next
  #   }
  #   
  #   #png(paste0('state_seasonality/', names(tokeep[i]), '.png'), width=1800, height=1000, res=300)
  #   boxplot(secchi~week, statesecchi, main=names(tokeep[i]), ylim=c(0,2*median(statesecchi$secchi, na.rm=TRUE)))
  #   axis(1, at=c(1,13,26,40, 52), tick = TRUE, line = 2, labels = FALSE)
  #   axis(1, at=c(6.5,19.5,32.5,46.5), tick = FALSE, line = 2, labels = c('winter', 'spring', 'summer', 'fall'))
  #   
  #   #dev.off()
  # }
  
  
  regions = list()
  
  regions[['west']] = c('montana', 'wyoming', 'idaho', 'washington', 'oregon', 'california', 'nevada',
                        'arizona', 'new mexico', 'colorado', 'utah')
  regions[['midwest']] = c('ohio', 'indiana', 'illinois', 'wisconsin', 'missouri', 'iowa', 'minnesota', 'kansas',
                           'nebraska', 'south dakota', 'north dakota', 'michigan')
  regions[['northeast']] = c('maine', 'new hampshire', 'vermont', 'massachusetts', 'rhode island', 'connecticut', 
                             'new jersey', 'pennsylvania', 'new york')
  regions[['south']] = c('florida', 'georgia', 'louisiana', 'arkansas', 'oklahoma', 'texas', 'south carolina', 
                         'north carolina', 'virginia', 'kentucky', 'tennessee', 'west virginia', 'maryland', 
                         'delaware', 'alabama', 'mississippi')
  cols = list()
  touse = rainbow(4)
  for(i in 1:length(regions)){
    cols[[i]] = rep(touse[i], length(regions[[i]]))
  }
  cols = unlist(cols)
  
  
  ## Save figure for manuscript
  #dev.off()
  png('figures/secchi_seasons_regionally.png', res=300, width=1800, height=1800)
  for(i in 1:length(regions)){
    statesites = subset(metadata, statename %in% regions[[i]])
    statesecchi = subset(secchi, wqx.id %in% statesites$wqx.id)
    
    weekly = group_by(statesecchi, week) %>% summarize(msecchi = median(secchi, na.rm=TRUE))
    if(i == 1){
      plot(weekly$week, weekly$msecchi, ylim=c(0,6), xlim=c(10,50), ylab='Secchi (m)', xlab='Week of year', col=touse[i])
    }else{
      points(weekly$week, weekly$msecchi, col=touse[i])
    }
    
    lines(weekly$week, smooth(weekly$msecchi), col=touse[i])
    cat(sprintf('%s: %g\n', names(regions)[i], median(statesecchi$secchi, na.rm=TRUE)))
  }
  
  subplot(plot(usa[unlist(regions), ], col=cols), 
          x=grconvertX(c(0.0,0.35), from='npc'),
          y=grconvertY(c(0.75,1), from='npc'),
          type='fig', pars=list( mar=c(0,0,0,0)+0.1))
  
  dev.off()

}