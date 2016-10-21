### functions for retrieving constituent data from WQP and mapping its spatial distribution ###

# To run everything, select all lines of code and click "Run" OR source this file.

get_mutate_HUC8s <- function(wfs = "http://cida.usgs.gov/gdp/geoserver/wfs", 
                             feature = "derivative:wbdhu8_alb_simp", 
                             plot_CRS = "+init=epsg:2163"){
  destination <- tempfile(pattern = 'huc_shape', fileext='.zip')
  query <- sprintf('%s?service=WFS&request=GetFeature&typeName=%s&outputFormat=shape-zip&version=1.0.0', wfs, feature)
  file <- GET(query, write_disk(destination, overwrite=T), progress())
  shp.path <- tempdir()
  unzip(destination, exdir = shp.path)
  layer <- strsplit(feature,'[:]')[[1]][2]
  hucs <- readOGR(shp.path, layer=layer) %>% 
    spTransform(CRS(plot_CRS))
  unlink(destination)
  return(hucs)
}

get_wqp_data <- function(charNames, 
                         siteType = NULL, 
                         plot_CRS = "+init=epsg:2163"){
  
  # setup list of arguments for whatWQPsites function
  char_list <- sapply(charNames, list)
  names(char_list) <- rep('characteristicName', length(char_list))
  site_list <- list(siteType = siteType)
  args <- append(char_list, site_list)
  
  sites <- do.call(whatWQPsites, args) %>% 
    select(LongitudeMeasure, LatitudeMeasure) %>%
    rename(lon=LongitudeMeasure, lat=LatitudeMeasure) %>% 
    data.frame %>% 
    mutate_wqp_site(plot_CRS)
  
  return(sites)
}

mutate_wqp_site <- function(sites, plot_CRS){
  SpatialPoints(sites, proj4string=CRS("+proj=longlat + datum=wgs84")) %>% 
    spTransform(CRS(plot_CRS))
}

plot_huc_sites <- function(hucs, sites, figure_name, missingDataCol = 'grey90'){
  
  if(!dir.exists('figures')){dir.create('figures')}
  filename <- file.path('figures', figure_name)
  
  point.in = gContains(hucs, sites, byid=TRUE)
  counts.by.id = colSums(point.in) %>% log
  
  counts.by.id[!is.finite(counts.by.id)] <- NA
  
  ## -- color markers --
  
  key.bins = c(NA, log(axTicks(1, axp=c(1, max(exp(counts.by.id)), 3), usr=c(1,3), log=TRUE)))
  bins = key.bins
  pal = colorRampPalette(brewer.pal(9, 'YlGnBu'))(length(bins)-1)
  key.cols = colorRampPalette(brewer.pal(9, 'YlGnBu'))(length(key.bins)-1)
  
  pal <- c(missingDataCol, pal) # 0 is grey
  key.cols <- c(missingDataCol, key.cols)  # 0 is grey
  key.text <- c(0, tail(exp(key.bins), -1))
  #get closest bin
  bin = unname(sapply(counts.by.id, function(x) ifelse(is.na(x),1,which.min(abs(x-bins)))))
  cols = rep(NA, length(counts.by.id))
  cols[!is.na(bin)] = pal[bin[!is.na(bin)]]
  
  png(filename = filename, width = 7, heigh=5, res=150, units = 'in')
  layout(matrix(data = c(1,1,1,1,1,1,2), ncol=1))
  par(mai = c(0,0,0,0), omi = c(0,0,0,0))
  par(mai = c(0,0,0,0), omi = c(0,0,0,0))
  
  xlim <- c(-1534607.9,2050000.1) # specific to the transform we are using
  ylim <- c(-2072574.6,727758.7)
  
  plot(hucs, add = FALSE, col = cols, border = NA, lwd = 0.5, xlim = xlim, ylim = ylim)
  #plot(sites, add=TRUE, col='red',pch=20,cex=0.2)
  
  # secondary plot for color legend
  plot(c(NA,NA),c(NA,NA), axes=F, ylim=c(0,1),xlim=c(0,1))
  bin.w = 0.05
  spc = .02
  text(.1,.5, 'Number of sites', pos=3, offset=0.1)
  for(i in 1:length(key.cols)){
    x1 = 0.20+(i-1)*(bin.w+spc)
    graphics::rect(x1, 0.3, x1+bin.w, 0.8, col=key.cols[i], lwd=NA)
    text(x1+bin.w/2, y=0.33, labels=key.text[i], pos=1)
  }
  dev.off()
}

## Workflow (using the functions)

# 1. Download the HUC8 spatial polygons. 

library(httr)
library(rgdal)
library(sp)
library(magrittr)

hucs <- get_mutate_HUC8s()

# 2. Get site spatial data from WQP based on characteristic names and site types. 

  # Examples below get data for lake temperatures (stored in WQP under multiple 
  # characteristic names) for lake sites and Phosphorus data for all sites. Each 
  # of these WQP queries can take up to 2 minutes to complete. Possible 
  # siteTypes and characteristicNames can be found through the WQP User Guide. 

library(dataRetrieval)
library(dplyr)
library(sp)

lake_Temp <- get_wqp_data(charNames = c("Temperature, sample",
                                          "Temperature, water",
                                          "Temperature"),
                            siteType = "Lake, Reservoir, Impoundment")

all_P <- get_wqp_data(charNames = 'Phosphorus')

# 3. Create a map for each set of spatial data.

  # You will need to use the hucs object created in step 1, and whatever spatial
  # data you downloaded from WQP in step 2.

library(rgeos)
library(RColorBrewer)

plot_huc_sites(hucs = hucs, sites = lake_Temp, figure_name = 'lake_temp_map.png')
plot_huc_sites(hucs = hucs, sites = all_P, figure_name = 'all_p_map.png') # takes ~ 1 min

# You should be able to see the figures at this location:
file.path(getwd(), 'figures')
