

get_mutate_HUC8s <- function(config){
  destination = tempfile(pattern = 'huc_shape', fileext='.zip')
  query <- sprintf('%s?service=WFS&request=GetFeature&typeName=%s&outputFormat=shape-zip&version=1.0.0', config$wfs, config$feature)
  file <- GET(query, write_disk(destination, overwrite=T), progress())
  shp.path <- tempdir()
  unzip(destination, exdir = shp.path)
  layer <- strsplit(config$feature,'[:]')[[1]][2]
  hucs = readOGR(shp.path, layer=layer) %>% 
    spTransform(CRS(config$plot_CRS))
  unlink(destination)
  return(hucs)
}

get_wqp_data <- function(name, wqp.config, map.config){
  char = strsplit(name,'[_]')[[1]][1]
  type = strsplit(name,'[_]')[[1]][2]
  
  char.names <- wqp.config[[char]]
  if (length(char.names[[1]]) > 1){
    char.names = sapply(char.names[[1]], list)
    names(char.names) <- rep('characteristicName', length(char.names))
  }

  args <- append(char.names, wqp.config[[type]])
  sites <- do.call(whatWQPsites, args) %>% 
    select(LongitudeMeasure, LatitudeMeasure) %>%
    rename(lon=LongitudeMeasure, lat=LatitudeMeasure) %>% data.frame %>% 
    mutate_wqp_site(map.config)

  return(sites)
}

mutate_wqp_site <- function(sites, config){
  SpatialPoints(sites, proj4string=CRS("+proj=longlat + datum=wgs84")) %>% 
    spTransform(CRS(config$plot_CRS))
}

plot_huc_sites <- function(hucs, sites, map.config, figure.name){
  

  png(filename = figure.name, width = 7, heigh=5, res=150, units = 'in')
  layout(matrix(data = c(1,1,1,1,1,1,2), ncol=1))
  par(mai = c(0,0,0,0), omi = c(0,0,0,0))
  
  plot_huc(hucs, sites, map.config)
  key.cols = c(map.config$missing_data, colorRampPalette(brewer.pal(9, 'YlGnBu'))(length(map.config$countBins)-1))
  # secondary plot for color legend
  plot(c(NA,NA),c(NA,NA), axes=F, ylim=c(0,1),xlim=c(0,1))
  bin.w = 0.05
  spc = .02
  text(.1,.5, 'Number of sites', pos=3, offset=0.1)
  for(i in 1:length(key.cols)){
    x1 = 0.20+(i-1)*(bin.w+spc)
    graphics::rect(x1, 0.3, x1+bin.w, 0.8, col=key.cols[i], lwd=NA)
    text(x1+bin.w/2, y=0.33, labels=map.config$countBins[i], pos=1)
  }
  
  dev.off()
}

plot_huc <- function(hucs, sites, map.config){
  point.in = gContains(hucs, sites, byid=TRUE)
  counts.by.id = colSums(point.in) %>% log
  
  counts.by.id[!is.finite(counts.by.id)] <- NA
  
  ## -- color markers --
  key.bins = c(NA, log(tail(map.config$countBins, -1L)))
  bins = key.bins
  pal = colorRampPalette(brewer.pal(9, 'YlGnBu'))(length(bins)-1)
  key.cols = colorRampPalette(brewer.pal(9, 'YlGnBu'))(length(key.bins)-1)
  
  pal <- c(map.config$missing_data, pal) # 0 is grey
  key.cols <- c(map.config$missing_data, key.cols)  # 0 is grey
  key.text <- map.config$countBins
  #get closest bin
  bin = unname(sapply(counts.by.id, function(x) ifelse(is.na(x),1,which.min(abs(x-bins)))))
  cols = rep(NA, length(counts.by.id))
  cols[!is.na(bin)] = pal[bin[!is.na(bin)]]
  
  xlim <- c(-1534607.9,2050000.1) # specific to the transform we are using
  ylim <- c(-2072574.6,727758.7)
  
  plot(hucs, add = FALSE, col = cols, border = NA, lwd = 0.5, xlim = xlim, ylim = ylim)
}

plot_huc_panel <- function(hucs, map.config, figure.name, ...){
  sites <- list(...) #verify even number
  # loop
  browser()
  point.in = gContains(hucs, sites, byid=TRUE)
  counts.by.id = colSums(point.in) %>% log
  
  counts.by.id[!is.finite(counts.by.id)] <- NA
  
  ## -- color markers --
  
  key.bins = c(NA, log(axTicks(1, axp=c(1, max(exp(counts.by.id)), 3), usr=c(1,3), log=TRUE)))
  bins = key.bins
  pal = colorRampPalette(brewer.pal(9, 'YlGnBu'))(length(bins)-1)
  key.cols = colorRampPalette(brewer.pal(9, 'YlGnBu'))(length(key.bins)-1)
  
  pal <- c(map.config$missing_data, pal) # 0 is grey
  key.cols <- c(map.config$missing_data, key.cols)  # 0 is grey
  key.text <- c(0, tail(exp(key.bins), -1))
  #get closest bin
  bin = unname(sapply(counts.by.id, function(x) ifelse(is.na(x),1,which.min(abs(x-bins)))))
  cols = rep(NA, length(counts.by.id))
  cols[!is.na(bin)] = pal[bin[!is.na(bin)]]
  
  png(filename = figure.name, width = 7, heigh=5, res=150, units = 'in')
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
