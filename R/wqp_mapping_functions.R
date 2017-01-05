

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
  
  xlim <- c(-1834607.9,2750000.1) # specific to the transform we are using
  ylim <- c(-2072574.6,727758.7)
  
  plot(hucs, add = FALSE, col = cols, border = NA, lwd = 0.5, xlim = xlim, ylim = ylim)
}
#' @importFrom 
plot_huc_panel <- function(hucs, map.config, figure.name, ...){
  sites <- list(...) #verify even number
  # loop
  # AGU 1/2 page vertical figure: 95 mm x 230 mm
  # from https://publications.agu.org/author-resource-center/graphics/
  
  png(filename = figure.name, width = 95, height=230, res=300, units = 'mm')
  plot.mat <- matrix(c(0,1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,16,16,
                       rep(c(17,6,6,6,8,8,8,10,10,10,12,12,12,14,14,14,16,16),6),
                       rep(c(18,7,7,7,9,9,9,11,11,11,13,13,13,15,15,15,16,16),6)), ncol=13)
  layout(plot.mat)
  labels <- c("Arsenic", "Nitrogen", "Phosphorus", "Secchi", "Temperature")
  par(mai = c(0,0,0,0), omi = c(0,0,0,0)) #c(0.04,.1,0,0)
  for (j in 1:(length(sites)/2)){
    plot(0,NA, axes=F, xlim=c(0,1), ylim=c(0,1))
    lims <- par("usr")
    text(0.5,0.55, labels[j], srt = 90, cex=1.5)
  }
  
  for (j in 1:length(sites)){
    plot_huc(hucs, sites[[j]], map.config)
    text(-1824607.9, 900000, paste0(letters[j],')'), cex = 1.5)
  }
  
  # secondary plot for color legend
  key.cols = c(map.config$missing_data, colorRampPalette(brewer.pal(9, 'YlGnBu'))(length(map.config$countBins)-1))
  par(mai = c(0,0,0,0))
  plot(c(NA,NA),c(NA,NA), axes=F, ylim=c(0,1),xlim=c(0,1))
  mar.spc <- 0.00
  spc = .02
  bin.w <- (1-(mar.spc*2+spc*(length(key.cols)-1)))/length(key.cols)
  bin.h <- 0.25 
  y.spc <- 0.55
  
  for(i in 1:length(key.cols)){
    x1 = mar.spc+(i-1)*(bin.w+spc)
    graphics::rect(x1, y.spc, x1+bin.w, bin.h+y.spc, col=key.cols[i], lwd=NA)
    text(x1+bin.w/2, y=y.spc, labels=map.config$countBins[i], pos=1)
  }
  text(.5,y.spc-0.3, 'Number of sites', pos=1, offset=0.1, cex=2)
  
  plot(0,NA, axes=F, xlim=c(0,1), ylim=c(0,1))
  text(0.45,0.2, "Lake sites", cex=1.5)
  plot(0,NA, axes=F, xlim=c(0,1), ylim=c(0,1))
  text(0.45,0.2, "All sites", cex=1.5)
  dev.off()
}
