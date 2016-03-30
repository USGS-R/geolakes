library(rgeos)
library(rgdal)
library(httr)
library(dplyr)
library(RColorBrewer)
library(dataRetrieval)

plot.CRS <- "+init=epsg:2163"

destination = tempfile(pattern = 'huc_shape', fileext='.zip')
query <- 'http://cida.usgs.gov/gdp/geoserver/wfs?service=WFS&request=GetFeature&typeName=derivative:wbdhu8_alb_simp&outputFormat=shape-zip&version=1.0.0'
file <- GET(query, write_disk(destination, overwrite=T), progress())
shp.path <- tempdir()
unzip(destination, exdir = shp.path)
hucs = readOGR(shp.path, layer='wbdhu8_alb_simp') %>% 
  spTransform(CRS(plot.CRS))

site.points <- data.frame(lon=c(-89,-92,-89.2, -93), lat=c(43, 42.4, 43.2, 45)) # these would be real sites

sp2 <- SpatialPoints(site.points, proj4string=CRS("+proj=longlat + datum=wgs84")) %>% 
  spTransform(CRS(plot.CRS))

point.in = gContains(hucs, sp2, byid=TRUE)
counts.by.id = colSums(point.in)
## -- analyze point in polygon /--

## -- color markers -- 
bins = pretty(counts.by.id, 100)
key.bins = pretty(counts.by.id, 5)
pal = colorRampPalette(brewer.pal(9, 'YlGnBu'))(length(bins))
key.cols = colorRampPalette(brewer.pal(9, 'YlGnBu'))(length(key.bins))

pal[1] <- missing.data # 0 is grey
key.cols[1] <- missing.data  # 0 is grey
#get closest bin
bin = unname(sapply(counts.by.id, function(x) ifelse(is.na(x),NA,which.min(abs(x-bins)))))
cols = rep(NA, length(counts.by.id))
cols[!is.na(bin)] = pal[bin[!is.na(bin)]]

layout(matrix(data = c(1,1,1,1,1,1,2), ncol=1))
par(mai = c(0,0,0,0), omi = c(0,0,0,0))
par(mai = c(0,0,0,0), omi = c(0,0,0,0))

xlim <- c(-1534607.9,2050000.1) # specific to the transform we are using
ylim <- c(-2072574.6,727758.7)

plot(hucs, add = FALSE, col = cols, border = NA, lwd = 0.5, xlim = xlim, ylim = ylim)
plot(sp2, add = TRUE, col='red', pch=20, cex=0.5)


# secondary plot for color legend
plot(c(NA,NA),c(NA,NA), axes=F, ylim=c(0,1),xlim=c(0,1))
bin.w = 0.07
spc = .02
text(.1,.5, 'Number of sites', pos=3, offset=0.1)
for(i in 1:length(key.cols)){
  x1 = 0.20+(i-1)*(bin.w+spc)
  graphics::rect(x1, 0.3, x1+bin.w, 0.8, col=key.cols[i], lwd=NA)
  text(x1+bin.w/2, y=0.33, labels=key.bins[i], pos=1)
}
