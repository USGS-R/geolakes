library(dplyr)
infile <- "datasets/all_secchi_usa.rds"
secchi.data <- readRDS(infile)

states.border <- rgdal::readOGR(dsn='../NPS-USGS-proposal/data/states_shape', layer="CONUS_States", verbose = FALSE)
states <- states.border$STATE %>% unique %>% as.character
sites <- secchi.data %>% 
  group_by(wqx.id) %>% summarize(lat=mean(dec_lat_va, na.rm=TRUE), lon=mean(dec_lon_va, na.rm=TRUE)) %>% 
  filter(!is.na(lat) & !is.na(lon))

toCap <- function(x) {
  out = x
  for (i in seq_len(length(x))){
    out[i] <- tools::toTitleCase(x[i])
  }
  return(out)
}

fips.codes <- maps::state.fips
fips.codes$state.name <- fips.codes$polyname %>% as.character %>% strsplit('[:]') %>% lapply(function(x) x[1]) %>% unlist %>% toCap
fips.codes <- fips.codes %>% 
  group_by(state.name) %>% summarize(fip=stringr::str_pad(unique(fips), width = 2, pad = "0"))

as.layer_name <- function(state, res='M'){
  # get fip from state
  fips <- fips.codes %>% filter(state.name == state) %>% .$fip
  state <- gsub(' ', '_', state)
  sprintf("NHD_%s_%s_%s_ST", res, fips, state)
}

xy = cbind(sites$lon, sites$lat)
library(sp)
library(rgeos)
pts = SpatialPoints(xy, proj4string=CRS("+proj=longlat +datum=WGS84"))


split_combine <- function(pts, feature){
  
  if (length(feature) > 20000){
    if (length(feature) < 40000){
      cbind(rgeos::gWithin(pts[1:50000], feature, byid = TRUE), 
            rgeos::gWithin(pts[50001:length(pts)], feature, byid = TRUE))
    } else {
      cbind(rgeos::gWithin(pts[1:40000], feature, byid = TRUE), 
            rgeos::gWithin(pts[40001:80000], feature, byid = TRUE),
            rgeos::gWithin(pts[80001:length(pts)], feature, byid = TRUE))
    }
    
  } else{
    rgeos::gWithin(pts, feature, byid = TRUE)
  }
}
# loop through all states

use.idx <- rep(FALSE, length(pts))

for (state in states){
  shp.name <- as.layer_name(state)
  feature <- suppressWarnings(rgdal::readOGR(dsn=file.path('../NPS-USGS-proposal/data/.cache', shp.name, paste0(shp.name,".gdb")), layer="NHDWaterbody", verbose = FALSE)) 
  feature <- feature[feature$FType %in% c(390, 436, 361), ]
  feature <- feature %>% 
    spTransform(CRS(proj4string(pts)))
  d= split_combine(pts, feature)
  use.idx <- use.idx | colSums(d) > 0
  message(state, ' has ', sum(colSums(d) > 0), ' sites, ', sum(use.idx),' total sites so far')
}

use.sites <- sites[use.idx, ] 
use.ids <- use.sites$wqx.id