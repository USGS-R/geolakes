
library(rgeos)
library(rgdal)
library(dplyr)
library(dataRetrieval)
library(RColorBrewer)


regions <- data.frame(STATE_NAME = c('Montana', 'Wyoming', 'Idaho', 'Washington', 'Oregon', 'California', 'Nevada',
                      'Arizona', 'New Mexico', 'Colorado', 'Utah'), group='west', stringsAsFactors = FALSE) %>% 
  rbind(data.frame(STATE_NAME=c('Ohio', 'Indiana', 'Illinois', 'Wisconsin', 'Missouri', 'Iowa', 'Minnesota', 'Kansas',
                         'Nebraska', 'South Dakota', 'North Dakota', 'Michigan'), group='midwest', stringsAsFactors = FALSE)) %>% 
  rbind(data.frame(STATE_NAME=c('Maine', 'New Hampshire', 'Vermont', 'Massachusetts', 'Rhode Island', 'Connecticut', 
                           'New Jersey', 'Pennsylvania', 'New York'), group='northeast', stringsAsFactors = FALSE)) %>% 
  rbind(data.frame(STATE_NAME=c('Florida', 'Georgia', 'Louisiana', 'Arkansas', 'Oklahoma', 'Texas', 'South Carolina', 
                       'North Carolina', 'Virginia', 'Kentucky', 'Tennessee', 'West Virginia', 'Maryland', 
                       'Delaware', 'Alabama', 'Mississippi'), group='south', stringsAsFactors = FALSE))


cols <- c(west = '#283044', south = '#F7CB65',northeast='#2c7fb8', midwest = '#7FCDBB')
missing.data = "grey85"

data.out <- readRDS('all_secchi_usa.rds')
state.mapping <- dataRetrieval::stateCd %>% 
  select(STATE, STATE_NAME) %>% 
  rename(fip=STATE) %>% left_join(regions) %>% 
  filter(!is.na(group))

d = group_by(data.out, Date) %>% 
  mutate(week = lubridate::week(Date)) %>% 
  filter(week > 13 & week < 47) %>% 
  left_join(state.mapping) 

use.states <- d %>% group_by(STATE_NAME) %>% tally %>% arrange(n) %>% filter(!is.na(STATE_NAME)) %>% .$STATE_NAME
d <- d %>% 
  group_by(week, group) %>% summarize(med = median(secchi, na.rm=TRUE), 
                                    q25 = quantile(secchi, na.rm=TRUE, probs = .25),
                                    q75 = quantile(secchi, na.rm=TRUE, probs = .75))
  # figure out which states don't have data....


plot.CRS <- "+init=epsg:2163"

## -- get spatial data --
shp.path = '/Users/jread/Documents/R/OWDI-Lower-Colorado-Drought-Vis/src_data/states_21basic'
states = readOGR(shp.path, layer='states') %>% 
  spTransform(CRS(plot.CRS))

states <- states[!states$STATE_NAME %in% c('Alaska', 'Hawaii'), ]

fig.height <- 2.25
png(filename = 'secchi_fig.png', width = 3.1, height=fig.height, res=600, units = 'in')

par(mar=c(0,0,0.2,0.2), omi=c(0.25,0.35,0,0), mgp=c(1.8,0.15,0), las=1)

plot(NA, NA, ylim=c(0,4.5), xlim=c(13, 47), axes=FALSE, ylab="", xlab="", xaxs='i', yaxs='i')

for (group.p in names(cols)){
  lines(d %>% filter(group==group.p) %>% .$week, d %>% filter(group==group.p) %>% .$med, col=cols[[group.p]], type='l', ylim=c(0,4), lwd=3)
}

axis(2, at=c(0,1,2,3,4), tck=0.02)
axis(1, at=c(17.14286, 25.85714, 34.71429, 43.42857), labels=c('May',"Jul", "Sep", "Nov") , tck=0.02)
u <- par("usr")
v <- c(
  grconvertX(u[1:2], "user", "ndc"),
  grconvertY(u[3:4], "user", "ndc")
)
v <- c( 0.7, 1, v[4]-(v[4]-v[3])/2.4, v[4] )
v[3:4] <- v[3:4]+0.015
v[1:2] <- v[1:2]-0.02
old.par <- par(fig=v, new=TRUE, mar=c(0,0,0,0))
# then loop and add maps
for (group.p in names(cols)){
  plot.cols <- rep(NA, length(states$STATE_NAME))
  use.i <- states$STATE_NAME %in% (regions %>% filter(group==group.p) %>% .$STATE_NAME) & states$STATE_NAME %in% use.states
  plot.cols[use.i] <- cols[[group.p]]
  plot(states, col=plot.cols, border='white', lwd=0.3, axes=FALSE, xlab="", ylab="", bg=NA, add=(group.p != names(cols)[1]))
  # missing states:
  plot.cols <- rep(NA, length(states$STATE_NAME))
  use.i <- states$STATE_NAME %in% (regions %>% filter(group==group.p) %>% .$STATE_NAME) & !states$STATE_NAME %in% use.states
  plot.cols[use.i] <- missing.data
  plot(states, col=plot.cols, border='white', lwd=0.3, axes=FALSE, xlab="", ylab="", bg=NA, add=TRUE)
}
par(old.par)
box()

mtext(side = 2, "Secchi depth (m)", las=0, line = 1, adj=0.5, padj=0.3)

dev.off()
