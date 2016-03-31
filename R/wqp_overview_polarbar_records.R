library(plotrix)
library(stringr)
library(dplyr)

# Import Data
data <- read.csv("inst/extdata/wqp_sites_records_groupedby_sitetype.csv")

# Filter data
summary_big <- data %>% 
  group_by(siteType) %>% 
  summarize(totalRecords = sum(numRecords)) %>% 
  filter(totalRecords >= 10000000)

summary_small <- data %>% 
  group_by(siteType) %>% 
  summarize(totalRecords = sum(numRecords)) %>% 
  filter(totalRecords < 10000000) 

data_big <- data %>% 
  filter(siteType %in% summary_big$siteType)

data_small <- data %>% 
  filter(siteType %in% summary_small$siteType)

### Choose data set for plotting: 
wqpData = data_small
###

n = unique(wqpData$siteType)
totals = summarise(group_by(wqpData,siteType),n = sum(numRecords))
totals$n = totals$n/ (max(totals$n)*1.25)
startAngles <- (1/4 - totals$n)*2*pi
radii <- seq(1, 0.3, length.out=length(n))
char_cols <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
               '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#8dd3c7',
               '#b15928','#bebada','#fb8072','#80b1d3','#fdb462')

#### Individual Groups ####
par(fig=c(0,0.8,0,1),mar=c(0,0,0,0))
plot(0,xlim=c(-1.2,1.2),ylim=c(-1.2,1.2),type="n",axes=F, xlab=NA, ylab=NA)
draw.circle(0,0,radii,border="lightgrey")
for (i in 1:length(n)) {
  grp = filter(wqpData,siteType == n[[i]])
  gs = rev(cumsum(totals$n[i] * (grp$numRecords/sum(grp$numRecords))))
  startA <- (1/4 - gs)*2*pi
  endA <- c(startA[-1],pi/2)
  draw.arc(0, 0, radii[i], startA,endA, col=rev(char_cols), lwd=130/length(n), lend=1, n=1000)
  
  ymult <- (par("usr")[4]-par("usr")[3])/(par("usr")[2]-par("usr")[1])*par("pin")[1]/par("pin")[2]
  text(x=0.03, y=radii[i]*0.93, labels=n[i], pos=2, cex=0.8)
}
text(0,0,"WQP",cex=1.5,col="grey")

# Add legend #
chars = word(unique(wqpData$characteristicType))
chars = word(chars,sep='/')
chars = word(chars,sep=',')
par(fig=c(0.73,1,0,1),oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),new=T)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend('center',legend = chars,
       fill=char_cols,ncol = 1,cex=0.8,xpd=T,bty='n')



