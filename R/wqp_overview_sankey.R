#geolakes sankey

siteTypes <- c('Surface water use',
               'Groundwater use',
               'Atmosphere',
               'Estuary',
               'Facility',
               'Glacier',
               'Lake, reservoir, or impoundment',
               'Land',
               'Ocean',
               'Spring',
               'Stream',
               'Subsurface',
               'Well',
               'Wetland')

characteristicGroups <- c('Biological',
                          'Information (metadata)',
                          'Inorganics, major metals',
                          'Inorganics, major non-metals',
                          'Inorganics, minor metals',
                          'Inorganics, minor non-metals',
                          'Microbiological',
                          'Nutrient',
                          'Organics (other)',
                          'Organics, PCBs',
                          'Organics, pesticide',
                          'Physical',
                          'Population/Community',
                          'Radiochemical',
                          'Sediment',
                          'Stable Isotopes',
                          'Toxicity')

source <- unlist(lapply(siteTypes, function(x) {rep(x, 17)}))
target <- rep(characteristicGroups, 14)
weight <- rep(1, 238)
df <- data.frame(source, target, weight, stringsAsFactors = FALSE)

#riverplot version
#http://www.exegetic.biz/blog/2014/08/plotting-flows-with-riverplot/

source <- unlist(lapply(siteTypes, function(x) {rep(x, 17)}))
target <- rep(characteristicGroups, 14)
edges <- data.frame(N1 = source, N2 = target, Value = rep(1, 238), stringsAsFactors = FALSE)
nodes <- data.frame(ID = unique(c(edges$N1, edges$N2)), 
                    x = c(rep(1, 14), rep(2, 17)),
                    y = c(0:13, 0:16),
                    stringsAsFactors = FALSE)
rownames(nodes) = nodes$ID

library(RColorBrewer)
palette = paste0(brewer.pal(3, "Set1"), "60")
palette = brewer.pal(3, "Set1")
styles = lapply(nodes$y, function(n) {
  list(col = palette[n+1], lty = 1, textcol = "black")
})
library(riverplot)
names(styles) = nodes$ID
river <- makeRiver(nodes, edges)
plot(river, plot_area = 0.95, srt = 1, yscale = 0.25)


#sankey but only a subset of characteristic and site types

source <- unlist(lapply(siteTypes[1:3], function(x) {rep(x, 5)}))
target <- rep(characteristicGroups[1:5], 3)
edges <- data.frame(N1 = source, N2 = target, Value = rep(1, 15), stringsAsFactors = FALSE)
nodes <- data.frame(ID = unique(c(edges$N1, edges$N2)), 
                    x = c(rep(1, 3), rep(2, 5)),
                    y = c(0:2, 0:4),
                    stringsAsFactors = FALSE)
rownames(nodes) = nodes$ID

library(RColorBrewer)
palette = paste0(brewer.pal(3, "Set1"), "60")
palette = brewer.pal(3, "Set1")
styles = lapply(nodes$y, function(n) {
  list(col = palette[n+1], lty = 1, textcol = "black")
})
library(riverplot)
names(styles) = nodes$ID
river <- makeRiver(nodes, edges)
plot(river, plot_area = 0.95, srt = 1, yscale = 0.25)

########################################## 
# Example sankey chart that uses rCharts + igraph
# cannot get this to work with our stuff

library(rCharts)
library(igraph)

g <- graph.tree(40, children = 4)
E(g)$weight = 1

edgelist <- get.data.frame(g) #this will give us a data frame with from,to,weight


colnames(edgelist) <- c("source","target","value")
#make character rather than numeric for proper functioning
edgelist$source <- as.character(edgelist$source)
edgelist$target <- as.character(edgelist$target)
sankeyPlot <- rCharts$new()
sankeyPlot$setLib('http://timelyportfolio.github.io/rCharts_d3_sankey/libraries/widgets/d3_sankey')
sankeyPlot$setTemplate(script = "http://timelyportfolio.github.io/rCharts_d3_sankey/libraries/widgets/d3_sankey/layouts/chart.html")
sankeyPlot$set(
  data = edgelist,
  nodeWidth = 15,
  nodePadding = 10,
  layout = 32,
  width = 960,
  height = 500
)
sankeyPlot$print(chartId = 'sankey1')

