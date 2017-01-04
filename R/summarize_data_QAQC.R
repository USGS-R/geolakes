
wqx.pip <- readRDS("datasets/wqx_ids_PiP.rds")
# all records:
nrow(secchi.data)
secchi.data %>% filter(MonitoringLocationTypeName %in% use.types) %>% nrow
secchi.data %>% filter(MonitoringLocationTypeName %in% use.types, wqx.id %in% wqx.pip) %>% nrow
is.dup <- secchi.data %>% filter(MonitoringLocationTypeName %in% use.types, wqx.id %in% wqx.pip) %>% 
  select(Date, dec_lat_va, dec_lon_va, value) %>% duplicated
sum(is.dup)
non.dups <- secchi.data %>% filter(MonitoringLocationTypeName %in% use.types, wqx.id %in% wqx.pip) %>% .[!is.dup, ] 
non.qas <- non.dups %>% 
  filter(ActivityTypeCode %in% c("Field Msr/Obs","Sample-Routine",
                                 "Field Msr/Obs-Portable Data Logger",
                                 "Sample-Composite Without Parents",
                                 "Sample-Field Split","Sample-Other")) %>%
  filter(ActivityMediaName %in% c("Water","Other","Habitat"))
nrow(non.dups) - nrow(non.qas)

unit.map <- data.frame(units=c('m','in','ft','cm',"mm","mi", NA), 
                       convert = c(1,0.0254,0.3048,0.01, 0.001, 1609.34, NA), 
                       stringsAsFactors = FALSE)

non.qas$units <- gsub(" ", "", non.qas$units)
good.units <- non.qas %>%
  filter(units %in% unit.map$units) %>%
  select(-convert, -secchi) %>%
  left_join(unit.map, by="units") %>% filter(!is.na(units)) %>%
  mutate(secchi = value*convert) %>%
  mutate(secchi = abs(secchi))

nrow(non.qas) - nrow(good.units)


secchi.filtered <- good.units %>% 
  rename(STATE = StateCode) %>% 
  left_join(stateCd, by=c("STATE")) %>% 
  left_join(regions, by="STATE_NAME") %>%
  mutate(week = lubridate::week(Date)) %>%
  filter(!is.na(area)) %>% filter(secchi > 0, secchi < 46) #meters

#Number of sites & number of records:
iqr <- function(x){
  paste('[',
        paste(round(quantile(x, na.rm=TRUE, probs = c(.25,0.75)), 2), collapse=', '),
        ']', sep='')
}
group_by(secchi.filtered, area) %>% 
  summarize(nSites = length(unique(wqx.id)), 
            nRecords = length(wqx.id),
            med = median(secchi, na.rm=TRUE),
            IQR = iqr(secchi))

secchi.filtered %>% .$secchi %>% median(na.rm=TRUE)
secchi.filtered %>% .$secchi %>% iqr

# state counts for records:
data.frame(table(select(secchi.filtered, STATE))) %>% 
  left_join(stateCd[,c("STATE","STATE_NAME")], by=c("Var1"="STATE")) %>% 
  arrange(desc(Freq))

# records per week per region:
secchi.filtered %>%
  filter(week > 13 & week < 47) %>%
  group_by(week, area) %>% tally %>% arrange(desc(week)) %>% tidyr::spread(value = 'n', key = 'area') %>% data.frame %>% 
  write.table(file = 'datasets/secchi_records_week.csv', sep = ',', quote=F, row.names = F)

# sites per week per region:
secchi.filtered %>%
  filter(week > 13 & week < 47) %>%
  group_by(week, area) %>% summarize(n=length(unique(wqx.id))) %>% arrange(desc(week)) %>% tidyr::spread(value = 'n', key = 'area') %>% data.frame %>% 
  write.table(file = 'datasets/secchi_sites_week.csv', sep = ',', quote=F, row.names = F)