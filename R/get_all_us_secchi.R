#library(dataRetrieval)
#library(dplyr)
#GET_all_us_secchi

get_all_us_secchi = function(outfile){
  
  characteristicNames = c("Depth, Secchi disk depth", "Depth, Secchi disk depth (choice list)", "Secchi Reading Condition (choice list)", "Secchi depth", "Water transparency, Secchi disc")
  siteTypes = "Lake, Reservoir, Impoundment"
  
  metadata = group_by(dataRetrieval::stateCd, STATE) %>% do((function(df){
      whatWQPsites(stateCode=df$STATE, characteristicName=characteristicNames, siteType=siteTypes)
    })(.))
  
  secchi = group_by(dataRetrieval::stateCd, STATE) %>% do((function(df){
      readWQPdataPaged(startDateLo='1980-01-01', startDateHi='2016-01-01', 
                       stateCode=df$STATE, characteristicName=characteristicNames, 
                       siteType=siteTypes)
    })(.))
  
  save(secchi, metadata, file=outfile)
  
}