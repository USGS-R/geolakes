## pull in data for carbon lakes analysis

library(dataRetrieval)
library(data.table)

DOC_names <- c("Hydrophilic fraction of organic carbon",
               "Hydrophobic fraction of organic carbon",
               "Organic carbon", 
               "Total Particulate Organic Carbon", 
               "Transphilic fraction of organic carbon")

temp_names <- c("Temperature", 
                "Temperature, sample", 
                "Temperature, water")

phosphorus_names <- c("Exchangeable phosphorus", 
                      "Inorganic phosphorus",
                      "Non-apatite inorganic phosphorus",
                      "Organic phosphorus",
                      "Ortho-Phosphate-Phosphorus",
                      "Phosphate-phosphorus",
                      "Phosphate-phosphorus as P",
                      "Phosphate-phosphorus as PO4",
                      "Phosphorus",
                      "Phosphorus as P",
                      "Phosphorus, Particulate Organic",
                      "Phosphorus, hydrolyzable",
                      "Phosphorus, hydrolyzable plus orthophosphate",
                      "Phosphorus, phosphate (PO4) as orthophosphate",
                      "Soluble Reactive Phosphorus (SRP)",
                      "Total Particulate Phosphorus")

chlorophyll_names <- c("Chlorophyll", 
                       "Chlorophyll A", 
                       "Chlorophyll a", 
                       "Chlorophyll a (probe relative fluorescence)",
                       "Chlorophyll a (probe)",
                       "Chlorophyll a - Periphyton (attached)",
                       "Chlorophyll a - Phytoplankton (suspended)",
                       "Chlorophyll a, corrected for pheophytin",
                       "Chlorophyll a, free of pheophytin",
                       "Chlorophyll a, uncorrected for pheophytin",
                       "Chlorophyll b",
                       "Chlorophyll c",
                       "Chlorophyll/Pheophytin ratio")

DO_names <- c("Dissolved oxygen", 
              "Dissolved oxygen (DO)", 
              "Dissolved oxygen saturation", 
              "Dissolved oxygen uptake")

DIC_names <- c("Inorganic carbon", 
               "Particulate Inorganic Carbon")

pH_names <- "pH"


queryByState <- function(state, characteristicNames){
  sites <- whatWQPsites(statecode = state, 
                        siteType = 'Lake, Reservoir, Impoundment',
                        characteristicName = characteristicNames)
  char_data <- lapply(sites$MonitoringLocationIdentifier, FUN = readWQPdata, 
                      characteristicName = characteristicNames)
  df_state <- rbindlist(char_data)
  
  return(df_state)
}


## example: get all dissolved inorganic carbon data (only for RI and NJ for now -- shorter to run)
DIC_list <- lapply(stateCd$STUSAB[c(31)], queryByState, characteristicNames = DIC_names)
DIC_data <- rbindlist(DIC_list)
