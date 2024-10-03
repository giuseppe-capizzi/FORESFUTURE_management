# routine...
library(tidyverse)

# Nominal demand = demand input 
# Demand offset = Nominal demand - extracted
# Actual demand = Nominal demand + Demand offset

# FUNCTION to retrieve nominal, offset and actual demand -----------

# load demand 
# demand_2001_2020 <- readRDS("C:/Users/giuseppe.capizzi/OneDrive - ctfc.cat/OCCC_ScnForestals/Rdata/demand_2001_2020.rds")

# scn <- readRDS("C:/Users/giuseppe.capizzi/OneDrive - ctfc.cat/OCCC_ScnForestals/Rdata/Scenarios/BAU/BAU_45/BAU_2021-2030_45/8.rds")

demand_ret <- function(prov = NULL, scn = NULL, step = 1){
  # load scenario and select extracted demand 
  ex_IFN <- scn$extractedDemand
  ex_IFN <- ex_IFN[row.names(ex_IFN) != "Total",] # remove row Total
  # retrieve extracted to calculate offset demand
  extracted <- data.frame(extracted = ex_IFN[,"1"], Species = row.names(ex_IFN))
    
  # retrieve demand of province 
  if(prov == 8){demand <- demand_2001_2020$Barcelona}
  if(prov == 17){demand <- demand_2001_2020$Girona}
  if(prov == 25){demand <- demand_2001_2020$Lleida}
  if(prov == 43){demand <- demand_2001_2020$Tarragona}
  
  # check if species are the same 
  demand <- demand[demand$Step == step,]
  if(!identical(row.names(ex_IFN),demand[,"Name"])){stop("Species are different")}
  
  # add correct order of names to extracted demand 
  ex_IFN$Species <- demand[order(demand$Species),"Name"] 

  # retrieve nominal demand 
  nominal <- data.frame(nominal = ex_IFN$Demand*10, Species = ex_IFN$Species)
  # check if all species are in common
  if(!all(nominal$Species %in% extracted$Species)){stop("Species are different")}
  # join 
  final <- full_join(extracted, nominal, by= join_by(Species)) %>% relocate(Species, .before = extracted)
  final$offset <- final$nominal - final$extracted
  final$actual <- final$nominal + final$offset
  
  # return data.frame
  return(final)
}

# prov a<- demand_ret(prov = 8, scn = scn)

