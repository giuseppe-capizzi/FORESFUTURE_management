############### Building results FORMES ########################
# remotes::install_github("spif-ctfc/medfuels")
# remotes::install_github("emf-creaf/IFNallometry")

library(medfuels)
library(IFNallometry)
library(medfate)
library(tidyverse)
library(stringr)

source("Rscripts/B2b_utils.R")
source("Rscripts/B2c_demand_cal.R")
demand_2001_2020 <- readRDS("C:/Users/giuseppe.capizzi/OneDrive - ctfc.cat/OCCC_ScnForestals/Rdata/demand_2001_2020.rds")
treedata_FORMES <- readRDS("Rdata/treedata_FORMES.rds")

pattern <- "\\d{4}-\\d{4}"
DBHclasses <- c(0, seq(2.5, 200, by = 5), 2000)

# Create all the variables as the file "Test_Barcelona...", 
# if the variable is not replicable just insert NAs

# example file from Miquel
Test_Barcelona <- readRDS("C:/Users/giuseppe.capizzi/OneDrive - ctfc.cat/OCCC_ScnForestals/Rdata/Test_Barcelona_AMF_mpiesm_rca4_rcp85_example_Miquel.rds")


############## Tree_table -----
tree_table_ex <- Test_Barcelona$tree_table
head(tree_table_ex)
names(tree_table_ex)


# load example IFNscenario file 
ex_IFN <- readRDS("C:/Users/giuseppe.capizzi/OneDrive - ctfc.cat/OCCC_ScnForestals/Rdata/Scenarios/AMF/AMF_85/AMF_2021-2030_85/8.rds")



# funcion per tree_table, cut_table, dead_table

table_gen <- function(rcp = NULL, mgm = NULL, prov = NULL, type = NULL){
  # browser()
  # create empty df
  column_names <- c("Climate","Management","Province","id","Step","Year","Cohort","Species",   
                    "DBH","Height","N","Volume","Aerial","Roots","DBHclass")
  
  table <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  # Assign column names to the data frame
  colnames(table) <- column_names
  
  
  # Initial 
  # filter per province
  treedata_FORMES$Province <- substr(treedata_FORMES$ID,1, nchar(treedata_FORMES$ID)-4)
  treedata_ini <- treedata_FORMES %>% filter(Province == prov)
  
  table_ini <- data.frame(matrix(ncol = length(column_names), nrow = nrow(treedata_ini)))
  colnames(table_ini) <- column_names
  
  # fill data frames
  table_ini$Climate = paste0("rcp", rcp)
  table_ini$Management = mgm
  if(prov == 8){provincia = "Barcelona"}
  if(prov == 17){provincia = "Girona"}
  if(prov == 25){provincia = "LLeida"}
  if(prov == 43){provincia = "Tarragona"}
  table_ini$Province = provincia
  table_ini$id = treedata_ini$ID 
  table_ini$Step = 0
  table_ini$Year = "2000"
  # table_1$Cohort
  table_ini$Species = FORMES::speciesNames(treedata_ini$Species) 
  table_ini$DBH = treedata_ini$DBH 
  table_ini$Height = treedata_ini$H*100
  table_ini$N = treedata_ini$N 
  table_ini$Volume = volume_scenario_FORMES(treedata_ini, SpParamsMED, prov) 
  biom_1 = tree_biomass_scenario_FORMES(treedata_ini, SpParamsMED, as.CO2 = TRUE) 
  table_ini$Aerial <- biom_1$Aerial/1000 
  table_ini$Roots <- biom_1$Roots/1000 
  table_ini$DBHclass <- cut(treedata_ini$DBH, DBHclasses, right = FALSE) 
  
  table <- rbind(table, table_ini)
  # BAU common
  # load 
  Bau_common <- readRDS(paste0("Rdata/Scenarios/BAU/BAU_2001-2020/", prov ,".rds"))
  if(type == "tree"){bau_tree <- Bau_common[["treeDataSequence"]]}
  if(type == "dead"){bau_tree <- Bau_common[["treeDataDead"]]}
  if(type == "cut"){bau_tree <- Bau_common[["treeDataCut"]]}
  
  # filters
  bau_tree_1 <- bau_tree[bau_tree$Step == 1,]
  bau_tree_2 <- bau_tree[bau_tree$Step == 2,]
  
  #create data.frame 
  table_bau_1 <- data.frame(matrix(ncol = length(column_names), nrow = nrow(bau_tree_1)))
  table_bau_2 <- data.frame(matrix(ncol = length(column_names), nrow = nrow(bau_tree_2)))
  # Assign column names to the data frame
  colnames(table_bau_1) <- column_names
  colnames(table_bau_2) <- column_names
  
  # fill data frames
  table_bau_1$Climate = paste0("rcp", rcp) ; table_bau_2$Climate = paste0("rcp", rcp) 
  table_bau_1$Management = mgm ; table_bau_2$Management = mgm
  if(prov == 8){provincia = "Barcelona"}
  if(prov == 17){provincia = "Girona"}
  if(prov == 25){provincia = "LLeida"}
  if(prov == 43){provincia = "Tarragona"}
  table_bau_1$Province = provincia ; table_bau_2$Province = provincia 
  table_bau_1$id = bau_tree_1$ID ; table_bau_2$id = bau_tree_2$ID 
  table_bau_1$Step = bau_tree_1$Step ; table_bau_2$Step = bau_tree_2$Step
  table_bau_1$Year = "2001-2010" ;  table_bau_2$Year = "2011-2020"
  # table_1$Cohort
  table_bau_1$Species = FORMES::speciesNames(bau_tree_1$Species) ; table_bau_2$Species = FORMES::speciesNames(bau_tree_2$Species)
  table_bau_1$DBH = bau_tree_1$DBH ; table_bau_2$DBH = bau_tree_2$DBH
  table_bau_1$Height = bau_tree_1$H*100 ; table_bau_2$Height = bau_tree_2$H*100
  table_bau_1$N = bau_tree_1$N ; table_bau_2$N = bau_tree_2$N
  table_bau_1$Volume = volume_scenario_FORMES(bau_tree_1, SpParamsMED, prov) ; table_bau_2$Volume = volume_scenario_FORMES(bau_tree_2, SpParamsMED, prov)
  biom_1 = tree_biomass_scenario_FORMES(bau_tree_1, SpParamsMED, as.CO2 = TRUE) ; biom_2 = tree_biomass_scenario_FORMES(bau_tree_2, SpParamsMED, as.CO2 = TRUE)
  table_bau_1$Aerial <- biom_1$Aerial/1000 ; table_bau_2$Aerial <- biom_2$Aerial/1000# From kg/ha to Mg/ha
  table_bau_1$Roots <- biom_1$Roots/1000 ; table_bau_2$Roots <- biom_2$Roots/1000 
  table_bau_1$DBHclass <- cut(bau_tree_1$DBH, DBHclasses, right = FALSE) ; table_bau_2$DBHclass <- cut(bau_tree_2$DBH, DBHclasses, right = FALSE)
  
  table <- rbind(table, table_bau_1, table_bau_2)
  
  
  # enter in directories 
  dirs <- list.files(paste0("Rdata/Scenarios/", mgm, "/",mgm,"_", rcp), full.names = T)
  for(d in dirs){
    # retrieve years range
    year_range <- str_extract(d, pattern)
    
    # load scenario and data frame of interest (type)
    scn <- readRDS(paste0(d, "/", prov, ".rds"))
    if(type == "tree"){treeData <- scn[["treeDataSequence"]]}
    if(type == "dead"){treeData <- scn[["treeDataDead"]]}
    if(type == "cut"){treeData <- scn[["treeDataCut"]]}

    # check that there are only step 0 and 1
    if(any(treeData$Step > 1)){stop("Attention: more than 1 step in scenario")}
    # filter for step 1
    treeData <- treeData[treeData$Step == 1,]
    
    #create data.frame for 1 step (1 directory/ year range)
    table_1 <- data.frame(matrix(ncol = length(column_names), nrow = nrow(treeData)))
    # Assign column names to the data frame
    colnames(table_1) <- column_names
    
    # fill data.frame for 1 step (year range)
    table_1$Climate = paste0("rcp", rcp)
    table_1$Management = mgm
    if(prov == 8){provincia = "Barcelona"}
    if(prov == 17){provincia = "Girona"}
    if(prov == 25){provincia = "LLeida"}
    if(prov == 43){provincia = "Tarragona"}
    table_1$Province = provincia
    table_1$id = treeData$ID
    table_1$Step = treeData$Step
    table_1$Year = year_range
    # table_1$Cohort
    table_1$Species = FORMES::speciesNames(treeData$Species)
    table_1$DBH = treeData$DBH
    table_1$Height = treeData$H*100
    table_1$N = treeData$N
    table_1$Volume = volume_scenario_FORMES(treeData, SpParamsMED, prov)
    biom = tree_biomass_scenario_FORMES(treeData, SpParamsMED, as.CO2 = TRUE)
    table_1$Aerial <- biom$Aerial/1000# From kg/ha to Mg/ha
    table_1$Roots <- biom$Roots/1000 # From kg/ha to Mg/ha
    table_1$DBHclass <- cut(treeData$DBH, DBHclasses, right = FALSE)

    table <- rbind(table, table_1)
  }
  
  return(table)
  
}
table_gen_NOGEST <- function(rcp = NULL, prov = NULL, type = NULL){
  # create empty df
  column_names <- c("Climate","Management","Province","id","Step","Year","Cohort","Species",   
                    "DBH","Height","N","Volume","Aerial","Roots","DBHclass")
  
  table <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  # Assign column names to the data frame
  colnames(table) <- column_names
  
  # load scenario and data frame of interest (type)
  scn <- readRDS(paste0("Rdata/Scenarios/NOG_2001-2100_", rcp,"/", prov, ".rds"))
  if(type == "tree"){treeData <- scn[["treeDataSequence"]]}
  if(type == "dead"){treeData <- scn[["treeDataDead"]]}
  if(type == "cut"){return(table)}
  
  # Initial 
  # filter per province
  treedata_FORMES$Province <- substr(treedata_FORMES$ID,1, nchar(treedata_FORMES$ID)-4)
  treedata_ini <- treedata_FORMES %>% filter(Province == prov)
  
  table_ini <- data.frame(matrix(ncol = length(column_names), nrow = nrow(treedata_ini)))
  colnames(table_ini) <- column_names
  
  # fill data frames
  table_ini$Climate = paste0("rcp", rcp)
  table_ini$Management = "NOG"
  if(prov == 8){provincia = "Barcelona"}
  if(prov == 17){provincia = "Girona"}
  if(prov == 25){provincia = "LLeida"}
  if(prov == 43){provincia = "Tarragona"}
  table_ini$Province = provincia
  table_ini$id = treedata_ini$ID 
  table_ini$Step = 0
  table_ini$Year = "2000"
  # table_1$Cohort
  table_ini$Species = FORMES::speciesNames(treedata_ini$Species) 
  table_ini$DBH = treedata_ini$DBH 
  table_ini$Height = treedata_ini$H*100
  table_ini$N = treedata_ini$N 
  table_ini$Volume = volume_scenario_FORMES(treedata_ini, SpParamsMED, prov) 
  biom_1 = tree_biomass_scenario_FORMES(treedata_ini, SpParamsMED, as.CO2 = TRUE) 
  table_ini$Aerial <- biom_1$Aerial/1000 
  table_ini$Roots <- biom_1$Roots/1000 
  table_ini$DBHclass <- cut(treedata_ini$DBH, DBHclasses, right = FALSE) 
  
  table <- rbind(table, table_ini)
  
  # retrieve steps 
  steps <- scn$steps
  if(steps != 10){stop("Wrong number of steps")}
  year_ranges <- c("2001-2010", "2011-2020", "2021-2030", "2031-2040", "2041-2050",
                   "2051-2060", "2061-2070", "2071-2080", "2081-2090", "2091-2100")
  
  for(s in 1:steps){
    # retrieve years range
    year_range <- year_ranges[s]
    
    # filter for step 
    treeData_step <- treeData[treeData$Step == s,]
    
    #create data.frame for 1 step (1 directory/ year range)
    table_1 <- data.frame(matrix(ncol = length(column_names), nrow = nrow(treeData_step)))
    # Assign column names to the data frame
    colnames(table_1) <- column_names
    
    # fill data.frame for 1 step (year range)
    table_1$Climate = paste0("rcp", rcp)
    table_1$Management = "NOG"
    if(prov == 8){provincia = "Barcelona"}
    if(prov == 17){provincia = "Girona"}
    if(prov == 25){provincia = "LLeida"}
    if(prov == 43){provincia = "Tarragona"}
    table_1$Province = provincia
    table_1$id = treeData_step$ID
    table_1$Step = treeData_step$Step
    table_1$Year = year_range
    # table_1$Cohort
    table_1$Species = FORMES::speciesNames(treeData_step$Species)
    table_1$DBH = treeData_step$DBH
    table_1$Height = treeData_step$H*100
    table_1$N = treeData_step$N
    table_1$Volume = volume_scenario_FORMES(treeData_step, SpParamsMED, prov)
    biom = tree_biomass_scenario_FORMES(treeData_step, SpParamsMED, as.CO2 = TRUE)
    table_1$Aerial <- biom$Aerial/1000# From kg/ha to Mg/ha
    table_1$Roots <- biom$Roots/1000 # From kg/ha to Mg/ha
    table_1$DBHclass <- cut(treeData_step$DBH, DBHclasses, right = FALSE)
    
    table <- rbind(table, table_1)
  }

  return(table)
  
}

mgm = "ASEA" 
rcp = "45"

prova_table<- table_gen(rcp = rcp,mgm = mgm, prov = 8, type = "tree")
prova_table_NOGEST <- table_gen_NOGEST(rcp = rcp, prov = 8, type = "tree")

# type =c("tree", "dead", "cut")  # Sequence, Dead, Cut treeData
# test
test_tree_table <- table_gen(rcp = rcp, mgm = mgm, prov = 17, type = "tree")


############## volume_table #####
volume_table_ex <- Test_Barcelona$volume_table
head(volume_table_ex)
names(volume_table_ex)

pattern <- "\\d{4}-\\d{4}"
DBHclasses <- c(0, seq(2.5, 200, by = 5), 2000)
mgm = "ASEA" 
rcp = "45"

volume_gen <- function(rcp = NULL, mgm = NULL, prov = NULL){
  
  # browser()
  
  # create empty df
  column_names <- c("Climate", "Management", "Province", "Year", "initial", "growth",
  "mortality", "extracted", "final", "cumulative_growth", "cumulative_extraction",
  "initial_target", "growth_target", "mortality_target", "extracted_target",
  "final_target", "nominal_demand", "demand_offset", "actual_demand", "cumulative_nominal_demand",
  "cumulative_extracted_demand")
  
  table <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  # Assign column names to the data frame
  colnames(table) <- column_names
  
  # BAU common
  # load 
  Bau_common <- readRDS(paste0("Rdata/Scenarios/BAU/BAU_2001-2020/", prov ,".rds"))
  
  # filters
  volumeData <- Bau_common$volume
  
  #create data.frame 
  table_bau_1 <- data.frame(matrix(ncol = length(column_names), nrow = 1))
  table_bau_2 <- data.frame(matrix(ncol = length(column_names), nrow = 1))
  # Assign column names to the data frame
  colnames(table_bau_1) <- column_names
  colnames(table_bau_2) <- column_names
  
  # retrieve demand bau 2001-2020
  if(prov == 8){demand_bau <- demand_2001_2020$Barcelona}
  if(prov == 17){demand_bau <- demand_2001_2020$Girona}
  if(prov == 25){demand_bau <- demand_2001_2020$Lleida}
  if(prov == 43){demand_bau <- demand_2001_2020$Tarragona}
  
  # Initial
  # filter per province
  treedata_FORMES$Province <- substr(treedata_FORMES$ID,1, nchar(treedata_FORMES$ID)-4)
  treedata_ini <- treedata_FORMES %>% filter(Province == prov)
  
  table_ini <- data.frame(matrix(ncol = length(column_names), nrow = 1))
  colnames(table_ini) <- column_names
  
  # fill data frames
  table_ini$Climate = paste0("rcp", rcp)
  table_ini$Management = mgm
  if(prov == 8){provincia = "Barcelona"}
  if(prov == 17){provincia = "Girona"}
  if(prov == 25){provincia = "LLeida"}
  if(prov == 43){provincia = "Tarragona"}
  table_ini$Province = provincia
  table_ini$Year = "2000"
  table_ini$initial = colSums(volumeData$standingSpp)["0"]
  table_ini$growth = 0
  table_ini$mortality = 0
  table_ini$extracted = 0
  table_ini$final = (table_ini$initial + table_ini$growth) -  table_ini$extracted
  table_ini$nominal_demand = 0
  table_ini$demand_offset = 0
  table_ini$actual_demand = 0
  
  table <- rbind(table, table_ini)
  
  # fill data.frame for 1 step (year range)
  table_bau_1$Climate = paste0("rcp", rcp) ; table_bau_2$Climate = paste0("rcp", rcp)
  table_bau_1$Management = mgm ; table_bau_2$Management = mgm
  if(prov == 8){provincia = "Barcelona"}
  if(prov == 17){provincia = "Girona"}
  if(prov == 25){provincia = "LLeida"}
  if(prov == 43){provincia = "Tarragona"}
  table_bau_1$Province = provincia ; table_bau_2$Province = provincia
  table_bau_1$Year = "2001-2010" ; table_bau_2$Year = "2011-2020"
  table_bau_1$initial = colSums(volumeData$standingSpp)["0"] ; table_bau_2$initial = colSums(volumeData$standingSpp)["1"]
  table_bau_1$growth = colSums(volumeData$standgrowthSpp)["1"] ; table_bau_2$growth = colSums(volumeData$standgrowthSpp)["2"]
  table_bau_1$mortality = colSums(volumeData$deadSpp)["1"] ; table_bau_2$mortality = colSums(volumeData$deadSpp)["2"]
  table_bau_1$extracted = colSums(volumeData$extractedSpp)["1"] ; table_bau_2$extracted = colSums(volumeData$extractedSpp)["2"]
  table_bau_1$final = (table_bau_1$initial + table_bau_1$growth) - table_bau_1$extracted ; table_bau_2$final = (table_bau_2$initial + table_bau_2$growth) - table_bau_2$extracted
  demand <- demand_ret(prov = prov, scn = Bau_common) ; demand_2 <- demand_ret(prov = prov, scn = Bau_common, step=2)
  table_bau_1$nominal_demand = sum(demand_bau[demand_bau$Step==1,"AnnualDemand"]) ; table_bau_2$nominal_demand = sum(demand_bau[demand_bau$Step==2,"AnnualDemand"]) 
  table_bau_1$demand_offset = sum(demand$offset) ; table_bau_2$demand_offset = sum(demand_2$offset)
  table_bau_1$actual_demand = sum(demand$actual) ; table_bau_2$actual_demand = sum(demand_2$actual)
  table <- rbind(table, table_bau_1, table_bau_2)
  
  
  
  
  # enter in directories 
  dirs <- list.files(paste0("Rdata/Scenarios/", mgm, "/",mgm,"_", rcp), full.names = T)
  for(d in dirs){
    # retrieve years range
    year_range <- str_extract(d, pattern)
    
    # load scenario 
    scn <- readRDS(paste0(d, "/", prov, ".rds"))
    # select volume df
    volumeData <- scn$volume
    
    
    # check that there are only step 0 and 1
    if(scn$steps!=1){stop("Attention: more than 1 step in scenario")}
    # # filter for step 1
    # treeData <- treeData[treeData$Step == 1,]
    
    #create data.frame for 1 step (1 directory/ year range)
    table_1 <- data.frame(matrix(ncol = length(column_names), nrow = 1))
    # Assign column names to the data frame
    colnames(table_1) <- column_names
    
    # browser()
    # fill data.frame for 1 step (year range)
    table_1$Climate = paste0("rcp", rcp)
    table_1$Management = mgm
    if(prov == 8){provincia = "Barcelona"}
    if(prov == 17){provincia = "Girona"}
    if(prov == 25){provincia = "LLeida"}
    if(prov == 43){provincia = "Tarragona"}
    table_1$Province = provincia
    table_1$Year = year_range
    table_1$initial = colSums(volumeData$standingSpp)["0"]
    table_1$growth = colSums(volumeData$standgrowthSpp)["1"]
    table_1$mortality = colSums(volumeData$deadSpp)["1"]
    table_1$extracted = colSums(volumeData$extractedSpp)["1"]
    table_1$final = (table_1$initial + table_1$growth) - table_1$extracted
    demand <- demand_ret(prov = prov, scn = scn)
    table_1$nominal_demand = sum(demand$nominal)
    table_1$demand_offset = sum(demand$offset)
    table_1$actual_demand = sum(demand$actual)
    table <- rbind(table, table_1)
  }
  table$cumulative_growth = cumsum(table$growth)
  table$cumulative_extracted_demand = cumsum(table$extracted_target)
  table$cumulative_extraction = cumsum(table$extracted)
  table$cumulative_nominal_demand = cumsum(table$nominal_demand)
  return(table)
  
}
volume_gen_NOGEST <- function(rcp = NULL, prov = NULL){
  # browser()
  # create empty df
  column_names <- c("Climate", "Management", "Province", "Year", "initial", "growth",
                    "mortality", "extracted", "final", "cumulative_growth", "cumulative_extraction",
                    "initial_target", "growth_target", "mortality_target", "extracted_target",
                    "final_target", "nominal_demand", "demand_offset", "actual_demand", "cumulative_nominal_demand",
                    "cumulative_extracted_demand")
  
  table <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  # Assign column names to the data frame
  colnames(table) <- column_names
  
  # load scenario and data frame of interest (type)
  scn <- readRDS(paste0("Rdata/Scenarios/NOG_2001-2100_", rcp,"/", prov, ".rds"))
  # select volume df
  volumeData <- scn$volume
  
  steps <- scn$steps
  if(steps != 10){stop("Wrong number of steps")}
  year_ranges <- c("2001-2010", "2011-2020", "2021-2030", "2031-2040", "2041-2050",
                   "2051-2060", "2061-2070", "2071-2080", "2081-2090", "2091-2100")
  
  # Initial
  # filter per province
  treedata_FORMES$Province <- substr(treedata_FORMES$ID,1, nchar(treedata_FORMES$ID)-4)
  treedata_ini <- treedata_FORMES %>% filter(Province == prov)
  
  table_ini <- data.frame(matrix(ncol = length(column_names), nrow = 1))
  colnames(table_ini) <- column_names
  
  # fill data frames
  table_ini$Climate = paste0("rcp", rcp)
  table_ini$Management = "NOG"
  if(prov == 8){provincia = "Barcelona"}
  if(prov == 17){provincia = "Girona"}
  if(prov == 25){provincia = "LLeida"}
  if(prov == 43){provincia = "Tarragona"}
  table_ini$Province = provincia
  table_ini$Year = "2000"
  table_ini$initial = colSums(volumeData$standingSpp)["0"]
  table_ini$growth = 0
  table_ini$mortality = 0
  table_ini$extracted = 0
  table_ini$final = (table_ini$initial + table_ini$growth) -  table_ini$extracted
  table_ini$nominal_demand = 0
  table_ini$demand_offset = 0
  table_ini$actual_demand = 0
  
  table <- rbind(table, table_ini)
  
  for(s in 1:steps){
    # retrieve years range
    year_range <- year_ranges[s]
    
    #create data.frame for 1 step (1 directory/ year range)
    table_1 <- data.frame(matrix(ncol = length(column_names), nrow = 1))
    # Assign column names to the data frame
    colnames(table_1) <- column_names
    
    # browser()
    # fill data.frame for 1 step (year range)
    table_1$Climate = paste0("rcp", rcp)
    table_1$Management = "NOG"
    if(prov == 8){provincia = "Barcelona"}
    if(prov == 17){provincia = "Girona"}
    if(prov == 25){provincia = "LLeida"}
    if(prov == 43){provincia = "Tarragona"}
    table_1$Province = provincia
    table_1$Year = year_range
    table_1$initial = colSums(volumeData$standingSpp)[as.character(s-1)]
    table_1$growth = colSums(volumeData$standgrowthSpp)[as.character(s)]
    table_1$mortality = colSums(volumeData$deadSpp)[as.character(s)]
    table_1$extracted = colSums(volumeData$extractedSpp)[as.character(s)]
    table_1$final = (table_1$initial + table_1$growth) - table_1$extracted

    table_1$nominal_demand = 0
    table_1$demand_offset = 0
    table_1$actual_demand = 0
    table <- rbind(table, table_1)
  }
  table$cumulative_growth = cumsum(table$growth)
  table$cumulative_extracted_demand = cumsum(table$extracted_target)
  table$cumulative_extraction = cumsum(table$extracted)
  table$cumulative_nominal_demand = cumsum(table$nominal_demand)
  return(table)
  
}

prova_vol <- volume_gen(rcp = "45", mgm = "BAU", prov = 8)
prova_vol_NOG <- volume_gen_NOGEST(rcp = "45",prov = 8)

############## summary_table #####

# examples 
readRDS(paste0("Rdata/WB/wb_", rcp, "_", prov, "_", substr(year_range, 1,4), "_", substr(year_range, 6,9), ".rds" ))



bind_scenario_summary_table <- function(rcp = NULL, mgm = NULL, prov = NULL) {
  reshape_summary<-function(sf) {
    for(i in 1:nrow(sf)) {
      if(!is.null(sf$summary[[i]])) {
        sum_i <- data.frame(sf$summary[[i]])
        sum_i$Year <- format(as.Date(row.names(sum_i)), "%Y")
        sum_i$id <- sf$id[i]
        row.names(sum_i) <- NULL
        sum_i <- sum_i |>
          relocate(id, Year, .before = PET)
        sf$summary[[i]] <- sum_i
      }
    }
    return(sf)
  }
  
  # create empty df
  column_names <- c("Climate", "Management", "Province", "id", "Year", "PET", "Precipitation",
                    "Rain", "Snow", "NetRain", "Snowmelt", "Infiltration", "Runoff", "DeepDrainage",
                    "Evapotranspiration", "Interception", "SoilEvaporation", "HerbTranspiration",
                    "PlantExtraction", "Transpiration", "HydraulicRedistribution",
                    "GrossPrimaryProduction", "MaintenanceRespiration", "SynthesisRespiration",
                    "NetPrimaryProduction", "Tree_BA", "Adult_BA", "Sapling_BA", "Tree_density",
                    "Adult_density", "Sapling_density", "Shrub_density", "Tree_cover", "Adult_cover",
                    "Sapling_cover", "Shrub_cover", "Herb_cover", "Tree_lai", "Adult_lai",
                    "Sapling_lai", "Shrub_lai", "Herb_lai", "Total_lai", "Tree_fuel", "Adult_fuel",
                    "Sapling_fuel", "Shrub_fuel", "Herb_fuel", "Total_fuel", "PARground", "SWRground",
                    "SFP", "CFP", "LAI_mean", "Cm_mean", "LAI_min", "Cm_min", "LAI_max", "Cm_max",
                    "PlantStress", "StemPLC", "CFMC_overstory_min", "CFMC_understory_min", "DFMC_min",
                    "N120", "N100", "N80", "N880", "na.rm", "Pdaymax", "MAT")
  

  # common bau
  # load wb
  wb_bau_1 <- readRDS(paste0("Rdata/WB/BAU_2001_2020/wb_", prov, "_2001_2010.rds" ))
  wb_bau_2 <- readRDS(paste0("Rdata/WB/BAU_2001_2020/wb_", prov, "_2011_2020.rds" ))
  # #create data.frame for 1 step (1 directory/ year range)
  
  wb_bau_1 <- reshape_summary(wb_bau_1) ; wb_bau_2 <- reshape_summary(wb_bau_2)
  table_bau_1 <- bind_rows(wb_bau_1$summary) ; table_bau_2 <- bind_rows(wb_bau_2$summary) 
  
  table <- bind_rows(table_bau_1, table_bau_2)

  
  # enter in directories 
  dirs <- list.files(paste0("Rdata/Scenarios/", mgm, "/",mgm,"_", rcp), full.names = T)
  for(d in 1:length(dirs)){
    # retrieve years range
    year_range <- str_extract(dirs[d], pattern)
    
    # load scenario 
    # scn <- readRDS(paste0(dirs[d], "/", prov, ".rds"))
    
    # load wb
    wb <- readRDS(paste0("Rdata/WB/", mgm ,"/wb_", rcp, "_", prov, "_", substr(year_range, 1,4), "_", substr(year_range, 6,9), ".rds" ))
    # #create data.frame for 1 step (1 directory/ year range)
    # table_1 <- data.frame(matrix(ncol = length(column_names), nrow = 1))
    # # Assign column names to the data frame
    # colnames(table_1) <- column_names

    
    # fill data.frame 
    wb <- reshape_summary(wb)
    table_1 <- bind_rows(wb$summary) 

    table <- bind_rows(table, table_1)

  }
  # fill data.frame with remaining columns
  for (c in column_names[!column_names %in% names(table)]){
    table[c] = NA
  }
  
  table$Climate = paste0("rcp", rcp)
  table$Management = mgm
  if(prov == 8){provincia = "Barcelona"}
  if(prov == 17){provincia = "Girona"}
  if(prov == 25){provincia = "LLeida"}
  if(prov == 43){provincia = "Tarragona"}
  table$Province = provincia

  table <- table |>
    relocate(Climate, Management, Province, .before = id)
  return(table)
}
bind_scenario_summary_table_NOGEST <- function(rcp = NULL, prov = NULL) {
  reshape_summary<-function(sf) {
    for(i in 1:nrow(sf)) {
      if(!is.null(sf$summary[[i]])) {
        sum_i <- data.frame(sf$summary[[i]])
        sum_i$Year <- format(as.Date(row.names(sum_i)), "%Y")
        sum_i$id <- sf$id[i]
        row.names(sum_i) <- NULL
        sum_i <- sum_i |>
          relocate(id, Year, .before = PET)
        sf$summary[[i]] <- sum_i
      }
    }
    return(sf)
  }
  
  # create empty df
  column_names <- c("Climate", "Management", "Province", "id", "Year", "PET", "Precipitation",
                    "Rain", "Snow", "NetRain", "Snowmelt", "Infiltration", "Runoff", "DeepDrainage",
                    "Evapotranspiration", "Interception", "SoilEvaporation", "HerbTranspiration",
                    "PlantExtraction", "Transpiration", "HydraulicRedistribution",
                    "GrossPrimaryProduction", "MaintenanceRespiration", "SynthesisRespiration",
                    "NetPrimaryProduction", "Tree_BA", "Adult_BA", "Sapling_BA", "Tree_density",
                    "Adult_density", "Sapling_density", "Shrub_density", "Tree_cover", "Adult_cover",
                    "Sapling_cover", "Shrub_cover", "Herb_cover", "Tree_lai", "Adult_lai",
                    "Sapling_lai", "Shrub_lai", "Herb_lai", "Total_lai", "Tree_fuel", "Adult_fuel",
                    "Sapling_fuel", "Shrub_fuel", "Herb_fuel", "Total_fuel", "PARground", "SWRground",
                    "SFP", "CFP", "LAI_mean", "Cm_mean", "LAI_min", "Cm_min", "LAI_max", "Cm_max",
                    "PlantStress", "StemPLC", "CFMC_overstory_min", "CFMC_understory_min", "DFMC_min",
                    "N120", "N100", "N80", "N880", "na.rm", "Pdaymax", "MAT")
  
  year_ranges <- c("2001-2010", "2011-2020", "2021-2030", "2031-2040", "2041-2050",
                   "2051-2060", "2061-2070", "2071-2080", "2081-2090", "2091-2100")
  
  for(y in year_ranges){
    # scn <- readRDS(paste0("Rdata/Scenarios/NOG_2001-2100_", rcp,"/", prov, ".rds"))
    cat(paste0(y,"\n"))
    # retrieve years range
    year_range <- y
    
    # load wb
    wb <- readRDS(paste0("Rdata/WB/NOGEST/wb_", rcp, "_", prov, "_", substr(year_range, 1,4), "_", substr(year_range, 6,9), ".rds" ))
    
    # fill data.frame 
    wb <- reshape_summary(wb)
    
    table_1 <- bind_rows(wb$summary) 
    
    if(y == "2001-2010"){
      table <- table_1
    } else {
      table <- bind_rows(table, table_1)
    }
  }

  # fill data.frame with remaining columns
  for (c in column_names[!column_names %in% names(table)]){
    table[c] = NA
  }
  
  table$Climate = paste0("rcp", rcp)
  table$Management = "NOG"
  if(prov == 8){provincia = "Barcelona"}
  if(prov == 17){provincia = "Girona"}
  if(prov == 25){provincia = "LLeida"}
  if(prov == 43){provincia = "Tarragona"}
  table$Province = provincia
  
  table <- table |>
    relocate(Climate, Management, Province, .before = id)
  return(table)
}
# test 
mgm = "AMF" 
rcp = "45"
prov = 8
amf_prova <- bind_scenario_summary_table(rcp = rcp, mgm = mgm, prov = prov)

nogest_prova <- bind_scenario_summary_table_NOGEST(rcp = rcp, prov = prov)

############## binding_function #####

# one file for each management, province and rcp
bind_scenario_province_results <- function(prov, rcp, mgm, WB) {
  # browser()
  climate_model = "mpiesm_rca4"
  management_scen = mgm
  climate_scen = paste0("rcp", rcp)
  
  provinces <- c(8,17,25,43)
  provinceStrings <- c("Barcelona", "Girona", "Lleida", "Tarragona")
  provinceCode <- provinces[prov]
  provinceName <- provinceStrings[prov]
  
  cli::cli_h3(paste0("Binding scenario results for province ", provinceName, ", " , rcp, ", ", mgm))
  
  DBHclasses <- c(0, seq(2.5, 200, by = 5), 2000)
  
  cli::cli_progress_step("Volume table")
  if(mgm == "NOG"){
    vol_table <- volume_gen_NOGEST(rcp = rcp,prov = provinceCode)
  } else {
    vol_table <- volume_gen(rcp = rcp,mgm = mgm, prov = provinceCode)
  }
    
  
  # Calculating tree volume and biomass
  cli::cli_progress_step("Tree table")
  if(mgm == "NOG"){
    tt<- table_gen_NOGEST(rcp = rcp, prov = provinceCode, type = "tree")
  } else {
    tt<- table_gen(rcp = rcp, mgm = mgm, prov = provinceCode, type = "tree")
  }
  

  cli::cli_progress_step("Dead tree table")
  if(mgm == "NOG"){
    dtt<- table_gen_NOGEST(rcp = rcp, prov = provinceCode, type = "dead")
  } else {
    dtt<- table_gen(rcp = rcp, mgm = mgm, prov = provinceCode, type = "dead")
  }
  
  dtt$N_starvation = 0 
  dtt$N_dessication = 0
  
  cli::cli_progress_step("Cut tree table")
  if(mgm == "NOG"){
    ctt<- table_gen_NOGEST(rcp = rcp, prov = provinceCode, type = "cut")
  } else {
    ctt<- table_gen(rcp = rcp, mgm = mgm, prov = provinceCode, type = "cut")
  }
  
  cli::cli_progress_step("Summary table")
  if(WB){
    if(mgm == "NOG"){
      summary_table <- bind_scenario_summary_table_NOGEST(rcp = rcp, prov = provinceCode)
    } else {
      summary_table <- bind_scenario_summary_table(rcp = rcp, mgm = mgm, prov = provinceCode)
    }
  } else {
    summary_table <- NULL
  }
  
  
  
  cli::cli_progress_step("Storing list")
  scen_list <- list(volume_table = vol_table,
                    tree_table = tt, dead_tree_table = dtt, cut_tree_table = ctt
                    ,summary_table = summary_table
                    )
  
  saveRDS(scen_list, file = paste0("Rdata/binded/", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds"))
  # saveRDS(scen_list, file = paste0("Rdata/binded/", provinceName, "_", mgm,"_", rcp, ".rds"))

  cli::cli_progress_done()
}


# test RSB
bind_scenario_province_results(prov = 1,rcp = "45", mgm = "RSB")
WB = F

# (1) BAU
mgm = "BAU"
bind_scenario_province_results(prov = 1, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 2, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 3, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 4, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 1, rcp = "85", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 2, rcp = "85", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 3, rcp = "85", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 4, rcp = "85", mgm = mgm, WB=WB)

# (2) AMF
mgm = "AMF"
bind_scenario_province_results(prov = 1, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 2, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 3, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 4, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 1, rcp = "85", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 2, rcp = "85", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 3, rcp = "85", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 4, rcp = "85", mgm = mgm, WB=WB)

# (3) RSB
mgm = "RSB"
bind_scenario_province_results(prov = 1, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 2, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 3, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 4, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 1, rcp = "85", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 2, rcp = "85", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 3, rcp = "85", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 4, rcp = "85", mgm = mgm, WB=WB)

# (4) ASEA
mgm = "ASEA"
bind_scenario_province_results(prov = 1, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 2, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 3, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 4, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 1, rcp = "85", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 2, rcp = "85", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 3, rcp = "85", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 4, rcp = "85", mgm = mgm, WB=WB)

# (5) ACG
mgm = "ACG"
bind_scenario_province_results(prov = 1, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 2, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 3, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 4, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 1, rcp = "85", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 2, rcp = "85", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 3, rcp = "85", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 4, rcp = "85", mgm = mgm, WB=WB)

# (6) NOG
mgm = "NOG"
bind_scenario_province_results(prov = 1, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 2, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 3, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 4, rcp = "45", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 1, rcp = "85", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 2, rcp = "85", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 3, rcp = "85", mgm = mgm, WB=WB)
bind_scenario_province_results(prov = 4, rcp = "85", mgm = mgm, WB=WB)



















