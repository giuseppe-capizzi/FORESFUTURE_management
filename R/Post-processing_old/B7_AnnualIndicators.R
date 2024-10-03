rm(list=ls())
library(tidyverse)
library(Hmisc)

# Generates FES indicators for each year and plot in a selected province, climate model, climate scenario and management scenario
scenario_annual_province_indicators <- function(iprov, climate_model, climate_scen, management_scen, test = FALSE) {

  provinces <- c(8,17,25,43)
  provinceStrings <- c("Barcelona", "Girona", "Lleida", "Tarragona")
  
  provinceCode <- provinces[iprov]
  provinceName <- provinceStrings[iprov]

  if(test) {
    bind_file <- paste0("Rdata/Test_binded/Test_", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds")
  } else {
    bind_file <- paste0("Rdata/binded/", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds")
  }
  if(!file.exists(bind_file)) return(data.frame())
  
  scen_list <- readRDS(file = bind_file)
  

  # scen_list$summary_table = summary_table 

  tt <- scen_list$tree_table
  ctt <- scen_list$cut_tree_table
  dtt <- scen_list$dead_tree_table
  st <- scen_list$shrub_table
  dst <- scen_list$dead_shrub_table
  summary_table <- NULL
  if(!formes) {
    summary_table <- scen_list$summary_table
  } else {
    bind_file <- paste0("Rdata/FORMES/wb_summary_binded/", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds")
    if(file.exists(bind_file))   summary_table <- readRDS(file = bind_file)
  }
  
  # Structure (all trees)
  struct_trees <- tt |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(TreeRichness = length(unique(Species)), .groups = "drop")

  # Structure (no saplings)
  struct_nosapl <- tt |>
    dplyr::filter(DBH> 7.5) |>
    dplyr::mutate(BA = N*pi*(DBH/200)^2) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(TreeDensity = sum(N, na.rm=TRUE), 
                     BasalArea = sum(BA, na.rm=TRUE), 
                     meanDBH = weighted.mean(DBH, N),
                     sdDBH  = sqrt(wtd.var(DBH, weights = N)),
                     meanHeight = weighted.mean(Height, N),
                     sdHeight  = sqrt(wtd.var(Height, weights = N)),
                     .groups = "drop") |>
    dplyr::mutate(QMD = sqrt(BasalArea/(0.00007854*TreeDensity)),
                  cvDBH = sdDBH/meanDBH)

  # Structure shrubs
  if(!is.null(st)){
    struct_shrub <- st |>
      dplyr::select(-Step) |>
      dplyr::group_by(Climate, Management, Province, id, Year) |>
      dplyr::summarise(ShrubCover = min(100, sum(Cover, na.rm=TRUE)), 
                       ShrubRichness = length(unique(Species)), .groups = "drop")
  } else{
    struct_shrub = data.frame(Climate=NA, Management=NA, Province=NA,  id=NA, Year=NA, ShrubCover=NA, ShrubRichness=NA)
  }

  
  # Volume stock
  all_volume_stock <- tt |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(AllVolume = sum(Volume, na.rm=TRUE), .groups = "drop") 
  struct_volume_stock <- tt |>
    dplyr::filter((DBH > 22.5) & (Species != "Quercus ilex")) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(VolumeStructure = sum(Volume, na.rm=TRUE), .groups = "drop")
  firewood_volume_stock <- tt |>
    dplyr::filter((DBH <= 22.5) | (Species == "Quercus ilex")) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(VolumeFirewood = sum(Volume, na.rm=TRUE), .groups = "drop")
  volume_stock <- all_volume_stock |>
    dplyr::full_join(struct_volume_stock, by=c("Climate", "Management", "Province", "id", "Year")) |>
    dplyr::full_join(firewood_volume_stock, struct_volume_stock, by=c("Climate", "Management", "Province", "id", "Year"))
  
    
  # Volume cut
  struct_volume_cut <- ctt |>
    dplyr::filter((DBH > 22.5) & (Species != "Quercus ilex")) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(CutStructure = sum(Volume, na.rm=TRUE)/10, .groups = "drop") # we divide by 10 to have annual rate
  firewood_volume_cut <- ctt |>
    dplyr::filter((DBH <= 22.5) | (Species == "Quercus ilex")) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(CutFirewood = sum(Volume, na.rm=TRUE)/10, .groups = "drop")  # we divide by 10 to have annual rate
  volume_cut <- struct_volume_cut |>
    dplyr::full_join(firewood_volume_cut, by=c("Climate", "Management", "Province", "id", "Year")) |>
    tidyr::replace_na(list(CutFirewood = 0, CutStructure = 0)) |>
    dplyr::mutate(CutAll = CutStructure+CutFirewood) 

  # Biomass stock
  biom_trees <- tt |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(TreeBiomass = sum(Aerial+Roots, na.rm=TRUE), .groups = "drop")
  if(!is.null(st)){
    biom_shrubs <- st |>
      dplyr::select(-Step) |>
      dplyr::group_by(Climate, Management, Province, id, Year) |>
      dplyr::summarise(ShrubBiomass = sum(Aerial+Roots, na.rm=TRUE), .groups = "drop")
  } else{
    biom_shrubs = data.frame(Climate=NA, Management=NA, Province=NA,  id=NA,    Year=NA, ShrubBiomass=NA)
  }

  biom_live <- biom_trees |>
    dplyr::full_join(biom_shrubs, by=c("Climate", "Management", "Province", "id", "Year")) |>
    dplyr::mutate(LiveBiomass = TreeBiomass + ShrubBiomass)
  
  # Biomass dead
  tree_biom_dead <- dtt |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(TreeDeadBiomass = sum(Aerial+Roots, na.rm=TRUE),
                     N_dead = sum(N),
                     N_starvation = sum(N_starvation), 
                     N_dessication = sum(N_dessication), .groups = "drop") |>
    dplyr::mutate(TreeDeadBiomass_starvation = TreeDeadBiomass*N_starvation/N_dead,
                  TreeDeadBiomass_dessication = TreeDeadBiomass*N_dessication/N_dead,
                  TreeDeadBiomass_other = TreeDeadBiomass - TreeDeadBiomass_starvation - TreeDeadBiomass_dessication) |>
    dplyr::select(-c(N_dead, N_starvation, N_dessication))
  if(!is.null(dst)){
    shrub_biom_dead <- dst |>
      dplyr::select(-Step) |>
      dplyr::group_by(Climate, Management, Province, id, Year) |>
      dplyr::summarise(ShrubDeadBiomass = sum(Aerial+Roots, na.rm=TRUE),
                       Cover_dead = sum(Cover),
                       Cover_starvation = sum(Cover_starvation), 
                       Cover_dessication = sum(Cover_dessication), .groups = "drop") |>
      dplyr::mutate(ShrubDeadBiomass_starvation = ShrubDeadBiomass*Cover_starvation/Cover_dead,
                    ShrubDeadBiomass_dessication = ShrubDeadBiomass*Cover_dessication/Cover_dead,
                    ShrubDeadBiomass_other = ShrubDeadBiomass - ShrubDeadBiomass_starvation - ShrubDeadBiomass_dessication) |>
      dplyr::select(-c(Cover_dead, Cover_starvation, Cover_dessication))
  } else{
    shrub_biom_dead = data.frame(Climate=NA, Management=NA, Province=NA,  id=NA,    Year=NA, 
                                 ShrubDeadBiomass=NA, ShrubDeadBiomass_starvation=NA,ShrubDeadBiomass_dessication=NA, ShrubDeadBiomass_other=NA)
  }

  
  biom_dead <- tree_biom_dead |>
    dplyr::full_join(shrub_biom_dead, by=c("Climate", "Management", "Province", "id", "Year")) |>
    tidyr::replace_na(list(TreeDeadBiomass = 0, ShrubDeadBiomass = 0,
                           TreeDeadBiomass_other = 0, ShrubDeadBiomass_other = 0,
                           TreeDeadBiomass_starvation = 0, ShrubDeadBiomass_starvation = 0,
                           TreeDeadBiomass_dessication = 0, ShrubDeadBiomass_dessication = 0)) |>
    dplyr::mutate(DeadBiomass = TreeDeadBiomass+ShrubDeadBiomass,
                  DeadBiomass_other = TreeDeadBiomass_other + ShrubDeadBiomass_other,
                  DeadBiomass_starvation = TreeDeadBiomass_starvation + ShrubDeadBiomass_starvation,
                  DeadBiomass_dessication = TreeDeadBiomass_dessication + ShrubDeadBiomass_dessication)
  
  if(!is.null(scen_list$summary_table)){
  summary_table <- scen_list$summary_table |>
    dplyr::mutate(PPET = Precipitation/PET,
                  WaterUseEfficiency = NetPrimaryProduction/Transpiration,
                  CarbonUseEfficiency = SynthesisRespiration/NetPrimaryProduction,
                  BlueWater = Runoff+DeepDrainage,
                  RunoffCoefficient = BlueWater/Precipitation,
                  RegulationCoefficient = DeepDrainage/BlueWater) |>
    dplyr::group_by(Climate, Management, Province, id) |>
    dplyr::mutate(CumulativeDeepDrainage = cumsum(DeepDrainage),
                  CumulativeBlueWater = cumsum(BlueWater),
                  CumulativePrecipitation = cumsum(Precipitation),
                  CumulativePET = cumsum(PET)) |>
    dplyr::ungroup() |>
    dplyr::mutate(CumulativeRunoffCoefficient = CumulativeBlueWater/CumulativePrecipitation,
                  CumulativeRegulation = CumulativeDeepDrainage/CumulativeBlueWater,
                  CumulativePPET = CumulativePrecipitation/CumulativePET,
                  Year = ifelse(!is.na(scen_list$summary_table$Year), as.numeric(Year), NA))
  ###### code for FORMES inputs ###
  summary_table <- summary_table |> mutate(all_Year = summary_table$Year, .before = Year) |>
    mutate(Year = case_when(summary_table$Year %in% 2001:2010 ~ "2001-2010",
                            summary_table$Year %in% 2011:2020 ~ "2011-2020",
                            summary_table$Year %in% 2021:2030 ~ "2021-2030",
                            summary_table$Year %in% 2031:2040 ~ "2031-2040",
                            summary_table$Year %in% 2041:2050 ~ "2041-2050",
                            summary_table$Year %in% 2051:2060 ~ "2051-2060",
                            summary_table$Year %in% 2061:2070 ~ "2061-2070",
                            summary_table$Year %in% 2071:2080 ~ "2071-2080",
                            summary_table$Year %in% 2081:2090 ~ "2081-2090",
                            summary_table$Year %in% 2091:2100 ~ "2091-2100",
                            .default = "NA"))
  } else {
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
    summary_table = data.frame(matrix(ncol = length(column_names), nrow = 0))
    colnames(summary_table) <- column_names
  }
  #####-----###
  annual_vol_biom <- struct_trees |>
    dplyr::full_join(struct_nosapl, by=c("Climate", "Management", "Province", "id", "Year"))|>
    dplyr::full_join(struct_shrub, by=c("Climate", "Management", "Province", "id", "Year"))|>
    dplyr::full_join(volume_stock, by=c("Climate", "Management", "Province", "id", "Year"))|>
    dplyr::full_join(biom_live, by=c("Climate", "Management", "Province", "id", "Year")) |>
    dplyr::full_join(biom_dead, by=c("Climate", "Management", "Province", "id", "Year"))
    if(management_scen !="NOG"){
      annual_vol_biom <- annual_vol_biom |>
      dplyr::full_join(volume_cut, by=c("Climate", "Management", "Province", "id", "Year"))
    } else {
      annual_vol_biom$CutFirewood = 0
      annual_vol_biom$CutStructure = 0
      annual_vol_biom$CutAll = 0
    }
    # dplyr::full_join(summary_table, by=c("Climate", "Management", "Province", "id", "Year")) |>
  annual_vol_biom <- annual_vol_biom |>
    tidyr::replace_na(list(CutAll = 0, CutFirewood = 0, CutStructure = 0,
                           DeadBiomass = 0, TreeDeadBiomass = 0, ShrubDeadBiomass = 0,
                           DeadBiomass_starvation = 0, TreeDeadBiomass_starvation = 0, ShrubDeadBiomass_starvation = 0,
                           DeadBiomass_dessication = 0, TreeDeadBiomass_dessication = 0, ShrubDeadBiomass_dessication = 0,
                           DeadBiomass_other = 0, TreeDeadBiomass_other = 0, ShrubDeadBiomass_other = 0)) |>
    dplyr::arrange(Climate, Management, Province, id, Year) |>
    dplyr::group_by(Climate, Management, Province, id) |>
    dplyr::mutate(CumulativeCutFirewood = cumsum(CutFirewood),
                  CumulativeCutStructure = cumsum(CutStructure),
                  CumulativeCutAll = cumsum(CutAll),
                  CumulativeDeadBiomass = cumsum(DeadBiomass),
                  CumulativeDeadBiomass_starvation = cumsum(DeadBiomass_starvation),
                  CumulativeDeadBiomass_dessication = cumsum(DeadBiomass_dessication),
                  CumulativeDeadBiomass_other = cumsum(DeadBiomass_other),
                  CumulativeTreeDeadBiomass = cumsum(TreeDeadBiomass),
                  CumulativeTreeDeadBiomass_starvation = cumsum(TreeDeadBiomass_starvation),
                  CumulativeTreeDeadBiomass_dessication = cumsum(TreeDeadBiomass_dessication),
                  CumulativeTreeDeadBiomass_other = cumsum(TreeDeadBiomass_other),
                  CumulativeShrubDeadBiomass = cumsum(ShrubDeadBiomass),
                  CumulativeShrubDeadBiomass_starvation = cumsum(ShrubDeadBiomass_starvation),
                  CumulativeShrubDeadBiomass_dessication = cumsum(ShrubDeadBiomass_dessication),
                  CumulativeShrubDeadBiomass_other = cumsum(ShrubDeadBiomass_other)) |>
    tidyr::replace_na(list(CumulativeCutFirewood = 0, CumulativeCutStructure = 0, CumulativeCutAll = 0))
  return(annual_vol_biom)
}

# Generates FES indicators for each year and plot for the whole study area in a selected climate model, climate scenario and management scenario
scenario_annual_indicators<-function(climate_model, climate_scen, management_scen, test = FALSE) {
  BCN <- scenario_annual_province_indicators(1, climate_model, climate_scen, management_scen, test)
  GIR <- scenario_annual_province_indicators(2, climate_model, climate_scen, management_scen, test)
  LLE <- scenario_annual_province_indicators(3, climate_model, climate_scen, management_scen, test)
  TAR <- scenario_annual_province_indicators(4, climate_model, climate_scen, management_scen, test)
  if(test) {
    saveRDS(bind_rows(BCN, GIR, LLE, TAR),
            file = paste0("Rdata/Test_annual_indicators/Test_", management_scen, "_", climate_model, "_", climate_scen, ".rds"))
  } else {
    saveRDS(bind_rows(BCN, GIR, LLE, TAR),
            file = paste0("Rdata/annual_indicators/", management_scen, "_", climate_model, "_", climate_scen, ".rds"))
  }
}

scenario_annual_province_species_abundance <- function(iprov, climate_model, climate_scen, management_scen, test = FALSE) {
  
  provinces <- c(8,17,25,43)
  provinceStrings <- c("Barcelona", "Girona", "Lleida", "Tarragona")
  
  provinceCode <- provinces[iprov]
  provinceName <- provinceStrings[iprov]
  
  if(test) {
    bind_file <- paste0("Rdata/Test_binded/Test_", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds")
  } else {
    bind_file <- paste0("Rdata/binded/", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds")
  }
  if(!file.exists(bind_file)) return(data.frame())
  
  scen_list <- readRDS(file = bind_file)
  
  tt <- scen_list$tree_table
  st <- scen_list$shrub_table
  
  tree_species_abundance <- tt |>
    dplyr::group_by(Climate, Management, Province, Step, Year, Species) |>
    dplyr::summarise(Volume = sum(Volume, na.rm=TRUE),
                     Aerial = sum(Aerial, na.rm=TRUE),
                     Roots = sum(Roots, na.rm=TRUE),
                     .groups = "drop") |>
    dplyr::mutate(GrowthForm = "Tree")

  if(!is.null(st)){
    shrub_species_abundance <- st |>
      dplyr::group_by(Climate, Management, Province, Step, Year, Species) |>
      dplyr::summarise(Aerial = sum(Aerial, na.rm=TRUE),
                       Roots = sum(Roots, na.rm=TRUE),
                       .groups = "drop") |>
      dplyr::mutate(GrowthForm = "Shrub") 
  } else{
    shrub_species_abundance = data.frame(Climate=NA, Management=NA, Province=NA,  Setp=NA,  Year=NA,
                                         Species=NA, Aerial=NA, Roots=NA)
  }

  
  species_abundance <- tree_species_abundance |>
    dplyr::bind_rows(shrub_species_abundance)
  return(species_abundance)
}

scenario_species_abundance<-function(climate_model, climate_scen, management_scen, test = FALSE) {
  BCN <- scenario_annual_province_species_abundance(1, climate_model, climate_scen, management_scen, test)
  GIR <- scenario_annual_province_species_abundance(2, climate_model, climate_scen, management_scen, test)
  LLE <- scenario_annual_province_species_abundance(3, climate_model, climate_scen, management_scen, test)
  TAR <- scenario_annual_province_species_abundance(4, climate_model, climate_scen, management_scen, test)
  if(test) {
    saveRDS(bind_rows(BCN, GIR, LLE, TAR),
            file = paste0("Rdata/Test_species_abundance/Test_", management_scen, "_", climate_model, "_", climate_scen, ".rds"))
  } else {
    saveRDS(bind_rows(BCN, GIR, LLE, TAR),
            file = paste0("Rdata/species_abundance/", management_scen, "_", climate_model, "_", climate_scen, ".rds"))
  }
  
}

climate_model <- "mpiesm_rca4"
test <- FALSE
climate_scen = "rcp45"
management_scen = "BAU"

# (1) BAU
scenario_annual_indicators(climate_model, "rcp45", "BAU", test = test)
scenario_annual_indicators(climate_model, "rcp85", "BAU", test = test)

scenario_species_abundance(climate_model, "rcp45", "BAU", test = test)
scenario_species_abundance(climate_model, "rcp85", "BAU", test = test)

# (2) AMF
scenario_annual_indicators(climate_model, "rcp45", "AMF", test = test)
scenario_annual_indicators(climate_model, "rcp85", "AMF", test = test)

scenario_species_abundance(climate_model, "rcp45", "AMF", test = test)
scenario_species_abundance(climate_model, "rcp85", "AMF", test = test)

# (3) RSB
scenario_annual_indicators(climate_model, "rcp45", "RSB", test = test)
scenario_annual_indicators(climate_model, "rcp85", "RSB", test = test)

scenario_species_abundance(climate_model, "rcp45", "RSB", test = test)
scenario_species_abundance(climate_model, "rcp85", "RSB", test = test)

# (4) ASEA
scenario_annual_indicators(climate_model, "rcp45", "ASEA", test = test)
scenario_annual_indicators(climate_model, "rcp85", "ASEA", test = test)

scenario_species_abundance(climate_model, "rcp45", "ASEA", test = test)
scenario_species_abundance(climate_model, "rcp85", "ASEA", test = test)

# (5) ACG
scenario_annual_indicators(climate_model, "rcp45", "ACG", test = test)
scenario_annual_indicators(climate_model, "rcp85", "ACG", test = test)

scenario_species_abundance(climate_model, "rcp45", "ACG", test = test)
scenario_species_abundance(climate_model, "rcp85", "ACG", test = test)

# (6) NOG
scenario_annual_indicators(climate_model, "rcp45", "NOG", test = test)
scenario_annual_indicators(climate_model, "rcp85", "NOG", test = test)

scenario_species_abundance(climate_model, "rcp45", "NOG", test = test)
scenario_species_abundance(climate_model, "rcp85", "NOG", test = test)











