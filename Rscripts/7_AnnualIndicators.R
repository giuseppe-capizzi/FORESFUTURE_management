library(tidyverse)

# Generates FES indicators for each year and plot in a selected province, climate model, climate scenario and management scenario
scenario_annual_province_indicators <- function(iprov, climate_model, climate_scen, management_scen) {
  
  provinces <- c(8,17,25,43)
  provinceStrings <- c("Barcelona", "Girona", "Lleida", "Tarragona")
  
  provinceCode <- provinces[iprov]
  provinceName <- provinceStrings[iprov]

  bind_file <- paste0("Rdata/binded/", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds")
  if(!file.exists(bind_file)) return(data.frame())
  
  scen_list <- readRDS(file = bind_file)
  
  tt <- scen_list$tree_table
  ctt <- scen_list$cut_tree_table
  dtt <- scen_list$dead_tree_table
  st <- scen_list$shrub_table
  dst <- scen_list$dead_shrub_table
  
  # Structure (no saplings)
  struct_nosapl <- tt |>
    dplyr::filter(DBH> 7.5) |>
    dplyr::mutate(BA = N*pi*(DBH/200)^2) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(TreeDensity = sum(N, na.rm=TRUE), 
                     BasalArea = sum(BA, na.rm=TRUE), .groups = "drop") |>
    dplyr::mutate(QMD = sqrt(BasalArea/(0.00007854*TreeDensity)))
  
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
    dplyr::summarise(CutStructure = sum(Volume, na.rm=TRUE), .groups = "drop")
  firewood_volume_cut <- ctt |>
    dplyr::filter((DBH <= 22.5) | (Species == "Quercus ilex")) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(CutFirewood = sum(Volume, na.rm=TRUE), .groups = "drop") 
  volume_cut <- struct_volume_cut |>
    dplyr::full_join(firewood_volume_cut, by=c("Climate", "Management", "Province", "id", "Year")) |>
    tidyr::replace_na(list(CutFirewood = 0, CutStructure = 0)) |>
    dplyr::mutate(CutAll = CutStructure+CutFirewood) 

  
  # Biomass stock
  biom_trees <- tt |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(TreeBiomass = sum(Aerial+Roots, na.rm=TRUE), .groups = "drop")
  biom_shrubs <- st |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(ShrubBiomass = sum(Aerial+Roots, na.rm=TRUE), .groups = "drop")
  
  # Biomass dead
  tree_biom_dead <- dtt |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(TreeDeadBiomass = sum(Aerial+Roots, na.rm=TRUE), .groups = "drop")
  shrub_biom_dead <- dst |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(ShrubDeadBiomass = sum(Aerial+Roots, na.rm=TRUE), .groups = "drop") 
  biom_dead <- tree_biom_dead |>
    dplyr::full_join(shrub_biom_dead, by=c("Climate", "Management", "Province", "id", "Year")) |>
    tidyr::replace_na(list(TreeDeadBiomass = 0, ShrubDeadBiomass = 0)) |>
    dplyr::mutate(DeadBiomass = TreeDeadBiomass+ShrubDeadBiomass)
  
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
                  Year = as.numeric(Year))

  annual_vol_biom <- struct_nosapl |>
    dplyr::full_join(volume_stock, by=c("Climate", "Management", "Province", "id", "Year"))|>
    dplyr::full_join(volume_cut, by=c("Climate", "Management", "Province", "id", "Year"))|>
    dplyr::full_join(biom_trees, by=c("Climate", "Management", "Province", "id", "Year")) |>
    dplyr::full_join(biom_shrubs, by=c("Climate", "Management", "Province", "id", "Year"))|>
    dplyr::full_join(biom_dead, by=c("Climate", "Management", "Province", "id", "Year")) |>
    dplyr::full_join(summary_table, by=c("Climate", "Management", "Province", "id", "Year")) |>
    tidyr::replace_na(list(CutAll = 0, CutFirewood = 0, CutStructure = 0,
                           DeadBiomass = 0, TreeDeadBiomass = 0, ShrubDeadBiomass = 0)) |>
    dplyr::arrange(Climate, Management, Province, id, Year) |>
    dplyr::group_by(Climate, Management, Province, id) |>
    dplyr::mutate(CumulativeCutFirewood = cumsum(CutFirewood),
                  CumulativeCutStructure = cumsum(CutStructure),
                  CumulativeCutAll = cumsum(CutAll),
                  CumulativeDeadBiomass = cumsum(DeadBiomass),
                  CumulativeTreeDeadBiomass = cumsum(TreeDeadBiomass),
                  CumulativeShrubDeadBiomass = cumsum(ShrubDeadBiomass)) |>
    tidyr::replace_na(list(CumulativeCutFirewood = 0, CumulativeCutStructure = 0, CumulativeCutAll = 0))
  return(annual_vol_biom)
}

# Generates FES indicators for each year and plot for the whole study area in a selected climate model, climate scenario and management scenario
scenario_annual_indicators<-function(climate_model, climate_scen, management_scen) {
  BCN <- scenario_annual_province_indicators(1, climate_model, climate_scen, management_scen)
  GIR <- scenario_annual_province_indicators(2, climate_model, climate_scen, management_scen)
  LLE <- scenario_annual_province_indicators(3, climate_model, climate_scen, management_scen)
  TAR <- scenario_annual_province_indicators(4, climate_model, climate_scen, management_scen)
  saveRDS(bind_rows(BCN, GIR, LLE, TAR),
          file = paste0("Rdata/annual_indicators/", management_scen, "_", climate_model, "_", climate_scen, ".rds"))
}

climate_model <- "mpiesm_rca4"
# (1) BAU
scenario_annual_indicators(climate_model, "rcp45", "BAU")
scenario_annual_indicators(climate_model, "rcp85", "BAU")
# (2) AMF
scenario_annual_indicators(climate_model, "rcp45", "AMF")
scenario_annual_indicators(climate_model, "rcp85", "AMF")
# (3) RSB
scenario_annual_indicators(climate_model, "rcp45", "RSB")
scenario_annual_indicators(climate_model, "rcp85", "RSB")
# (4) ASEA
scenario_annual_indicators(climate_model, "rcp45", "ASEA")
scenario_annual_indicators(climate_model, "rcp85", "ASEA")
# (5) ACG
scenario_annual_indicators(climate_model, "rcp45", "ACG")
scenario_annual_indicators(climate_model, "rcp85", "ACG")
# (6) NOG
scenario_annual_indicators(climate_model, "rcp45", "NOG")
scenario_annual_indicators(climate_model, "rcp85", "NOG")

