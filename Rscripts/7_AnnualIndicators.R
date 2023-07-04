library(tidyverse)

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
  

  all_volume_stock <- tt |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(AllVolume = sum(Volume, na.rm=TRUE), .groups = "drop")
  
  struct_volume_stock <- tt |>
    dplyr::filter((DBH > 22.5) & (Species != "Quercus ilex")) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(VolumeStructure = sum(Volume, na.rm=TRUE), .groups = "drop")
  
  struct_volume_cut <- ctt |>
    dplyr::filter((DBH > 22.5) & (Species != "Quercus ilex")) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(CutStructure = sum(Volume, na.rm=TRUE), .groups = "drop")
  
  firewood_volume_stock <- tt |>
    dplyr::filter((DBH <= 22.5) | (Species == "Quercus ilex")) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(VolumeFirewood = sum(Volume, na.rm=TRUE), .groups = "drop")
  
  firewood_volume_cut <- ctt |>
    dplyr::filter((DBH <= 22.5) | (Species == "Quercus ilex")) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(CutFirewood = sum(Volume, na.rm=TRUE), .groups = "drop")
  
  dens_ba_nosapl <- tt |>
    dplyr::filter(DBH> 7.5) |>
    dplyr::mutate(BA = N*pi*(DBH/200)^2) |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(TreeDensity = sum(N, na.rm=TRUE), 
                     BasalArea = sum(BA, na.rm=TRUE), .groups = "drop")
  
  biom_trees <- tt |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(TreeBiomass = sum(Aerial+Roots, na.rm=TRUE), .groups = "drop")
  
  tree_biom_dead <- dtt |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(TreeDeadBiomass = sum(Aerial+Roots, na.rm=TRUE), .groups = "drop") |>
    dplyr::group_by(Climate, Management, Province, id) |>
    dplyr::mutate(CumulativeTreeDeadBiomass = cumsum(TreeDeadBiomass))
  
  biom_shrubs <- st |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(ShrubBiomass = sum(Aerial+Roots, na.rm=TRUE), .groups = "drop")
  
  shrub_biom_dead <- dst |>
    dplyr::select(-Step) |>
    dplyr::group_by(Climate, Management, Province, id, Year) |>
    dplyr::summarise(ShrubDeadBiomass = sum(Aerial+Roots, na.rm=TRUE), .groups = "drop") |>
    dplyr::group_by(Climate, Management, Province, id) |>
    dplyr::mutate(CumulativeShrubDeadBiomass = cumsum(ShrubDeadBiomass))
  
  
  scen_list$summary_table$Year <- as.numeric(scen_list$summary_table$Year)
  dens_ba_nosapl |>
    dplyr::full_join(all_volume_stock, by=c("Climate", "Management", "Province", "id", "Year"))|>
    dplyr::full_join(struct_volume_stock, by=c("Climate", "Management", "Province", "id", "Year"))|>
    dplyr::full_join(firewood_volume_stock, by=c("Climate", "Management", "Province", "id", "Year"))|>
    dplyr::full_join(struct_volume_cut, by=c("Climate", "Management", "Province", "id", "Year"))|>
    dplyr::full_join(firewood_volume_cut, by=c("Climate", "Management", "Province", "id", "Year"))|>
    dplyr::full_join(biom_trees, by=c("Climate", "Management", "Province", "id", "Year")) |>
    dplyr::full_join(tree_biom_dead, by=c("Climate", "Management", "Province", "id", "Year")) |>
    dplyr::full_join(biom_shrubs, by=c("Climate", "Management", "Province", "id", "Year"))|>
    dplyr::full_join(shrub_biom_dead, by=c("Climate", "Management", "Province", "id", "Year")) |>
    dplyr::full_join(scen_list$summary_table, by=c("Climate", "Management", "Province", "id", "Year")) -> annual_vol_biom
  
  annual_vol_biom$Year[is.na(annual_vol_biom$Year)] <- 2000
  annual_vol_biom$CutFirewood[is.na(annual_vol_biom$CutFirewood)] <- 0
  annual_vol_biom$CutStructure[is.na(annual_vol_biom$CutStructure)] <- 0
  return(annual_vol_biom)
}

scenario_annual_indicators<-function(climate_model, climate_scen, management_scen) {
  BCN <- scenario_annual_province_indicators(1, climate_model, climate_scen, management_scen)
  GIR <- scenario_annual_province_indicators(2, climate_model, climate_scen, management_scen)
  LLE <- scenario_annual_province_indicators(3, climate_model, climate_scen, management_scen)
  TAR <- scenario_annual_province_indicators(4, climate_model, climate_scen, management_scen)
  saveRDS(bind_rows(BCN, GIR, LLE, TAR),
          file = paste0("Rdata/annual_indicators/", management_scen, "_", climate_model, "_", climate_scen, ".rds"))
}

scenario_annual_indicators(climate_model, "rcp45", "BAU")
scenario_annual_indicators(climate_model, "rcp85", "BAU")
scenario_annual_indicators(climate_model, "rcp45", "AMF")
