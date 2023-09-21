library(tidyverse)
library(medfate)
source("Rscripts/A2_utils.R")

bind_scenario_volume_table <- function(provinceName, climate_model, climate_scen, management_scen, test = FALSE) {
  td <- data.frame()
  
  if(test) {
    res_01_10 <- readRDS(paste0("Rdata/Test_historic/test_", provinceName,"_2001_2010.rds"))
    res_11_20 <- readRDS(paste0("Rdata/Test_historic/test_", provinceName,"_2011_2020.rds"))
  } else {
    res_01_10 <- readRDS(paste0("Rdata/historic/", provinceName,"_2001_2010.rds"))
    res_11_20 <- readRDS(paste0("Rdata/historic/", provinceName,"_2011_2020.rds"))
  } 
  
  td_01_10 <- res_01_10$result_volumes
  td_11_20 <- res_11_20$result_volumes
  td<- bind_rows(td, td_01_10, td_11_20)
  
  yearsIni <- seq(2021 ,2091, by=10)
  yearsFin <- seq(2030, 2100, by=10)
  for(iy in 1:length(yearsIni)) {
    if(test) {
      res_file <- paste0("Rdata/Test_", management_scen, "/test_", management_scen, "_", provinceName,"_", climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds")
    } else {
      res_file <- paste0("Rdata/", management_scen, "/", management_scen, "_", provinceName,"_", climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds")
    }
    if(file.exists(res_file)) {
      res <- readRDS(res_file)
      td_i <- res$result_volumes
      td <- bind_rows(td, td_i)
    }
  }
  td$Climate <- climate_scen
  td$Management <- management_scen
  td$Province <- provinceName
  td <- td |>
    relocate(Climate, Management, Province, .before = Year)
  td$cumulative_extracted_demand = cumsum(td$extracted_target)
  td$cumulative_extraction = cumsum(td$extracted)
  td$cumulative_growth = cumsum(td$growth)
  td$cumulative_nominal_demand = cumsum(td$nominal_demand)
  return(td)
}

# Binds tree and shrub tables across decades of the same scenario
bind_scenario_structural_table <- function(structural_table, provinceName, climate_model, climate_scen, management_scen, test = FALSE) {
  td <- data.frame()
  
  if(test) {
    res_01_10 <- readRDS(paste0("Rdata/Test_historic/test_", provinceName,"_2001_2010.rds"))
    res_11_20 <- readRDS(paste0("Rdata/Test_historic/test_", provinceName,"_2011_2020.rds"))
  } else {
    res_01_10 <- readRDS(paste0("Rdata/historic/", provinceName,"_2001_2010.rds"))
    res_11_20 <- readRDS(paste0("Rdata/historic/", provinceName,"_2011_2020.rds"))
  }
  td_01_10 <- bind_rows(res_01_10$result_sf[[structural_table]]) |>
    select(-c("Z50", "Z95"))
  
  td_11_20<- bind_rows(res_11_20$result_sf[[structural_table]]) |>
    select(-c("Z50", "Z95")) |>
    mutate(Step = Step+10)|>
    filter(!is.na(Year))
  td<- bind_rows(td, td_01_10, td_11_20)
  
  yearsIni <- seq(2021 ,2091, by=10)
  yearsFin <- seq(2030, 2100, by=10)
  for(iy in 1:length(yearsIni)) {
    if(test) {
      res_file <- paste0("Rdata/Test_", management_scen, "/test_", management_scen, "_", provinceName,"_", climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds")
    } else {
      res_file <- paste0("Rdata/", management_scen, "/", management_scen, "_", provinceName,"_", climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds")
    }
    if(file.exists(res_file)) {
      res <- readRDS(res_file)
      td_i <- bind_rows(res$result_sf[[structural_table]]) |>
        select(-c("Z50", "Z95")) |>
        mutate(Step = Step+10*(iy+1))|>
        filter(!is.na(Year))
      td <- bind_rows(td, td_i)
    }
  }
  td$Climate <- climate_scen
  td$Management <- management_scen
  td$Province <- provinceName
  td <- td |>
    relocate(Climate, Management, Province, .before = id)
  
  return(td)
}
# Binds summary tables across decades of the same scenario
bind_scenario_summary_table <- function(provinceName, climate_model, climate_scen, management_scen, test = FALSE) {
  td <- data.frame()
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
  if(test) {
    res_01_10 <- readRDS(paste0("Rdata/Test_historic/test_", provinceName,"_2001_2010.rds"))
    res_11_20 <- readRDS(paste0("Rdata/Test_historic/test_", provinceName,"_2011_2020.rds"))
  } else {
    res_01_10 <- readRDS(paste0("Rdata/historic/", provinceName,"_2001_2010.rds"))
    res_11_20 <- readRDS(paste0("Rdata/historic/", provinceName,"_2011_2020.rds"))
  }
  res_01_10$result_sf <- reshape_summary(res_01_10$result_sf)
  td_01_10 <- bind_rows(res_01_10$result_sf$summary) 
  res_11_20$result_sf <- reshape_summary(res_11_20$result_sf)
  td_11_20 <- bind_rows(res_11_20$result_sf$summary) 
  td<- bind_rows(td, td_01_10, td_11_20)
  
  yearsIni <- seq(2021 ,2091, by=10)
  yearsFin <- seq(2030, 2100, by=10)
  for(iy in 1:length(yearsIni)) {
    if(test) {
      res_file <- paste0("Rdata/Test_", management_scen, "/test_", management_scen, "_", provinceName,"_", climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds")
    } else {
      res_file <- paste0("Rdata/", management_scen, "/", management_scen, "_", provinceName,"_", climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds")
    }
    if(file.exists(res_file)) {
      res <- readRDS(res_file)
      res$result_sf <- reshape_summary(res$result_sf)
      td_i <- bind_rows(res$result_sf$summary) 
      td <- bind_rows(td, td_i)
    }
  }
  td$Climate <- climate_scen
  td$Management <- management_scen
  td$Province <- provinceName
  td <- td |>
    relocate(Climate, Management, Province, .before = id)
  return(td)
}
# Binds summary tables, binds structural tables, calculates volume and biomass and stores result
bind_scenario_province_results <- function(iprov, climate_model, climate_scen, management_scen, test = FALSE) {
  
  
  provinces <- c(8,17,25,43)
  provinceStrings <- c("Barcelona", "Girona", "Lleida", "Tarragona")
  provinceCode <- provinces[iprov]
  provinceName <- provinceStrings[iprov]

  cli::cli_h3(paste0("Binding scenario results for province ", provinceName, ", ", climate_model, ", " , climate_scen, ", ", management_scen))
    
  DBHclasses <- c(0, seq(2.5, 200, by = 5), 2000)
  
  cli::cli_progress_step("Volume table")
  vol_table <- bind_scenario_volume_table(provinceName, climate_model, climate_scen, management_scen, test)
  
  # Calculating tree volume and biomass
  cli::cli_progress_step("Tree table")
  tt<- bind_scenario_structural_table("tree_table", provinceName, climate_model, climate_scen, management_scen, test)
  tt$Volume <-volume_scenario(tt, SpParamsMED, provinceCode)
  biom <- tree_biomass_scenario(tt, SpParamsMED, as.CO2 = TRUE)
  tt$Aerial <- biom$Aerial/1000# From kg/ha to Mg/ha
  tt$Roots <- biom$Roots/1000 # From kg/ha to Mg/ha
  tt$DBHclass <- cut(tt$DBH, DBHclasses, right = FALSE)
  
  cli::cli_progress_step("Dead tree table")
  dtt<- bind_scenario_structural_table("dead_tree_table", provinceName, climate_model, climate_scen, management_scen, test)
  dtt$Volume <-volume_scenario(dtt, SpParamsMED, provinceCode)
  biom <- tree_biomass_scenario(dtt, SpParamsMED, as.CO2 = TRUE)
  dtt$Aerial <- biom$Aerial/1000# From kg/ha to Mg/ha
  dtt$Roots <- biom$Roots/1000 # From kg/ha to Mg/ha
  dtt$DBHclass <- cut(dtt$DBH, DBHclasses, right = FALSE)
  
  cli::cli_progress_step("Cut tree table")
  ctt<- bind_scenario_structural_table("cut_tree_table", provinceName, climate_model, climate_scen, management_scen, test)
  ctt$Volume <-volume_scenario(ctt, SpParamsMED, provinceCode)
  biom <- tree_biomass_scenario(ctt, SpParamsMED, as.CO2 = TRUE)
  ctt$Aerial <- biom$Aerial/1000# From kg/ha to Mg/ha
  ctt$Roots <- biom$Roots/1000 # From kg/ha to Mg/ha
  ctt$DBHclass <- cut(ctt$DBH, DBHclasses, right = FALSE)
  
  cli::cli_progress_step("Shrub table")
  st <- bind_scenario_structural_table("shrub_table", provinceName, climate_model, climate_scen, management_scen, test)
  st <-shrub_biomass_scenario(st, SpParamsMED, as.CO2 = TRUE)
  
  cli::cli_progress_step("Dead shrub table")
  dst<- bind_scenario_structural_table("dead_shrub_table", provinceName, climate_model, climate_scen, management_scen, test)
  dst <-shrub_biomass_scenario(dst, SpParamsMED, as.CO2 = TRUE)
  
  cli::cli_progress_step("Cut shrub table")
  cst<- bind_scenario_structural_table("cut_shrub_table", provinceName, climate_model, climate_scen, management_scen, test)
  cst <-shrub_biomass_scenario(cst, SpParamsMED, as.CO2 = TRUE)
  
  cli::cli_progress_step("Summary table")
  summary_table <- bind_scenario_summary_table(provinceName, climate_model, climate_scen, management_scen, test)

  
  cli::cli_progress_step("Storing list")
  scen_list <- list(volume_table = vol_table,
                    tree_table = tt, dead_tree_table = dtt, cut_tree_table = ctt,
                    shrub_table = st, dead_shrub_table = dst, cut_shrub_table = cst,
                    summary_table = summary_table)
  
  
  if(test) {
    saveRDS(scen_list, file = paste0("Rdata/Test_binded/Test_", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds"))
  } else {
    saveRDS(scen_list, file = paste0("Rdata/binded/", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds"))
  }
  cli::cli_progress_done()
}

climate_model <- "mpiesm_rca4"

test <- FALSE
# (1) BAU
# bind_scenario_province_results(1, climate_model, "rcp45", "BAU", test = test)
# bind_scenario_province_results(2, climate_model, "rcp45", "BAU", test = test)
# bind_scenario_province_results(3, climate_model, "rcp45", "BAU", test = test)
# bind_scenario_province_results(4, climate_model, "rcp45", "BAU", test = test)
# bind_scenario_province_results(1, climate_model, "rcp85", "BAU", test = test)
# bind_scenario_province_results(2, climate_model, "rcp85", "BAU", test = test)
# bind_scenario_province_results(3, climate_model, "rcp85", "BAU", test = test)
# bind_scenario_province_results(4, climate_model, "rcp85", "BAU", test = test)

# (2) AMF
# bind_scenario_province_results(1, climate_model, "rcp45", "AMF", test = test)
# bind_scenario_province_results(2, climate_model, "rcp45", "AMF", test = test)
# bind_scenario_province_results(3, climate_model, "rcp45", "AMF", test = test)
# bind_scenario_province_results(4, climate_model, "rcp45", "AMF", test = test)
# bind_scenario_province_results(1, climate_model, "rcp85", "AMF", test = test)
# bind_scenario_province_results(2, climate_model, "rcp85", "AMF", test = test)
# bind_scenario_province_results(3, climate_model, "rcp85", "AMF", test = test)
# bind_scenario_province_results(4, climate_model, "rcp85", "AMF", test = test)

# (3) RSB
# bind_scenario_province_results(1, climate_model, "rcp45", "RSB", test = test)
# bind_scenario_province_results(2, climate_model, "rcp45", "RSB", test = test)
# bind_scenario_province_results(3, climate_model, "rcp45", "RSB", test = test)
# bind_scenario_province_results(4, climate_model, "rcp45", "RSB", test = test)
# bind_scenario_province_results(1, climate_model, "rcp85", "RSB", test = test)
# bind_scenario_province_results(2, climate_model, "rcp85", "RSB", test = test)
# bind_scenario_province_results(3, climate_model, "rcp85", "RSB", test = test)
# bind_scenario_province_results(4, climate_model, "rcp85", "RSB", test = test)

# (4) ASEA
# bind_scenario_province_results(1, climate_model, "rcp45", "ASEA", test = test)
# bind_scenario_province_results(2, climate_model, "rcp45", "ASEA", test = test)
# bind_scenario_province_results(3, climate_model, "rcp45", "ASEA", test = test)
# bind_scenario_province_results(4, climate_model, "rcp45", "ASEA", test = test)
# bind_scenario_province_results(1, climate_model, "rcp85", "ASEA", test = test)
# bind_scenario_province_results(2, climate_model, "rcp85", "ASEA", test = test)
# bind_scenario_province_results(3, climate_model, "rcp85", "ASEA", test = test)
# bind_scenario_province_results(4, climate_model, "rcp85", "ASEA", test = test)

# (5) ACG
# bind_scenario_province_results(1, climate_model, "rcp45", "ACG", test = test)
# bind_scenario_province_results(2, climate_model, "rcp45", "ACG", test = test)
# bind_scenario_province_results(3, climate_model, "rcp45", "ACG", test = test)
# bind_scenario_province_results(4, climate_model, "rcp45", "ACG", test = test)
# bind_scenario_province_results(1, climate_model, "rcp85", "ACG", test = test)
# bind_scenario_province_results(2, climate_model, "rcp85", "ACG", test = test)
# bind_scenario_province_results(3, climate_model, "rcp85", "ACG", test = test)
# bind_scenario_province_results(4, climate_model, "rcp85", "ACG", test = test)

# (6) NOG
# bind_scenario_province_results(1, climate_model, "rcp45", "NOG", test = test)
# bind_scenario_province_results(2, climate_model, "rcp45", "NOG", test = test)
# bind_scenario_province_results(3, climate_model, "rcp45", "NOG", test = test)
# bind_scenario_province_results(4, climate_model, "rcp45", "NOG", test = test)
# bind_scenario_province_results(1, climate_model, "rcp85", "NOG", test = test)
# bind_scenario_province_results(2, climate_model, "rcp85", "NOG", test = test)
# bind_scenario_province_results(3, climate_model, "rcp85", "NOG", test = test)
# bind_scenario_province_results(4, climate_model, "rcp85", "NOG", test = test)
