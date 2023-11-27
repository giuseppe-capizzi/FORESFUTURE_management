library(tidyverse)
library(medfate)

climate_model <- "mpiesm_rca4"

# Binds summary tables across decades of the same scenario
bind_scenario_summary_table <- function(iprov, climate_model, climate_scen, management_scen) {
  
  provinces <- c(8,17,25,43)
  provinceStrings <- c("Barcelona", "Girona", "Lleida", "Tarragona")
  
  provinceCode <- provinces[iprov]
  provinceName <- provinceStrings[iprov]
  
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
  cli::cli_progress_step("2001-2010")
  res_01_10 <- readRDS(paste0("../FORESFUTURE_WB/WB/historic/wb_", provinceCode,"_2001_2010.rds"))
  res_01_10 <- reshape_summary(res_01_10)
  td_01_10 <- bind_rows(res_01_10$summary) 
  par_01_10 <- readRDS(paste0("../FORESFUTURE_WB/PARground/historic/PARground_", provinceCode,"_2001_2010.rds"))
  td_01_10 <- td_01_10 |>
    left_join(sf::st_drop_geometry(par_01_10[,c("id", "PARground")]), by=c("id"))
  
  cli::cli_progress_step("2011-2020")
  res_11_20 <- readRDS(paste0("../FORESFUTURE_WB/WB/historic/wb_", provinceCode,"_2011_2020.rds"))
  res_11_20 <- reshape_summary(res_11_20)
  td_11_20 <- bind_rows(res_11_20$summary) 
  par_11_20 <- readRDS(paste0("../FORESFUTURE_WB/PARground/historic/PARground_", provinceCode,"_2011_2020.rds"))
  td_11_20 <- td_11_20 |>
    left_join(sf::st_drop_geometry(par_11_20[,c("id", "PARground")]), by=c("id"))
  
  td<- bind_rows(td_01_10, td_11_20)
  
  yearsIni <- seq(2021 ,2091, by=10)
  yearsFin <- seq(2030, 2100, by=10)
  for(iy in 1:length(yearsIni)) {
    cli::cli_progress_step(paste0(yearsIni[iy],"_", yearsFin[iy]))
    res_file <- paste0("../FORESFUTURE_WB/WB/", management_scen, "/wb_",climate_scen,"_", provinceCode, "_", yearsIni[iy],"_", yearsFin[iy],".rds")
    if(file.exists(res_file)) {
      res <- readRDS(res_file)
      res <- reshape_summary(res)
      td_i <- bind_rows(res$summary) 
      par_i <- readRDS(paste0("../FORESFUTURE_WB/PARground/", management_scen, "/PARground_",climate_scen,"_", provinceCode, "_", yearsIni[iy],"_", yearsFin[iy],".rds"))
      td_i <- td_i |>
        left_join(sf::st_drop_geometry(par_i[,c("id", "PARground")]), by=c("id"))
      td <- bind_rows(td, td_i)
    }
  }
  td$Climate <- paste0("rcp", climate_scen)
  td$Management <- management_scen
  td$Province <- provinceName
  td <- td |>
    relocate(Climate, Management, Province, .before = id)
  cli::cli_progress_step("Saving")
  saveRDS(td, file = paste0("Rdata/FORMES/wb_summary_binded/", provinceName, "_", management_scen, "_", climate_model,"_rcp", climate_scen, ".rds"))
  cli::cli_progress_done()
}

# (1) BAU
# bind_scenario_summary_table(1, climate_model, "45", "BAU")
# bind_scenario_summary_table(2, climate_model, "45", "BAU")
# bind_scenario_summary_table(3, climate_model, "45", "BAU")
# bind_scenario_summary_table(4, climate_model, "45", "BAU")
# bind_scenario_summary_table(1, climate_model, "85", "BAU")
# bind_scenario_summary_table(2, climate_model, "85", "BAU")
# bind_scenario_summary_table(3, climate_model, "85", "BAU")
# bind_scenario_summary_table(4, climate_model, "85", "BAU")

# (2) AMF
# bind_scenario_summary_table(1, climate_model, "45", "AMF")
# bind_scenario_summary_table(2, climate_model, "45", "AMF")
# bind_scenario_summary_table(3, climate_model, "45", "AMF")
# bind_scenario_summary_table(4, climate_model, "45", "AMF")
# bind_scenario_summary_table(1, climate_model, "85", "AMF")
# bind_scenario_summary_table(2, climate_model, "85", "AMF")
# bind_scenario_summary_table(3, climate_model, "85", "AMF")
# bind_scenario_summary_table(4, climate_model, "85", "AMF")

# (3) RSB
# bind_scenario_summary_table(1, climate_model, "45", "RSB")
# bind_scenario_summary_table(2, climate_model, "45", "RSB")
# bind_scenario_summary_table(3, climate_model, "45", "RSB")
# bind_scenario_summary_table(4, climate_model, "45", "RSB")
# bind_scenario_summary_table(1, climate_model, "85", "RSB")
# bind_scenario_summary_table(2, climate_model, "85", "RSB")
# bind_scenario_summary_table(3, climate_model, "85", "RSB")
# bind_scenario_summary_table(4, climate_model, "85", "RSB")

# (4) ASEA
# bind_scenario_summary_table(1, climate_model, "45", "ASEA")
# bind_scenario_summary_table(2, climate_model, "45", "ASEA")
# bind_scenario_summary_table(3, climate_model, "45", "ASEA")
# bind_scenario_summary_table(4, climate_model, "45", "ASEA")
# bind_scenario_summary_table(1, climate_model, "85", "ASEA")
# bind_scenario_summary_table(2, climate_model, "85", "ASEA")
# bind_scenario_summary_table(3, climate_model, "85", "ASEA")
# bind_scenario_summary_table(4, climate_model, "85", "ASEA")

# (5) ACG
# bind_scenario_summary_table(1, climate_model, "45", "ACG")
# bind_scenario_summary_table(2, climate_model, "45", "ACG")
# bind_scenario_summary_table(3, climate_model, "45", "ACG")
# bind_scenario_summary_table(4, climate_model, "45", "ACG")
# bind_scenario_summary_table(1, climate_model, "85", "ACG")
# bind_scenario_summary_table(2, climate_model, "85", "ACG")
# bind_scenario_summary_table(3, climate_model, "85", "ACG")
# bind_scenario_summary_table(4, climate_model, "85", "ACG")

# (6) NOG
# bind_scenario_summary_table(1, climate_model, "45", "NOG")
# bind_scenario_summary_table(2, climate_model, "45", "NOG")
# bind_scenario_summary_table(3, climate_model, "45", "NOG")
# bind_scenario_summary_table(4, climate_model, "45", "NOG")
# bind_scenario_summary_table(1, climate_model, "85", "NOG")
# bind_scenario_summary_table(2, climate_model, "85", "NOG")
# bind_scenario_summary_table(3, climate_model, "85", "NOG")
# bind_scenario_summary_table(4, climate_model, "85", "NOG")