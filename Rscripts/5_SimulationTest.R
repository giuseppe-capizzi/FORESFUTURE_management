library(medfateland)
library(medfate)


# Parameters --------------------------------------------------------------

climate_base = "emf/datasets/Climate/"

ntest <- 200
chunk_size <- 10
num_cores <- 20

subset_initial = FALSE
common_2001_2010 = FALSE
common_2011_2020 = FALSE
BAU_2021_2100 = FALSE
AMF_2021_2100 = TRUE
RSB_2021_2100 = FALSE
ASEA_2021_2100 = FALSE
ACG_2021_2100 = FALSE

local_control <- defaultControl()
local_control$fireHazardResults <- TRUE


# Preliminaries -----------------------------------------------------------
cli::cli_h1(paste0("PRELIMINARIES"))

cli::cli_li(paste0("Loading plot data"))
nfiplot <- readRDS("Rdata/nfiplot.rds")
nfiplot$represented_area <- nfiplot$area

cli::cli_li(paste0("Reference wood demand"))
volumes_2001_2010 <- c("Abies alba" = 4.783,
                       "Alnus glutinosa" = 1.79,
                       "Fraxinus spp." = 0.90,
                       "Castanea sativa" = 13.6,
                       "Eucalyptus spp." = 19.7,
                       "Juniperus communis" = 7.05, # Altres coniferes
                       "Fagus sylvatica" = 12.92, 
                       "Pinus halepensis" = 81.93,
                       "Pinus nigra" = 97.93,
                       "Pinus pinaster" = 21.90,
                       "Pinus pinea" = 20.17,
                       "Pinus sylvestris" = 157.16,
                       "Pinus uncinata" = 15.34,
                       "Pinus radiata" = 21.76,
                       "Populus spp." = 50.39,
                       "Pseudotsuga menziesii" = 6.49,
                       "Quercus petraea" = 2.57, # Quercus spp.
                       "Quercus ilex" = 7.37,
                       "Platanus spp." = 4.55,
                       "Prunus spp." = 2.40, # Altres caducifolis
                       "Robinia pseudacacia" = 0.59)*1000 # Multiply by 1000 to achieve m3
volumes_2011_2020 <- c("Abies alba" = 3.82,
                       "Alnus glutinosa" = 1.26,
                       "Fraxinus spp." = 1.74,
                       "Castanea sativa" = 10.32,
                       "Eucalyptus spp." = 6.28,
                       "Juniperus communis" = 3.73, # Altres coniferes
                       "Fagus sylvatica" = 10.93, 
                       "Pinus halepensis" = 151.84,
                       "Pinus nigra" = 104.26,
                       "Pinus pinaster" = 63.01,
                       "Pinus pinea" = 39.36,
                       "Pinus sylvestris" = 187.89,
                       "Pinus uncinata" = 22.35,
                       "Pinus radiata" = 29.73,
                       "Populus spp." = 41.01,
                       "Pseudotsuga menziesii" = 6.49,
                       "Quercus petraea" = 1.31, # Quercus spp.
                       "Quercus ilex" = 3.07,
                       "Platanus spp." = 6.67,
                       "Prunus spp." = 1.16, # Altres caducifolis
                       "Robinia pseudacacia" = 0.80)*1000 # Multiply by 1000 to achieve m3
# Average volumes
volumes_2001_2020 <- (volumes_2001_2010 + volumes_2011_2020)/2

data("defaultPrescriptionsBySpecies")
# Set management units as a function of dominant species
assign_management_unit <- function(dominant_tree_species, prescription_by_species) {
  n_units <- nrow(prescription_by_species)
  management_unit <- rep(NA, length(dominant_tree_species))
  sp_index_list <- strsplit(prescription_by_species$SpIndex, "/")
  for(i in 1:n_units) {
    sp_ind <- sp_index_list[[i]]
    sp_names <- SpParamsMED$Name[SpParamsMED$SpIndex %in% sp_ind]
    # cat(paste0("     ", i," ", paste0(sp_names, collapse = "/"), " ", paste0(sp_ind, collapse = "/"),"\n"))
    management_unit[dominant_tree_species %in% sp_names] <- i
  }
  return(management_unit)
}



cli::cli_li(paste0("Defining summary function"))
summary_scenario <- function(object, ...) {
  summary_std <- medfate::summary.fordyn(object, 
                                         output = "WaterBalance", 
                                         freq = "years", FUN = sum, na.rm=TRUE) # fordyn summar
  summary_fire <- medfate::summary.fordyn(object, 
                                          output = "FireHazard", 
                                          freq = "years", 
                                          FUN = max, na.rm = TRUE)
  return(cbind(summary_std, summary_fire[,c(13,14), drop = FALSE]))
}

if(subset_initial) {
  cli::cli_li(paste0("Subsetting ", ntest, " random test plots"))
  nfiplot_test <- nfiplot[sample(nrow(nfiplot), ntest), ]
  cli::cli_li(paste0("Storing initial state"))
  saveRDS(nfiplot_test, "Rdata/test_initial.rds")
}


# Historical 2001-2010 --------------------------------------------------------

if(common_2001_2010) {
  cli::cli_h1(paste0("SIMULATION 2001-2010 (all scenarios)"))
  cli::cli_li(paste0("Loading initial state"))
  nfiplot_test <- readRDS("Rdata/test_initial.rds")
  
  cli::cli_li(paste0("Assigning management units by dominant species and excluding plots from management"))
  nfiplot_test$management_unit <- assign_management_unit(nfiplot_test$dominant_tree_species, defaultPrescriptionsBySpecies)
  nfiplot_test$management_unit[nfiplot_test$LowQuality] <- NA
  nfiplot_test$management_unit[nfiplot_test$management_BAU==0] <- NA
  
  cli::cli_li(paste0("Defining management scenario (historical demand)"))

  volumes_2001_2010_test <- volumes_2001_2010/(nrow(nfiplot)/nrow(nfiplot_test))
  scen_2001_2010_test <- create_management_scenario(defaultPrescriptionsBySpecies,  
                                                    volumes_2001_2010_test) 
  
  years <- 2001:2010
  cli::cli_li(paste0("Loading interpolators for years ", years[1]," to ", years[length(years)]))
  interpolators <- vector("list", length(years))
  for(iy in 1:length(years)) {
    interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                        paste0("Products/InterpolationData/Catalunya/Historic/calibrated_2.0/interpolator_", years[iy],"_calibrated.nc"))
    interpolators[[iy]] <- meteoland::read_interpolator(interpolator_file)
    file.remove(interpolator_file)
  }
  res_01_10 <- fordyn_scenario(nfiplot_test, SpParamsMED, meteo = interpolators,
                               volume_function = NULL, local_control = local_control,
                               management_scenario = scen_2001_2010_test, summary_function = summary_scenario,
                               parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
  
  cli::cli_li(paste0("Storing results"))
  saveRDS(res_01_10, "Rdata/test_2001_2010.rds")
}


# Historical 2011-2020 --------------------------------------------------------

if(common_2011_2020) {
  cli::cli_h1(paste0("SIMULATION 2011-2020 (all scenarios)"))
  
  cli::cli_li(paste0("Recovering previous run"))
  res_01_10 <- readRDS("Rdata/test_2001_2010.rds")

  
  cli::cli_li(paste0("Defining management scenario (historical demand)"))
  
  volumes_2011_2020_test <- volumes_2011_2020/(nrow(nfiplot)/nrow(res_01_10$next_sf))
  scen_2011_2020_test <- create_management_scenario(defaultPrescriptionsBySpecies,  
                                                    volumes_2011_2020_test) 
  
  years <- 2011:2020
  cli::cli_li(paste0("Loading interpolators for years ", years[1]," to ", years[length(years)]))
  interpolators <- vector("list", length(years))
  for(iy in 1:length(years)) {
    interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                        paste0("Products/InterpolationData/Catalunya/Historic/calibrated_2.0/interpolator_", years[iy],"_calibrated.nc"))
    interpolators[[iy]] <- meteoland::read_interpolator(interpolator_file)
    file.remove(interpolator_file)
  }
  
  res_11_20 <- fordyn_scenario(res_01_10, SpParamsMED, meteo = interpolators,
                               volume_function = NULL, local_control = local_control,
                               management_scenario = scen_2011_2020_test, summary_function = summary_scenario,
                               parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
  
  cli::cli_li(paste0("Storing results"))
  saveRDS(res_11_20, "Rdata/test_2011_2020.rds")
}


# Business as usual (BAU) -------------------------------------------------

if(BAU_2021_2100) {
  climate_models <- "mpiesm_rca4"
  climate_scens <- c("rcp45", "rcp85")
  for(climate_model in climate_models) {
    for(climate_scen in climate_scens) {
      cli::cli_h1(paste0("SIMULATION 2021-2100 / BAU / ", climate_model, " / ", climate_scen))
      
      cli::cli_li(paste0("Recovering end of historical run"))
      res <- readRDS("Rdata/test_2011_2020.rds")
      
      cli::cli_li(paste0("Re-assigning management units by dominant species and excluding plots from management"))
      next_sf <- res$next_sf
      next_sf$management_unit <- assign_management_unit(next_sf$dominant_tree_species, defaultPrescriptionsBySpecies)
      next_sf$management_unit[next_sf$LowQuality] <- NA
      next_sf$management_unit[next_sf$management_BAU==0] <- NA
      res$next_sf <- next_sf
      
      yearsIni <- seq(2021 , 2091, by=10)
      yearsFin <- seq(2030, 2100, by=10)
      for(iy in 1:length(yearsIni)) {
        cli::cli_li(paste0("Loading interpolator for years ", yearsIni[iy]," to ", yearsFin[iy]))
        interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                            paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                   climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                   yearsIni[iy], "_", yearsFin[iy],".nc"))
        interpolator <- meteoland::read_interpolator(interpolator_file)
        file.remove(interpolator_file)
        
        cli::cli_li(paste0("Defining management scenario (30% extraction rates)"))
        volumes_BAU_test <- volumes_2001_2020/(nrow(nfiplot)/nrow(res$next_sf))
        rates <- rep(30, 10) # 30% extraction rate
        names(rates) <- as.character(yearsIni[iy]:yearsFin[iy])
        scen_BAU_test <- create_management_scenario(units = defaultPrescriptionsBySpecies,  
                                                    annual_demand_by_species = volumes_BAU_test,
                                                    extraction_rate_by_year = rates) 
        
        res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                               volume_function = NULL, local_control = local_control,
                               management_scenario = scen_BAU_test, summary_function = summary_scenario,
                               parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
        
        cli::cli_li(paste0("Storing results"))
        saveRDS(res, paste0("Rdata/test_BAU_",climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds"))
      }
    }
  }
}


# Alta mobilització de fusta (AMF) ----------------------------------------

if(AMF_2021_2100) {
  
  climate_models <- "mpiesm_rca4"
  climate_scens <- c("rcp45", "rcp85")
  for(climate_model in climate_models) {
    for(climate_scen in climate_scens) {
      cli::cli_h1(paste0("SIMULATION 2021-2100 / AMF / ", climate_model, " / ", climate_scen))
      
      cli::cli_li(paste0("Recovering end of historical run"))
      res <- readRDS("Rdata/test_2011_2020.rds")
      
      cli::cli_li(paste0("Re-assigning management units by dominant species and excluding plots from management"))
      next_sf <- res$next_sf
      next_sf$management_unit <- assign_management_unit(next_sf$dominant_tree_species, defaultPrescriptionsBySpecies)
      next_sf$management_unit[next_sf$LowQuality] <- NA
      next_sf$management_unit[next_sf$managment_AMF==0] <- NA
      res$next_sf <- next_sf
      
      
      # 2021-2030
      cli::cli_li(paste0("Loading interpolator for years ", 2021," to ", 2030))
      interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                          paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                 climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                 2021, "_", 2030,".nc"))
      interpolator <- meteoland::read_interpolator(interpolator_file)
      file.remove(interpolator_file)
      
      cli::cli_li(paste0("Defining management scenario (40% extraction rates)"))
      volumes_AMF_test <- volumes_2001_2020/(nrow(nfiplot)/nrow(res$next_sf))
      rates <- rep(40, 10) # 40% extraction rate
      names(rates) <- as.character(2021:2030)
      scen_AMF_test <- create_management_scenario(units = defaultPrescriptionsBySpecies,  
                                                  annual_demand_by_species = volumes_AMF_test,
                                                  extraction_rate_by_year = rates) 

      res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                             volume_function = NULL, local_control = local_control,
                             management_scenario = scen_AMF_test, summary_function = summary_scenario,
                             parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
      saveRDS(res, paste0("Rdata/test_AMF_",climate_model,"_",climate_scen,"_2021_2030.rds"))
      
      # 2031-2100
      yearsIni <- seq(2031 , 2091, by=10)
      yearsFin <- seq(2040, 2100, by=10)
      for(iy in 1:length(yearsIni)) {
        
        cli::cli_li(paste0("Loading interpolator for years ", yearsIni[iy]," to ", yearsFin[iy]))
        interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                            paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                   climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                   yearsIni[iy], "_", yearsFin[iy],".nc"))
        interpolator <- meteoland::read_interpolator(interpolator_file)
        file.remove(interpolator_file)
        
        cli::cli_li(paste0("Defining management scenario (70% extraction rates)"))
        rates <- rep(70, 10) # 70% extraction rate
        names(rates) <- as.character(yearsIni[iy]:yearsFin[iy])
        scen_AMF_test <- create_management_scenario(units = defaultPrescriptionsBySpecies,  
                                                    annual_demand_by_species = volumes_AMF_test,
                                                    extraction_rate_by_year = rates) 
        
        res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                               volume_function = NULL, local_control = local_control,
                               management_scenario = scen_AMF_test, summary_function = summary_scenario,
                               parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
        
        cli::cli_li(paste0("Storing results"))
        saveRDS(res, paste0("Rdata/test_AMF_",climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds"))
      }
      
    }
  }
}


# Reducció superfície de bosc (RSB) ---------------------------------------

if(RSB_2021_2100) {
  climate_models <- "mpiesm_rca4"
  climate_scens <- c("rcp45", "rcp85")
  for(climate_model in climate_models) {
    for(climate_scen in climate_scens) {
      cli::cli_h1(paste0("SIMULATION 2021-2100 / RSB / ", climate_model, " / ", climate_scen))
      
      cli::cli_li(paste0("Recovering end of historical run"))
      res <- readRDS("Rdata/test_2011_2020.rds")
      
      cli::cli_li(paste0("Re-assigning management units by dominant species and excluding plots from management"))
      next_sf <- res$next_sf
      next_sf$management_unit <- assign_management_unit(next_sf$dominant_tree_species, defaultPrescriptionsBySpecies)
      next_sf$management_unit[next_sf$LowQuality] <- NA
      next_sf$management_unit[next_sf$managment_RSB==0] <- NA
     
      # 2021 - 2030
      cli::cli_li(paste0("Applying land-use changes (2021-2030)"))
      sel_to_agri <- next_sf$prior_agri %in% 1:10
      sel_to_pasture <- next_sf$prior_pasture %in% 1:10
      next_sf$forest[sel_to_agri] <- list(NULL)
      next_sf$state[sel_to_agri] <- list(NULL)
      next_sf$management_unit[sel_to_agri] <- NA
      for(ip in  which(sel_to_pasture)) {
        f <- emptyforest()
        f$herbCover <- 80
        f$herbHeight <- 20
        next_sf$forest[[ip]] <- f
        next_sf$state[[ip]] <- forest2growthInput(f, next_sf$soil[[ip]], SpParamsMED, local_control)
        next_sf$management_unit[[ip]] <- NA
      }
      res$next_sf <- next_sf
      
      cli::cli_li(paste0("Loading interpolator for years ", 2021," to ", 2030))
      interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                          paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                 climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                 2021, "_", 2030,".nc"))
      interpolator <- meteoland::read_interpolator(interpolator_file)
      file.remove(interpolator_file)
      
      cli::cli_li(paste0("Defining management scenario (30% extraction rates)"))
      volumes_RSB_test <- volumes_2001_2020/(nrow(nfiplot)/nrow(res$next_sf))
      rates <- rep(30, 10) # 30% extraction rate
      names(rates) <- as.character(2021:2030)
      scen_RSB_test <- create_management_scenario(units = defaultPrescriptionsBySpecies,  
                                                  annual_demand_by_species = volumes_RSB_test,
                                                  extraction_rate_by_year = rates) 
      
      res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                             volume_function = NULL, local_control = local_control,
                             management_scenario = scen_RSB_test, summary_function = summary_scenario,
                             parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
      saveRDS(res, paste0("Rdata/test_RSB_",climate_model,"_",climate_scen,"_2021_2030.rds"))
      
      # 2031 - 2040
      cli::cli_li(paste0("Applying land-use changes (2031-2040)"))
      sel_to_agri <- next_sf$prior_agri %in% 11:20
      sel_to_pasture <- next_sf$prior_pasture %in% 11:20
      next_sf$forest[sel_to_agri] <- list(NULL)
      next_sf$state[sel_to_agri] <- list(NULL)
      next_sf$management_unit[sel_to_agri] <- NA
      for(ip in  which(sel_to_pasture)) {
        f <- emptyforest()
        f$herbCover <- 80
        f$herbHeight <- 20
        next_sf$forest[[ip]] <- f
        next_sf$state[[ip]] <- forest2growthInput(f, next_sf$soil[[ip]], SpParamsMED, local_control)
        next_sf$management_unit[[ip]] <- NA
      }
      res$next_sf <- next_sf
      
      cli::cli_li(paste0("Loading interpolator for years ", 2031," to ", 2040))
      interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                          paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                 climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                 2031, "_", 2040,".nc"))
      interpolator <- meteoland::read_interpolator(interpolator_file)
      file.remove(interpolator_file)
      
      cli::cli_li(paste0("Defining management scenario (30% extraction rates)"))
      volumes_RSB_test <- volumes_2001_2020/(nrow(nfiplot)/nrow(res$next_sf))
      rates <- rep(30, 10) # 30% extraction rate
      names(rates) <- as.character(2031:2040)
      scen_RSB_test <- create_management_scenario(units = defaultPrescriptionsBySpecies,  
                                                  annual_demand_by_species = volumes_RSB_test,
                                                  extraction_rate_by_year = rates) 
      
      res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                             volume_function = NULL, local_control = local_control,
                             management_scenario = scen_RSB_test, summary_function = summary_scenario,
                             parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
      saveRDS(res, paste0("Rdata/test_RSB_",climate_model,"_",climate_scen,"_2031_2040.rds"))
      
      # 2041 - 2100
      yearsIni <- seq(2041 , 2091, by=10)
      yearsFin <- seq(2050, 2100, by=10)
      for(iy in 1:length(yearsIni)) {
        cli::cli_li(paste0("Loading interpolator for years ", yearsIni[iy]," to ", yearsFin[iy]))
        interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                            paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                   climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                   yearsIni[iy], "_", yearsFin[iy],".nc"))
        interpolator <- meteoland::read_interpolator(interpolator_file)
        file.remove(interpolator_file)
        
        cli::cli_li(paste0("Defining management scenario (30% extraction rates)"))
        volumes_RSB_test <- volumes_2001_2020/(nrow(nfiplot)/nrow(res$next_sf))
        rates <- rep(30, 10) # 30% extraction rate
        names(rates) <- as.character(yearsIni[iy]:yearsFin[iy])
        scen_RSB_test <- create_management_scenario(units = defaultPrescriptionsBySpecies,  
                                                    annual_demand_by_species = volumes_RSB_test,
                                                    extraction_rate_by_year = rates) 
        
        res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                               volume_function = NULL, local_control = local_control,
                               management_scenario = scen_RSB_test, summary_function = summary_scenario,
                               parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
        
        cli::cli_li(paste0("Storing results"))
        saveRDS(res, paste0("Rdata/test_RSB_",climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds"))
      } 
    }
  }
}


# Augment de la superfície exclosa d'aprofitaments (ASEA) ------------------

if(ASEA_2021_2100) {
  climate_models <- "mpiesm_rca4"
  climate_scens <- c("rcp45", "rcp85")
  for(climate_model in climate_models) {
    for(climate_scen in climate_scens) {
      cli::cli_h1(paste0("SIMULATION 2021-2100 / ASEA / ", climate_model, " / ", climate_scen))
      
      cli::cli_li(paste0("Recovering end of historical run"))
      res <- readRDS("Rdata/test_2011_2020.rds")
     
      cli::cli_li(paste0("Re-assigning management units by dominant species and excluding plots from management"))
      next_sf <- res$next_sf
      next_sf$management_unit <- assign_management_unit(next_sf$dominant_tree_species, defaultPrescriptionsBySpecies)
      next_sf$management_unit[next_sf$LowQuality] <- NA
      next_sf$management_unit[next_sf$managment_ASEA==0] <- NA
      res$next_sf <- next_sf 
      
      yearsIni <- seq(2021 , 2091, by=10)
      yearsFin <- seq(2030, 2100, by=10)
      for(iy in 1:length(yearsIni)) {
        cli::cli_li(paste0("Loading interpolator for years ", yearsIni[iy]," to ", yearsFin[iy]))
        interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                            paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                   climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                   yearsIni[iy], "_", yearsFin[iy],".nc"))
        interpolator <- meteoland::read_interpolator(interpolator_file)
        file.remove(interpolator_file)
        
        cli::cli_li(paste0("Defining management scenario (30% extraction rates)"))
        volumes_ASEA_test <- volumes_2001_2020/(nrow(nfiplot)/nrow(res$next_sf))
        rates <- rep(30, 10) # 30% extraction rate
        names(rates) <- as.character(yearsIni[iy]:yearsFin[iy])
        scen_ASEA_test <- create_management_scenario(units = defaultPrescriptionsBySpecies,  
                                                     annual_demand_by_species = volumes_ASEA_test,
                                                     extraction_rate_by_year = rates) 
        
        res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                               volume_function = NULL, local_control = local_control,
                               management_scenario = volumes_ASEA_test, summary_function = summary_scenario,
                               parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
        
        cli::cli_li(paste0("Storing results"))
        saveRDS(res, paste0("Rdata/test_ASEA_",climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds"))
      } 
    }
  }
}


# Acompanyament de l'ecosistema per l'adaptació al canvi global (ACG) --------

if(ACG_2021_2100) {
  climate_models <- "mpiesm_rca4"
  climate_scens <- c("rcp45", "rcp85")
  for(climate_model in climate_models) {
    for(climate_scen in climate_scens) {
      cli::cli_h1(paste0("SIMULATION 2021-2100 / ACG / ", climate_model, " / ", climate_scen))
      
      cli::cli_li(paste0("Recovering end of historical run"))
      res <- readRDS("Rdata/test_2011_2020.rds")
      
      cli::cli_li(paste0("Re-assigning management units by dominant species and excluding plots from management"))
      next_sf <- res$next_sf
      next_sf$management_unit <- assign_management_unit(next_sf$dominant_tree_species, defaultPrescriptionsBySpecies)
      next_sf$management_unit[next_sf$managment_ACG==0] <- NA
      res$next_sf <- next_sf 
      
      yearsIni <- seq(2021 , 2091, by=10)
      yearsFin <- seq(2030, 2100, by=10)
      for(iy in 1:length(yearsIni)) {
        
        cli::cli_li(paste0("Loading interpolator for years ", yearsIni[iy]," to ", yearsFin[iy]))
        interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                            paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                   climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                   yearsIni[iy], "_", yearsFin[iy],".nc"))
        interpolator <- meteoland::read_interpolator(interpolator_file)
        file.remove(interpolator_file)
        
        cli::cli_li(paste0("Defining management scenario (no demand)"))
        scen_ACG_test <- create_management_scenario(units = defaultPrescriptionsBySpecies) 
        
        res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                               volume_function = NULL, local_control = local_control,
                               management_scenario = scen_ACG_test, summary_function = summary_scenario,
                               parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
        
        cli::cli_li(paste0("Storing results"))
        saveRDS(res, paste0("Rdata/test_ACG_",climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds"))
      }
    }
  }
}
