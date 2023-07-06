library(medfateland)
library(medfate)
library(IFNallometry)


# Parameters --------------------------------------------------------------

climate_base = "emf/datasets/Climate/"

ntest <- 100
chunk_size <- 5
num_cores <- 20

provinces <- 1
overwrite = FALSE
mergeTreesBetweenDecades = TRUE
subset_initial = FALSE
common_2001_2010 = FALSE
common_2011_2020 = FALSE
BAU_2021_2100 = TRUE
AMF_2021_2100 = TRUE
RSB_2021_2100 = TRUE
ASEA_2021_2100 = TRUE
ACG_2021_2100 = TRUE
NOG_2021_2100 = TRUE

local_control <- defaultControl()
local_control$fireHazardResults <- TRUE

# Preliminaries -----------------------------------------------------------
cli::cli_h1(paste0("PRELIMINARIES"))


cli::cli_li(paste0("Loading plot data and splitting into provinces"))
nfiplot <- readRDS("Rdata/nfiplot.rds")
nfiplot$represented_area <- nfiplot$area
nfiplot_provs<-split(nfiplot, nfiplot$Provincia)

cli::cli_li(paste0("Reference wood demand"))
aprofit_decade_prov_spp <- readxl::read_excel("Data/aprofit_decade_prov_spp.xlsx", 
                                      sheet = "aprofit_decade_prov_medfate")

cli::cli_li(paste0("Prescriptions"))
default_prescriptions <- readxl::read_excel("Data/prescriptions_by_spp.xlsx", 
                                              sheet = "default")

adaptation_prescriptions <- readxl::read_excel("Data/prescriptions_by_spp.xlsx", 
                                            sheet = "adaption")

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

source("Rscripts/A2_utils.R")

cli::cli_li(paste0("Defining summary function"))
summary_scenario <- function(object, SpParams, ...) {
  summary_wb <- medfate::summary.fordyn(object, 
                                         output = "WaterBalance", 
                                         freq = "years", FUN = sum, na.rm=TRUE) # fordyn summar
  summary_cb <- medfate::summary.fordyn(object, 
                                         output = "CarbonBalance", 
                                         freq = "years", FUN = sum, na.rm=TRUE) # fordyn summar
  summary_fire <- medfate::summary.fordyn(object, 
                                          output = "FireHazard", 
                                          freq = "years", 
                                          FUN = max, na.rm = TRUE)
  summary_stand_min <- medfate::summary.fordyn(object, 
                                               output="Stand", 
                                               freq="years", 
                                               FUN = min, months=6:9, na.rm=TRUE)
  colnames(summary_stand_min)[4] <- "LAI_min"
  summary_stand_max <- medfate::summary.fordyn(object, 
                                               output="Stand", 
                                               freq="years", 
                                               FUN = max, months=6:9, na.rm=TRUE)
  colnames(summary_stand_max)[4] <- "LAI_max"
  f_sum <- t(sapply(object$ForestStructures, function(x) {unlist(summary(x, SpParams))}))
  summary_meteo <- t(sapply(object$GrowthResults, function(x) {
    c(Pdaymax = max(x$weather$Precipitation, na.rm = TRUE), 
      MAT = mean(x$weather$MeanTemperature, na.rm=TRUE))
    }))
  return(cbind(summary_wb,
               summary_cb,
               f_sum[-1, , drop = FALSE], 
               summary_fire[,c(13,14), drop = FALSE],
               summary_stand_min[,c(4), drop = FALSE],
               summary_stand_max[,c(4), drop = FALSE],
               summary_meteo))
}



if(subset_initial) {
  cli::cli_li(paste0("Subsetting ", ntest, " random test plots per province"))
  nfiplot_prov_test <- nfiplot_provs
  for(i in provinces) {
    df_i <- nfiplot_prov_test[[i]]
    df_i <- df_i[sample(nrow(df_i), ntest), ]
    nfiplot_prov_test[[i]] <- df_i
  }
  cli::cli_li(paste0("Storing initial state"))
  saveRDS(nfiplot_prov_test, "Rdata/test_initial.rds")
}


# Historical 2001-2010 --------------------------------------------------------

if(common_2001_2010) {
  cli::cli_h1(paste0("SIMULATION 2001-2010 (all scenarios)"))
  nfiplot_prov_test <- readRDS("Rdata/test_initial.rds")
  
  years <- 2001:2010
  cli::cli_li(paste0("Loading interpolators for years ", years[1]," to ", years[length(years)]))
  interpolators <- vector("list", length(years))
  for(iy in 1:length(years)) {
    interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                        paste0("Products/InterpolationData/Catalunya/Historic/calibrated_2.0/interpolator_", years[iy],"_calibrated.nc"))
    interpolators[[iy]] <- meteoland::read_interpolator(interpolator_file)
    file.remove(interpolator_file)
  }
  
  for(iprov in provinces) {
    cli::cli_h2(paste0("PROVINCE: ", provinceStrings[iprov]))
    
    nfiplot_test <- nfiplot_prov_test[[iprov]]
    cli::cli_li(paste0("Assigning management units by dominant species and excluding plots from management"))
    nfiplot_test$management_unit <- assign_management_unit(nfiplot_test$dominant_tree_species, default_prescriptions)
    nfiplot_test$management_unit[(nfiplot_test$LowQuality) | (nfiplot_test$management_BAU==0)] <- NA

    cli::cli_li(paste0("Defining management scenario (historical demand)"))
    volumes <- aprofit_decade_prov_spp |>
      dplyr::filter(Province == provinceStrings[iprov],
                    Decade == "2001-2010")
    volumes_2001_2010 <- volumes$Volume
    names(volumes_2001_2010) <- volumes$Species
    volumes_2001_2010_test <- volumes_2001_2010/(nrow(nfiplot_provs[[iprov]])/nrow(nfiplot_test))
    scen_2001_2010_test <- create_management_scenario(default_prescriptions,  
                                                      volumes_2001_2010_test) 
    
    res_01_10 <- fordyn_scenario(nfiplot_test, SpParamsMED, meteo = interpolators,
                                 volume_function = volume_scenario, volume_arguments = list(province = provinces[iprov]),
                                 local_control = local_control,
                                 management_scenario = scen_2001_2010_test, 
                                 summary_function = summary_scenario, summary_arguments = list(SpParams = SpParamsMED),
                                 parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
    
    cli::cli_li(paste0("Storing results"))
    saveRDS(res_01_10, paste0("Rdata/historic/test_", provinceStrings[iprov], "_2001_2010.rds"))
  }
}


# Historical 2011-2020 --------------------------------------------------------

if(common_2011_2020) {
  cli::cli_h1(paste0("SIMULATION 2011-2020 (all scenarios)"))
  
  years <- 2011:2020
  cli::cli_li(paste0("Loading interpolators for years ", years[1]," to ", years[length(years)]))
  interpolators <- vector("list", length(years))
  for(iy in 1:length(years)) {
    interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                        paste0("Products/InterpolationData/Catalunya/Historic/calibrated_2.0/interpolator_", years[iy],"_calibrated.nc"))
    interpolators[[iy]] <- meteoland::read_interpolator(interpolator_file)
    file.remove(interpolator_file)
  }
  
  
  for(iprov in provinces) {
    cli::cli_h2(paste0("PROVINCE: ", provinceStrings[iprov]))
    
    cli::cli_li(paste0("Recovering previous run"))
    res_01_10 <- readRDS(paste0("Rdata/historic/test_", provinceStrings[iprov],"_2001_2010.rds"))
    
    cli::cli_li(paste0("Defining management scenario (historical demand)"))
    volumes <- aprofit_decade_prov_spp |>
      dplyr::filter(Province == provinceStrings[iprov],
                    Decade == "2011-2020")
    volumes_2011_2020 <- volumes$Volume
    names(volumes_2011_2020) <- volumes$Species
    volumes_2011_2020_test <- volumes_2011_2020/(nrow(nfiplot_provs[[iprov]])/nrow(res_01_10$next_sf))
    scen_2011_2020_test <- create_management_scenario(default_prescriptions,  
                                                      volumes_2011_2020_test) 
    
    
    res_11_20 <- fordyn_scenario(res_01_10, SpParamsMED, meteo = interpolators,
                                 volume_function = volume_scenario,  volume_arguments = list(province = provinces[iprov]),
                                 local_control = local_control,
                                 management_scenario = scen_2011_2020_test, 
                                 summary_function = summary_scenario, summary_arguments = list(SpParams = SpParamsMED),
                                 parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
    
    cli::cli_li(paste0("Storing results"))
    saveRDS(res_11_20, paste0("Rdata/historic/test_", provinceStrings[iprov],"_2011_2020.rds"))
  }
}


# Business as usual (BAU) -------------------------------------------------

if(BAU_2021_2100) {
  climate_models <- "mpiesm_rca4"
  climate_scens <- c("rcp45", "rcp85")
  for(climate_model in climate_models) {
    for(climate_scen in climate_scens) {
      cli::cli_h1(paste0("SIMULATION 2021-2100 / BAU / ", climate_model, " / ", climate_scen))
      
      for(iprov in provinces) {
        cli::cli_h2(paste0("PROVINCE: ", provinceStrings[iprov]))
        
        cli::cli_li(paste0("Recovering end of historical run"))
        res <- readRDS(paste0("Rdata/historic/test_", provinceStrings[iprov], "_2011_2020.rds"))
        
        cli::cli_li(paste0("Re-assigning management units by dominant species and excluding plots from management"))
        next_sf <- res$next_sf
        next_sf$management_unit <- assign_management_unit(next_sf$dominant_tree_species, default_prescriptions)
        next_sf$management_unit[(next_sf$LowQuality) | (next_sf$management_BAU==0)] <- NA
        next_sf$management_arguments[(next_sf$LowQuality) | (next_sf$management_BAU==0)] <- list(NULL)
        res$next_sf <- next_sf
        
        yearsIni <- seq(2021 , 2091, by=10)
        yearsFin <- seq(2030, 2100, by=10)
        for(iy in 1:length(yearsIni)) {
          res_file <- paste0("Rdata/BAU/test_BAU_", provinceStrings[iprov],"_", climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds")
          if(!file.exists(res_file) || overwrite) {
            cli::cli_li(paste0("Loading interpolator for years ", yearsIni[iy]," to ", yearsFin[iy]))
            interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                                paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                       climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                       yearsIni[iy], "_", yearsFin[iy],".nc"))
            interpolator <- meteoland::read_interpolator(interpolator_file)
            file.remove(interpolator_file)
            
            if(mergeTreesBetweenDecades) {
              cli::cli_li(paste0("Merging tree cohorts"))
              next_sf <- res$next_sf
              for(i in 1:nrow(next_sf)) {
                next_sf$forest[[i]] <- medfate::forest_mergeTrees(next_sf$forest[[i]])
                next_sf$state[i] <- list(NULL)
              }
              res$next_sf <- next_sf
            }
            
            cli::cli_li(paste0("Defining management scenario (30% extraction rates)"))
            volumes <- aprofit_decade_prov_spp[,1:4] |>
              dplyr::filter(Province == provinceStrings[iprov]) |>
              dplyr::group_by(Species) |>
              dplyr::summarise(Volume = mean(Volume, na.rm=TRUE), .groups = "drop") 
            volumes_2001_2020 <- volumes$Volume
            names(volumes_2001_2020) <- volumes$Species
            
            volumes_BAU_test <- volumes_2001_2020/(nrow(nfiplot_provs[[iprov]])/nrow(res$next_sf))
            rates <- rep(30, 10) # 30% extraction rate
            names(rates) <- as.character(yearsIni[iy]:yearsFin[iy])
            scen_BAU_test <- create_management_scenario(units = default_prescriptions,  
                                                        annual_demand_by_species = volumes_BAU_test,
                                                        extraction_rate_by_year = rates) 
            
            res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                                   volume_function = volume_scenario, volume_arguments = list(province = provinces[iprov]),
                                   local_control = local_control,
                                   management_scenario = scen_BAU_test, 
                                   summary_function = summary_scenario, summary_arguments = list(SpParams = SpParamsMED),
                                   parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
            
            cli::cli_li(paste0("Storing results"))
            saveRDS(res, file = res_file)    
          } else {
            res <- readRDS(res_file)
          }
        }
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
      
      for(iprov in provinces) {
        cli::cli_h2(paste0("PROVINCE: ", provinceStrings[iprov]))
        
        res_file <- paste0("Rdata/AMF/test_AMF_",provinceStrings[iprov], "_", climate_model,"_",climate_scen,"_2021_2030.rds")
        if(!file.exists(res_file) || overwrite) {
          cli::cli_li(paste0("Recovering end of historical run"))
          res <- readRDS(paste0("Rdata/historic/test_", provinceStrings[iprov], "_2011_2020.rds"))
          
          cli::cli_li(paste0("Re-assigning management units by dominant species and excluding plots from management"))
          next_sf <- res$next_sf
          next_sf$management_unit <- assign_management_unit(next_sf$dominant_tree_species, default_prescriptions)
          next_sf$management_unit[(next_sf$LowQuality) | (next_sf$managment_AMF==0)] <- NA
          next_sf$management_arguments[(next_sf$LowQuality) | (next_sf$managment_AMF==0)] <- list(NULL)
          res$next_sf <- next_sf
          
          # 2021-2030
          
          cli::cli_li(paste0("Loading interpolator for years ", 2021," to ", 2030))
          interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                              paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                     climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                     2021, "_", 2030,".nc"))
          interpolator <- meteoland::read_interpolator(interpolator_file)
          file.remove(interpolator_file)
          
          if(mergeTreesBetweenDecades) {
            cli::cli_li(paste0("Merging tree cohorts"))
            next_sf <- res$next_sf
            for(i in 1:nrow(next_sf)) {
              next_sf$forest[[i]] <- medfate::forest_mergeTrees(next_sf$forest[[i]])
              next_sf$state[i] <- list(NULL)
            }
            res$next_sf <- next_sf
          }
          
          
          cli::cli_li(paste0("Defining management scenario (40% extraction rates)"))
          volumes <- aprofit_decade_prov_spp[,1:4] |>
            dplyr::filter(Province == provinceStrings[iprov]) |>
            dplyr::group_by(Species) |>
            dplyr::summarise(Volume = mean(Volume, na.rm=TRUE), .groups = "drop") 
          volumes_2001_2020 <- volumes$Volume
          names(volumes_2001_2020) <- volumes$Species
          
          volumes_AMF_test <- volumes_2001_2020/(nrow(nfiplot)/nrow(res$next_sf))
          rates <- rep(40, 10) # 40% extraction rate
          names(rates) <- as.character(2021:2030)
          scen_AMF_test <- create_management_scenario(units = default_prescriptions,  
                                                      annual_demand_by_species = volumes_AMF_test,
                                                      extraction_rate_by_year = rates) 
          
          res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                                 volume_function = volume_scenario, volume_arguments = list(province = provinces[iprov]),
                                 local_control = local_control,
                                 management_scenario = scen_AMF_test, 
                                 summary_function = summary_scenario, summary_arguments = list(SpParams = SpParamsMED),
                                 parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
          saveRDS(res, res_file)
        } else {
          res <- readRDS(res_file)
        }
        
        # 2031-2100
        yearsIni <- seq(2031 , 2091, by=10)
        yearsFin <- seq(2040, 2100, by=10)
        for(iy in 1:length(yearsIni)) {
          res_file <- paste0("Rdata/AMF/test_AMF_", provinceStrings[iprov], "_",climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds")
          
          if(!file.exists(res_file) || overwrite) {
            cli::cli_li(paste0("Loading interpolator for years ", yearsIni[iy]," to ", yearsFin[iy]))
            interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                                paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                       climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                       yearsIni[iy], "_", yearsFin[iy],".nc"))
            interpolator <- meteoland::read_interpolator(interpolator_file)
            file.remove(interpolator_file)
            
            if(mergeTreesBetweenDecades) {
              cli::cli_li(paste0("Merging tree cohorts"))
              next_sf <- res$next_sf
              for(i in 1:nrow(next_sf)) {
                next_sf$forest[[i]] <- medfate::forest_mergeTrees(next_sf$forest[[i]])
                next_sf$state[i] <- list(NULL)
              }
              res$next_sf <- next_sf
            }
            
            
            cli::cli_li(paste0("Defining management scenario (70% extraction rates)"))
            volumes <- aprofit_decade_prov_spp[,1:4] |>
              dplyr::filter(Province == provinceStrings[iprov]) |>
              dplyr::group_by(Species) |>
              dplyr::summarise(Volume = mean(Volume, na.rm=TRUE), .groups = "drop") 
            volumes_2001_2020 <- volumes$Volume
            names(volumes_2001_2020) <- volumes$Species
            
            volumes_AMF_test <- volumes_2001_2020/(nrow(nfiplot)/nrow(res$next_sf))
            rates <- rep(70, 10) # 70% extraction rate
            names(rates) <- as.character(yearsIni[iy]:yearsFin[iy])
            scen_AMF_test <- create_management_scenario(units = default_prescriptions,  
                                                        annual_demand_by_species = volumes_AMF_test,
                                                        extraction_rate_by_year = rates) 
            
            res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                                   volume_function = volume_scenario, volume_arguments = list(province = provinces[iprov]),
                                   local_control = local_control,
                                   management_scenario = scen_AMF_test, 
                                   summary_function = summary_scenario, summary_arguments = list(SpParams = SpParamsMED),
                                   parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
            
            cli::cli_li(paste0("Storing results"))
            saveRDS(res, res_file)
          } else {
            res <- readRDS(res_file)
          }
        }
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
      
      for(iprov in provinces) {
        cli::cli_h2(paste0("PROVINCE: ", provinceStrings[iprov]))
        
        cli::cli_li(paste0("Recovering end of historical run"))
        res <- readRDS(paste0("Rdata/historic/test_", provinceStrings[iprov], "_2011_2020.rds"))
        
        cli::cli_li(paste0("Re-assigning management units by dominant species and excluding plots from management"))
        next_sf <- res$next_sf
        next_sf$management_unit <- assign_management_unit(next_sf$dominant_tree_species, default_prescriptions)
        next_sf$management_unit[(next_sf$LowQuality) | (next_sf$managment_RSB==0)] <- NA
        next_sf$management_arguments[(next_sf$LowQuality) | (next_sf$managment_RSB==0)] <- list(NULL)
        
        # 2021 - 2030
        res_file <- paste0("Rdata/RSB/test_RSB_",provinceStrings[iprov], "_",climate_model,"_",climate_scen,"_2021_2030.rds")
        if(!file.exists(res_file) || overwrite) {
          cli::cli_li(paste0("Applying land-use changes (2021-2030)"))
          sel_to_agri <- next_sf$prior_agri %in% 1:10
          sel_to_pasture <- next_sf$prior_pasture %in% 1:10
          sel_to_remove <- sel_to_agri | sel_to_pasture
          i_herb <- which(sel_to_remove)
          ef<- emptyforest()
          ef$herbCover <- 100
          ef$herbHeight <- 5
          for(i in i_herb) {
            next_sf$forest[[i]] <- ef
            next_sf$state[[i]] <- forest2growthInput(ef, next_sf$soil[[i]], SpParamsMED, local_control)
            next_sf$management_unit[i] <- NA
            next_sf$management_arguments[i] <- list(NULL)
          }
          res$next_sf <- next_sf
          
          cli::cli_li(paste0("Loading interpolator for years 2021 to 2030"))
          interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                              paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                     climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                     2021, "_", 2030,".nc"))
          interpolator <- meteoland::read_interpolator(interpolator_file)
          file.remove(interpolator_file)
          
          if(mergeTreesBetweenDecades) {
            cli::cli_li(paste0("Merging tree cohorts"))
            next_sf <- res$next_sf
            for(i in 1:nrow(next_sf)) {
              next_sf$forest[[i]] <- medfate::forest_mergeTrees(next_sf$forest[[i]])
              next_sf$state[i] <- list(NULL)
            }
            res$next_sf <- next_sf
          }
          
          
          cli::cli_li(paste0("Defining management scenario (30% extraction rates)"))
          volumes <- aprofit_decade_prov_spp[,1:4] |>
            dplyr::filter(Province == provinceStrings[iprov]) |>
            dplyr::group_by(Species) |>
            dplyr::summarise(Volume = mean(Volume, na.rm=TRUE), .groups = "drop") 
          volumes_2001_2020 <- volumes$Volume
          names(volumes_2001_2020) <- volumes$Species
          volumes_RSB_test <- volumes_2001_2020/(nrow(nfiplot)/nrow(res$next_sf))
          rates <- rep(30, 10) # 30% extraction rate
          names(rates) <- as.character(2021:2030)
          scen_RSB_test <- create_management_scenario(units = default_prescriptions,  
                                                      annual_demand_by_species = volumes_RSB_test,
                                                      extraction_rate_by_year = rates) 
          
          res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                                 volume_function = volume_scenario, volume_arguments = list(province = provinces[iprov]),
                                 local_control = local_control,
                                 management_scenario = scen_RSB_test, 
                                 summary_function = summary_scenario, summary_arguments = list(SpParams = SpParamsMED),
                                 parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
          saveRDS(res, res_file)
        } else {
          res <- readRDS(res_file)
        }
        
        # 2031 - 2040
        res_file <- paste0("Rdata/RSB/test_RSB_",provinceStrings[iprov], "_",climate_model,"_",climate_scen,"_2031_2040.rds")
        if(!file.exists(res_file) || overwrite) {
          cli::cli_li(paste0("Applying land-use changes (2031-2040)"))
          sel_to_agri <- next_sf$prior_agri %in% 11:20
          sel_to_pasture <- next_sf$prior_pasture %in% 11:20
          sel_to_remove <- sel_to_agri | sel_to_pasture
          i_herb <- which(sel_to_remove)
          ef<- emptyforest()
          ef$herbCover <- 100
          ef$herbHeight <- 10
          for(i in i_herb) {
            next_sf$forest[[i]] <- ef
            next_sf$state[[i]] <- forest2growthInput(ef, next_sf$soil[[i]], SpParamsMED, local_control)
            next_sf$management_unit[i] <- NA
            next_sf$management_arguments[i] <- list(NULL)
          }
          res$next_sf <- next_sf
          
          cli::cli_li(paste0("Loading interpolator for years 2031 to 2040"))
          interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                              paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                     climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                     2031, "_", 2040,".nc"))
          interpolator <- meteoland::read_interpolator(interpolator_file)
          file.remove(interpolator_file)
          
          if(mergeTreesBetweenDecades) {
            cli::cli_li(paste0("Merging tree cohorts"))
            next_sf <- res$next_sf
            for(i in 1:nrow(next_sf)) {
              next_sf$forest[[i]] <- medfate::forest_mergeTrees(next_sf$forest[[i]])
              next_sf$state[i] <- list(NULL)
            }
            res$next_sf <- next_sf
          }
          
          cli::cli_li(paste0("Defining management scenario (30% extraction rates)"))
          volumes <- aprofit_decade_prov_spp[,1:4] |>
            dplyr::filter(Province == provinceStrings[iprov]) |>
            dplyr::group_by(Species) |>
            dplyr::summarise(Volume = mean(Volume, na.rm=TRUE), .groups = "drop") 
          volumes_2001_2020 <- volumes$Volume
          names(volumes_2001_2020) <- volumes$Species
          volumes_RSB_test <- volumes_2001_2020/(nrow(nfiplot)/nrow(res$next_sf))
          rates <- rep(30, 10) # 30% extraction rate
          names(rates) <- as.character(2031:2040)
          scen_RSB_test <- create_management_scenario(units = default_prescriptions,  
                                                      annual_demand_by_species = volumes_RSB_test,
                                                      extraction_rate_by_year = rates) 
          
          res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                                 volume_function = volume_scenario, volume_arguments = list(province = provinces[iprov]),
                                 local_control = local_control,
                                 management_scenario = scen_RSB_test, 
                                 summary_function = summary_scenario, summary_arguments = list(SpParams = SpParamsMED),
                                 parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
          saveRDS(res, res_file)
        } else {
          res <- readRDS(res_file)
        }
        
        # 2041 - 2100
        yearsIni <- seq(2041 , 2091, by=10)
        yearsFin <- seq(2050, 2100, by=10)
        for(iy in 1:length(yearsIni)) {
          res_file <- paste0("Rdata/RSB/test_RSB_",provinceStrings[iprov], "_",climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds")

          if(!file.exists(res_file) || overwrite) {
            cli::cli_li(paste0("Loading interpolator for years ", yearsIni[iy]," to ", yearsFin[iy]))
            interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                                paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                       climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                       yearsIni[iy], "_", yearsFin[iy],".nc"))
            interpolator <- meteoland::read_interpolator(interpolator_file)
            file.remove(interpolator_file)
            
            if(mergeTreesBetweenDecades) {
              cli::cli_li(paste0("Merging tree cohorts"))
              next_sf <- res$next_sf
              for(i in 1:nrow(next_sf)) {
                next_sf$forest[[i]] <- medfate::forest_mergeTrees(next_sf$forest[[i]])
                next_sf$state[i] <- list(NULL)
              }
              res$next_sf <- next_sf
            }
            
            cli::cli_li(paste0("Defining management scenario (30% extraction rates)"))
            volumes <- aprofit_decade_prov_spp[,1:4] |>
              dplyr::filter(Province == provinceStrings[iprov]) |>
              dplyr::group_by(Species) |>
              dplyr::summarise(Volume = mean(Volume, na.rm=TRUE), .groups = "drop") 
            volumes_2001_2020 <- volumes$Volume
            names(volumes_2001_2020) <- volumes$Species
            volumes_RSB_test <- volumes_2001_2020/(nrow(nfiplot)/nrow(res$next_sf))
            rates <- rep(30, 10) # 30% extraction rate
            names(rates) <- as.character(yearsIni[iy]:yearsFin[iy])
            scen_RSB_test <- create_management_scenario(units = default_prescriptions,  
                                                        annual_demand_by_species = volumes_RSB_test,
                                                        extraction_rate_by_year = rates) 
            
            res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                                   volume_function = volume_scenario, volume_arguments = list(province = provinces[iprov]),
                                   local_control = local_control,
                                   management_scenario = scen_RSB_test, 
                                   summary_function = summary_scenario, summary_arguments = list(SpParams = SpParamsMED),
                                   parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
            
            cli::cli_li(paste0("Storing results"))
            saveRDS(res, res_file)
          } else {
            res <- readRDS(res_file)
          }          
        } 
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
      
      
      for(iprov in provinces) {
        cli::cli_h2(paste0("PROVINCE: ", provinceStrings[iprov]))
        
        cli::cli_li(paste0("Recovering end of historical run"))
        res <- readRDS(paste0("Rdata/historic/test_", provinceStrings[iprov], "_2011_2020.rds"))
        
        cli::cli_li(paste0("Re-assigning management units by dominant species and excluding plots from management"))
        next_sf <- res$next_sf
        next_sf$management_unit <- assign_management_unit(next_sf$dominant_tree_species, default_prescriptions)
        next_sf$management_unit[(next_sf$LowQuality) | (next_sf$managment_ASEA==0)] <- NA
        next_sf$management_arguments[(next_sf$LowQuality) | (next_sf$managment_ASEA==0)] <- list(NULL)
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
          
          if(mergeTreesBetweenDecades) {
            cli::cli_li(paste0("Merging tree cohorts"))
            next_sf <- res$next_sf
            for(i in 1:nrow(next_sf)) {
              next_sf$forest[[i]] <- medfate::forest_mergeTrees(next_sf$forest[[i]])
              next_sf$state[i] <- list(NULL)
            }
            res$next_sf <- next_sf
          }
          
          cli::cli_li(paste0("Defining management scenario (30% extraction rates)"))
          volumes <- aprofit_decade_prov_spp[,1:4] |>
            dplyr::filter(Province == provinceStrings[iprov]) |>
            dplyr::group_by(Species) |>
            dplyr::summarise(Volume = mean(Volume, na.rm=TRUE), .groups = "drop") 
          volumes_2001_2020 <- volumes$Volume
          names(volumes_2001_2020) <- volumes$Species
          volumes_ASEA_test <- volumes_2001_2020/(nrow(nfiplot)/nrow(res$next_sf))
          rates <- rep(30, 10) # 30% extraction rate
          names(rates) <- as.character(yearsIni[iy]:yearsFin[iy])
          scen_ASEA_test <- create_management_scenario(units = default_prescriptions,  
                                                       annual_demand_by_species = volumes_ASEA_test,
                                                       extraction_rate_by_year = rates) 
          
          res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                                 volume_function = volume_scenario, volume_arguments = list(province = provinces[iprov]),
                                 local_control = local_control,
                                 management_scenario = scen_ASEA_test, 
                                 summary_function = summary_scenario, summary_arguments = list(SpParams = SpParamsMED),
                                 parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
          
          cli::cli_li(paste0("Storing results"))
          saveRDS(res, paste0("Rdata/ASEA/test_ASEA_",provinceStrings[iprov], "_",climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds"))
        } 
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
      
      for(iprov in provinces) {
        cli::cli_h2(paste0("PROVINCE: ", provinceStrings[iprov]))
        
        cli::cli_li(paste0("Recovering end of historical run"))
        res <- readRDS(paste0("Rdata/historic/test_", provinceStrings[iprov], "_2011_2020.rds"))
        
        cli::cli_li(paste0("Re-assigning management units by dominant species and excluding plots from management"))
        next_sf <- res$next_sf
        next_sf$management_unit <- assign_management_unit(next_sf$dominant_tree_species, adaptation_prescriptions)
        next_sf$management_unit[next_sf$managment_ACG==0] <- NA
        next_sf$management_arguments[next_sf$managment_ACG==0] <- list(NULL)
        res$next_sf <- next_sf 
        
        yearsIni <- seq(2021 , 2091, by=10)
        yearsFin <- seq(2030, 2100, by=10)
        for(iy in 1:length(yearsIni)) {
          
          res_file <- paste0("Rdata/ACG/test_ACG_", provinceStrings[iprov],"_",climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds")
          if(!file.exists(res_file) || overwrite) {
            cli::cli_li(paste0("Loading interpolator for years ", yearsIni[iy]," to ", yearsFin[iy]))
            interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                                paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                       climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                       yearsIni[iy], "_", yearsFin[iy],".nc"))
            interpolator <- meteoland::read_interpolator(interpolator_file)
            file.remove(interpolator_file)
            
            if(mergeTreesBetweenDecades) {
              cli::cli_li(paste0("Merging tree cohorts"))
              next_sf <- res$next_sf
              for(i in 1:nrow(next_sf)) {
                next_sf$forest[[i]] <- medfate::forest_mergeTrees(next_sf$forest[[i]])
                next_sf$state[i] <- list(NULL)
              }
              res$next_sf <- next_sf
            }
            
            cli::cli_li(paste0("Defining management scenario (no demand and adaptation prescriptions)"))
            scen_ACG_test <- create_management_scenario(units = adaptation_prescriptions) 
            
            res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                                   volume_function = volume_scenario, volume_arguments = list(province = provinces[iprov]),
                                   local_control = local_control,
                                   management_scenario = scen_ACG_test, 
                                   summary_function = summary_scenario, summary_arguments = list(SpParams = SpParamsMED),
                                   parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
            
            cli::cli_li(paste0("Storing results"))
            saveRDS(res, res_file)
          } else {
            res <- readRDS(res_file)
          }
        }        
      }
    }
  }
}


# No gestio (NOG) ------------------------------------------------------------

if(NOG_2021_2100) {
  climate_models <- "mpiesm_rca4"
  climate_scens <- c("rcp45", "rcp85")
  for(climate_model in climate_models) {
    for(climate_scen in climate_scens) {
      cli::cli_h1(paste0("SIMULATION 2021-2100 / NOG / ", climate_model, " / ", climate_scen))
      
      for(iprov in provinces) {
        cli::cli_h2(paste0("PROVINCE: ", provinceStrings[iprov]))
        
        cli::cli_li(paste0("Recovering end of historical run"))
        res <- readRDS(paste0("Rdata/historic/test_", provinceStrings[iprov], "_2011_2020.rds"))
        
        cli::cli_li(paste0("Re-assigning management units to missing"))
        next_sf <- res$next_sf
        next_sf$management_unit <- NA
        next_sf$management_arguments <- list(NULL)
        res$next_sf <- next_sf
        
        yearsIni <- seq(2021 , 2091, by=10)
        yearsFin <- seq(2030, 2100, by=10)
        for(iy in 1:length(yearsIni)) {
          
          res_file <- paste0("Rdata/NOG/test_NOG_", provinceStrings[iprov],"_", climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds")
          if(!file.exists(res_file) || overwrite) {
            cli::cli_li(paste0("Loading interpolator for years ", yearsIni[iy]," to ", yearsFin[iy]))
            interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                                paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                       climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                       yearsIni[iy], "_", yearsFin[iy],".nc"))
            interpolator <- meteoland::read_interpolator(interpolator_file)
            file.remove(interpolator_file)
            
            if(mergeTreesBetweenDecades) {
              cli::cli_li(paste0("Merging tree cohorts"))
              next_sf <- res$next_sf
              for(i in 1:nrow(next_sf)) {
                next_sf$forest[[i]] <- medfate::forest_mergeTrees(next_sf$forest[[i]])
                next_sf$state[i] <- list(NULL)
              }
              res$next_sf <- next_sf
            }
            
            cli::cli_li(paste0("Defining management scenario (no management)"))
            scen_NOG_test <- create_management_scenario(units = 1) 
            
            res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                                   volume_function = volume_scenario, volume_arguments = list(province = provinces[iprov]),
                                   local_control = local_control,
                                   management_scenario = scen_NOG_test, 
                                   summary_function = summary_scenario, summary_arguments = list(SpParams = SpParamsMED),
                                   parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
            
            cli::cli_li(paste0("Storing results"))
            saveRDS(res, res_file)
          } else {
            res <- readRDS(res_file)
          }
        }
      }
    }
  }
}


