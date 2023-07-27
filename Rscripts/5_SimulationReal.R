library(medfateland)
library(medfate)
library(IFNallometry)


# Parameters --------------------------------------------------------------

climate_base = "emf/datasets/Climate/"

chunk_size <- 20
num_cores <- 20

progress = TRUE
iprovinces <- 1:4
overwrite = FALSE
historic_2001_2010 = FALSE
historic_2011_2020 = FALSE
BAU_2021_2100 = TRUE
AMF_2021_2100 = TRUE
RSB_2021_2100 = TRUE 
ASEA_2021_2100 = TRUE
ACG_2021_2100 = FALSE
NOG_2021_2100 = FALSE

local_control <- defaultControl()
local_control$fireHazardResults <- TRUE
local_control$fireHazardStandardWind <- 11
local_control$dynamicallyMergeCohorts <- TRUE

# Preliminaries -----------------------------------------------------------
cli::cli_h1(paste0("PRELIMINARIES"))


cli::cli_li(paste0("Loading plot data and splitting into provinces"))
nfiplot <- readRDS("Rdata/nfiplot.rds")
nfiplot$represented_area <- nfiplot$area
nfiplot_provs<-split(nfiplot, nfiplot$Provincia)

cli::cli_li(paste0("Atmospheric CO2 data"))
CO2_ppm <- readxl::read_excel("Data/CO2_escenarios.xlsx", 
                                              sheet = "CO2_ppm")

cli::cli_li(paste0("Reference wood demand"))
aprofit_decade_prov_spp <- readxl::read_excel("Data/aprofit_decade_prov_spp.xlsx", 
                                      sheet = "aprofit_decade_prov_medfate")

cli::cli_li(paste0("Prescriptions"))
default_prescriptions <- readxl::read_excel("Data/prescriptions_by_spp.xlsx", 
                                              sheet = "default")

adaptation_prescriptions <- readxl::read_excel("Data/prescriptions_by_spp.xlsx", 
                                            sheet = "adaption")


source("Rscripts/A2_utils.R")



# Historical 2001-2010 --------------------------------------------------------

if(historic_2001_2010) {
  cli::cli_h1(paste0("SIMULATION 2001-2010 (all scenarios)"))

  years <- 2001:2010
  
  cli::cli_li(paste0("CO2 levels for years 2001 to 2010"))
  CO2ByYear <- CO2_ppm |> 
    dplyr::filter(Year %in% years) 
  CO2ByYear <- CO2ByYear$RCP45
  names(CO2ByYear) <- years
  cli::cli_li(paste0("Loading interpolators for years ", years[1]," to ", years[length(years)]))
  interpolators <- vector("list", length(years))
  for(iy in 1:length(years)) {
    interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                        paste0("Products/InterpolationData/Catalunya/Historic/calibrated_2.0/interpolator_", years[iy],"_calibrated.nc"))
    interpolators[[iy]] <- load_interpolator(interpolator_file, years[iy])
  }
  
  for(iprov in iprovinces) {
    cli::cli_h2(paste0("PROVINCE: ", provinceStrings[iprov]))
    
    nfiplot_prov <- nfiplot_provs[[iprov]]
    cli::cli_li(paste0("Assigning management units by dominant species and excluding plots from management"))
    nfiplot_prov$management_unit <- assign_management_unit(nfiplot_prov$dominant_tree_species, default_prescriptions)
    nfiplot_prov$management_unit[(nfiplot_prov$LowQuality) | (nfiplot_prov$management_BAU==0)] <- NA

    cli::cli_li(paste0("Defining management scenario (historical demand)"))
    volumes <- aprofit_decade_prov_spp |>
      dplyr::filter(Province == provinceStrings[iprov],
                    Decade == "2001-2010")
    volumes_2001_2010 <- volumes$Volume
    names(volumes_2001_2010) <- volumes$Species
    scen_2001_2010 <- create_management_scenario(default_prescriptions,  
                                                 volumes_2001_2010) 
    
    cli::cli_li(paste0("Entering fordyn_scenario()"))
    res_01_10 <- fordyn_scenario(nfiplot_prov, SpParamsMED, meteo = interpolators, progress = progress,
                                 CO2ByYear = CO2ByYear,
                                 volume_function = volume_scenario, volume_arguments = list(province = provinces[iprov]),
                                 local_control = local_control,
                                 management_scenario = scen_2001_2010, 
                                 summary_function = summary_scenario, summary_arguments = list(SpParams = SpParamsMED),
                                 parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
    
    cli::cli_li(paste0("Storing results"))
    saveRDS(res_01_10, paste0("Rdata/historic/", provinceStrings[iprov], "_2001_2010.rds"))
  }
}


# Historical 2011-2020 --------------------------------------------------------
if(historic_2011_2020) {
  cli::cli_h1(paste0("SIMULATION 2011-2020 (all scenarios)"))
  
  years <- 2011:2020
  
  cli::cli_li(paste0("CO2 levels for years 2001 to 2010"))
  CO2ByYear <- CO2_ppm |> 
    dplyr::filter(Year %in% years) 
  CO2ByYear <- CO2ByYear$RCP45
  names(CO2ByYear) <- years
  
  cli::cli_li(paste0("Loading interpolators for years ", years[1]," to ", years[length(years)]))
  interpolators <- vector("list", length(years))
  for(iy in 1:length(years)) {
    interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                        paste0("Products/InterpolationData/Catalunya/Historic/calibrated_2.0/interpolator_", years[iy],"_calibrated.nc"))
    interpolators[[iy]] <- load_interpolator(interpolator_file, years[iy])
  }
  
  
  for(iprov in iprovinces) {
    cli::cli_h2(paste0("PROVINCE: ", provinceStrings[iprov]))
    
    cli::cli_li(paste0("Recovering previous run"))
    res_01_10 <- readRDS(paste0("Rdata/historic/", provinceStrings[iprov],"_2001_2010.rds"))
    
    cli::cli_li(paste0("Defining management scenario (historical demand)"))
    volumes <- aprofit_decade_prov_spp |>
      dplyr::filter(Province == provinceStrings[iprov],
                    Decade == "2011-2020")
    volumes_2011_2020 <- volumes$Volume
    names(volumes_2011_2020) <- volumes$Species
    scen_2011_2020 <- create_management_scenario(default_prescriptions,  
                                                 volumes_2011_2020) 
    
    
    cli::cli_li(paste0("Entering fordyn_scenario()"))
    res_11_20 <- fordyn_scenario(res_01_10, SpParamsMED, meteo = interpolators, progress = progress,
                                 CO2ByYear = CO2ByYear,
                                 volume_function = volume_scenario,  volume_arguments = list(province = provinces[iprov]),
                                 local_control = local_control,
                                 management_scenario = scen_2011_2020, 
                                 summary_function = summary_scenario, summary_arguments = list(SpParams = SpParamsMED),
                                 parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
    
    cli::cli_li(paste0("Storing results"))
    saveRDS(res_11_20, paste0("Rdata/historic/", provinceStrings[iprov],"_2011_2020.rds"))
  }
}


# Business as usual (BAU) -------------------------------------------------

if(BAU_2021_2100) {
  climate_models <- "mpiesm_rca4"
  climate_scens <- c("rcp45", "rcp85")
  for(climate_model in climate_models) {
    for(climate_scen in climate_scens) {
      cli::cli_h1(paste0("SIMULATION 2021-2100 / BAU / ", climate_model, " / ", climate_scen))
      
      for(iprov in iprovinces) {
        cli::cli_h2(paste0("PROVINCE: ", provinceStrings[iprov]))
        
        cli::cli_li(paste0("Recovering end of historical run"))
        res <- readRDS(paste0("Rdata/historic/", provinceStrings[iprov], "_2011_2020.rds"))
        
        cli::cli_li(paste0("Re-assigning management units by dominant species and excluding plots from management"))
        next_sf <- res$next_sf
        next_sf$management_unit <- assign_management_unit(next_sf$dominant_tree_species, default_prescriptions)
        next_sf$management_unit[(next_sf$LowQuality) | (next_sf$management_BAU==0)] <- NA
        next_sf$management_arguments[(next_sf$LowQuality) | (next_sf$management_BAU==0)] <- list(NULL)
        res$next_sf <- next_sf
        
        yearsIni <- seq(2021 , 2091, by=10)
        yearsFin <- seq(2030, 2100, by=10)
        for(iy in 1:length(yearsIni)) {
          res_file <- paste0("Rdata/BAU/BAU_", provinceStrings[iprov],"_", climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds")
          if(!file.exists(res_file) || overwrite) {
            cli::cli_li(paste0("Loading interpolator for years ", yearsIni[iy]," to ", yearsFin[iy]))
            interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                                paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                       climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                       yearsIni[iy], "_", yearsFin[iy],".nc"))
            interpolator <- meteoland::read_interpolator(interpolator_file)
            file.remove(interpolator_file)
            
            
            cli::cli_li(paste0("CO2 levels for years ", yearsIni[iy]," to ", yearsFin[iy]))
            CO2ByYear <- CO2_ppm |> 
              dplyr::filter(Year %in% yearsIni[iy]:yearsFin[iy]) 
            CO2ByYear <- CO2ByYear[[toupper(climate_scen)]]
            names(CO2ByYear) <- yearsIni[iy]:yearsFin[iy]
            
            cli::cli_li(paste0("Defining management scenario (30% extraction rates)"))
            volumes <- aprofit_decade_prov_spp[,1:4] |>
              dplyr::filter(Province == provinceStrings[iprov]) |>
              dplyr::group_by(Species) |>
              dplyr::summarise(Volume = mean(Volume, na.rm=TRUE), .groups = "drop") 
            volumes_2001_2020 <- volumes$Volume
            names(volumes_2001_2020) <- volumes$Species
            
            rates <- rep(30, 10) # 30% extraction rate
            names(rates) <- as.character(yearsIni[iy]:yearsFin[iy])
            scen_BAU <- create_management_scenario(units = default_prescriptions,  
                                                        annual_demand_by_species = volumes_2001_2020,
                                                        extraction_rate_by_year = rates) 
            
            cli::cli_li(paste0("Entering fordyn_scenario()"))
            res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator, progress = progress,
                                   CO2ByYear = CO2ByYear,
                                   volume_function = volume_scenario, volume_arguments = list(province = provinces[iprov]),
                                   local_control = local_control,
                                   management_scenario = scen_BAU, 
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
      
      for(iprov in iprovinces) {
        cli::cli_h2(paste0("PROVINCE: ", provinceStrings[iprov]))
        
        res_file <- paste0("Rdata/AMF/AMF_",provinceStrings[iprov], "_", climate_model,"_",climate_scen,"_2021_2030.rds")
        if(!file.exists(res_file) || overwrite) {
          cli::cli_li(paste0("Recovering end of historical run"))
          res <- readRDS(paste0("Rdata/historic/", provinceStrings[iprov], "_2011_2020.rds"))
          
          cli::cli_li(paste0("Re-assigning management units by dominant species and excluding plots from management"))
          next_sf <- res$next_sf
          next_sf$management_unit <- assign_management_unit(next_sf$dominant_tree_species, default_prescriptions)
          next_sf$management_unit[(next_sf$LowQuality) | (next_sf$managment_AMF==0)] <- NA
          next_sf$management_arguments[(next_sf$LowQuality) | (next_sf$managment_AMF==0)] <- list(NULL)
          res$next_sf <- next_sf
          
          # 2021-2030
          cli::cli_li(paste0("CO2 levels for years 2021 to 2030"))
          CO2ByYear <- CO2_ppm |> 
            dplyr::filter(Year %in% 2021:2030) 
          CO2ByYear <- CO2ByYear$RCP45
          names(CO2ByYear) <- 2021:2030
          
          cli::cli_li(paste0("Loading interpolator for years ", 2021," to ", 2030))
          interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                              paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                     climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                     2021, "_", 2030,".nc"))
          interpolator <- meteoland::read_interpolator(interpolator_file)
          file.remove(interpolator_file)
          
          
          cli::cli_li(paste0("Defining management scenario (40% extraction rates)"))
          volumes <- aprofit_decade_prov_spp[,1:4] |>
            dplyr::filter(Province == provinceStrings[iprov]) |>
            dplyr::group_by(Species) |>
            dplyr::summarise(Volume = mean(Volume, na.rm=TRUE), .groups = "drop") 
          volumes_2001_2020 <- volumes$Volume
          names(volumes_2001_2020) <- volumes$Species
          
          rates <- rep(40, 10) # 40% extraction rate
          names(rates) <- as.character(2021:2030)
          scen_AMF <- create_management_scenario(units = default_prescriptions,  
                                                 annual_demand_by_species = volumes_2001_2020,
                                                 extraction_rate_by_year = rates) 
          
          cli::cli_li(paste0("Entering fordyn_scenario()"))
          res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator, progress = progress,
                                 CO2ByYear = CO2ByYear,
                                 volume_function = volume_scenario, volume_arguments = list(province = provinces[iprov]),
                                 local_control = local_control,
                                 management_scenario = scen_AMF, 
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
          res_file <- paste0("Rdata/AMF/AMF_", provinceStrings[iprov], "_",climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds")
          
          if(!file.exists(res_file) || overwrite) {
            cli::cli_li(paste0("Loading interpolator for years ", yearsIni[iy]," to ", yearsFin[iy]))
            interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                                paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                       climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                       yearsIni[iy], "_", yearsFin[iy],".nc"))
            interpolator <- meteoland::read_interpolator(interpolator_file)
            file.remove(interpolator_file)
            
            cli::cli_li(paste0("CO2 levels for years ", yearsIni[iy]," to ", yearsFin[iy]))
            CO2ByYear <- CO2_ppm |> 
              dplyr::filter(Year %in% yearsIni[iy]:yearsFin[iy]) 
            CO2ByYear <- CO2ByYear[[toupper(climate_scen)]]
            names(CO2ByYear) <- yearsIni[iy]:yearsFin[iy]
            
            cli::cli_li(paste0("Defining management scenario (70% extraction rates)"))
            volumes <- aprofit_decade_prov_spp[,1:4] |>
              dplyr::filter(Province == provinceStrings[iprov]) |>
              dplyr::group_by(Species) |>
              dplyr::summarise(Volume = mean(Volume, na.rm=TRUE), .groups = "drop") 
            volumes_2001_2020 <- volumes$Volume
            names(volumes_2001_2020) <- volumes$Species
            
            rates <- rep(70, 10) # 70% extraction rate
            names(rates) <- as.character(yearsIni[iy]:yearsFin[iy])
            scen_AMF <- create_management_scenario(units = default_prescriptions,  
                                                   annual_demand_by_species = volumes_2001_2020,
                                                   extraction_rate_by_year = rates) 
            
            cli::cli_li(paste0("Entering fordyn_scenario()"))
            res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator, progress = progress,
                                   CO2ByYear = CO2ByYear,
                                   volume_function = volume_scenario, volume_arguments = list(province = provinces[iprov]),
                                   local_control = local_control,
                                   management_scenario = scen_AMF, 
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
      
      for(iprov in iprovinces) {
        cli::cli_h2(paste0("PROVINCE: ", provinceStrings[iprov]))
        
        cli::cli_li(paste0("Recovering end of historical run"))
        res <- readRDS(paste0("Rdata/historic/", provinceStrings[iprov], "_2011_2020.rds"))
        
        cli::cli_li(paste0("Re-assigning management units by dominant species and excluding plots from management"))
        next_sf <- res$next_sf
        next_sf$management_unit <- assign_management_unit(next_sf$dominant_tree_species, default_prescriptions)
        next_sf$management_unit[(next_sf$LowQuality) | (next_sf$managment_RSB==0)] <- NA
        next_sf$management_arguments[(next_sf$LowQuality) | (next_sf$managment_RSB==0)] <- list(NULL)
        
        # 2021 - 2030
        res_file <- paste0("Rdata/RSB/RSB_",provinceStrings[iprov], "_",climate_model,"_",climate_scen,"_2021_2030.rds")
        if(!file.exists(res_file) || overwrite) {
          cli::cli_li(paste0("Applying land-use changes (2021-2030)"))
          sel_to_agri <- next_sf$prior_agri %in% 1:10
          sel_to_pasture <- next_sf$prior_pasture %in% 1:10
          sel_to_remove <- sel_to_agri | sel_to_pasture
          i_herb <- which(sel_to_remove)
          ef<- emptyforest()
          ef$herbCover <- 90
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
          
          cli::cli_li(paste0("CO2 levels for years 2021 to 2030"))
          CO2ByYear <- CO2_ppm |> 
            dplyr::filter(Year %in% 2021:2030) 
          CO2ByYear <- CO2ByYear[[toupper(climate_scen)]]
          names(CO2ByYear) <- 2021:2030
          
          cli::cli_li(paste0("Defining management scenario (30% extraction rates)"))
          volumes <- aprofit_decade_prov_spp[,1:4] |>
            dplyr::filter(Province == provinceStrings[iprov]) |>
            dplyr::group_by(Species) |>
            dplyr::summarise(Volume = mean(Volume, na.rm=TRUE), .groups = "drop") 
          volumes_2001_2020 <- volumes$Volume
          names(volumes_2001_2020) <- volumes$Species
          rates <- rep(30, 10) # 30% extraction rate
          names(rates) <- as.character(2021:2030)
          scen_RSB <- create_management_scenario(units = default_prescriptions,  
                                                 annual_demand_by_species = volumes_2001_2020,
                                                 extraction_rate_by_year = rates) 
          
          cli::cli_li(paste0("Entering fordyn_scenario()"))
          res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                                 CO2ByYear = CO2ByYear,
                                 volume_function = volume_scenario, volume_arguments = list(province = provinces[iprov]),
                                 local_control = local_control,
                                 management_scenario = scen_RSB, 
                                 summary_function = summary_scenario, summary_arguments = list(SpParams = SpParamsMED),
                                 parallelize = TRUE, chunk_size = chunk_size, num_cores = num_cores)
          saveRDS(res, res_file)
        } else {
          res <- readRDS(res_file)
        }
        
        # 2031 - 2040
        res_file <- paste0("Rdata/RSB/RSB_",provinceStrings[iprov], "_",climate_model,"_",climate_scen,"_2031_2040.rds")
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
          
          cli::cli_li(paste0("CO2 levels for years 2031 to 2040"))
          CO2ByYear <- CO2_ppm |> 
            dplyr::filter(Year %in% 2031:2040) 
          CO2ByYear <- CO2ByYear[[toupper(climate_scen)]]
          names(CO2ByYear) <- 2031:2040
          
          cli::cli_li(paste0("Defining management scenario (30% extraction rates)"))
          volumes <- aprofit_decade_prov_spp[,1:4] |>
            dplyr::filter(Province == provinceStrings[iprov]) |>
            dplyr::group_by(Species) |>
            dplyr::summarise(Volume = mean(Volume, na.rm=TRUE), .groups = "drop") 
          volumes_2001_2020 <- volumes$Volume
          names(volumes_2001_2020) <- volumes$Species
          rates <- rep(30, 10) # 30% extraction rate
          names(rates) <- as.character(2031:2040)
          scen_RSB <- create_management_scenario(units = default_prescriptions,  
                                                 annual_demand_by_species = volumes_2001_2020,
                                                 extraction_rate_by_year = rates) 
          
          cli::cli_li(paste0("Entering fordyn_scenario()"))
          res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                                 CO2ByYear = CO2ByYear,
                                 volume_function = volume_scenario, volume_arguments = list(province = provinces[iprov]),
                                 local_control = local_control,
                                 management_scenario = scen_RSB, 
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
          res_file <- paste0("Rdata/RSB/RSB_",provinceStrings[iprov], "_",climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds")

          if(!file.exists(res_file) || overwrite) {
            cli::cli_li(paste0("Loading interpolator for years ", yearsIni[iy]," to ", yearsFin[iy]))
            interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                                paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                       climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                       yearsIni[iy], "_", yearsFin[iy],".nc"))
            interpolator <- meteoland::read_interpolator(interpolator_file)
            file.remove(interpolator_file)

            cli::cli_li(paste0("CO2 levels for years ", yearsIni[iy]," to ", yearsFin[iy]))
            CO2ByYear <- CO2_ppm |> 
              dplyr::filter(Year %in% yearsIni[iy]:yearsFin[iy]) 
            CO2ByYear <- CO2ByYear[[toupper(climate_scen)]]
            names(CO2ByYear) <- yearsIni[iy]:yearsFin[iy]
            
            
            cli::cli_li(paste0("Defining management scenario (30% extraction rates)"))
            volumes <- aprofit_decade_prov_spp[,1:4] |>
              dplyr::filter(Province == provinceStrings[iprov]) |>
              dplyr::group_by(Species) |>
              dplyr::summarise(Volume = mean(Volume, na.rm=TRUE), .groups = "drop") 
            volumes_2001_2020 <- volumes$Volume
            names(volumes_2001_2020) <- volumes$Species
            rates <- rep(30, 10) # 30% extraction rate
            names(rates) <- as.character(yearsIni[iy]:yearsFin[iy])
            scen_RSB <- create_management_scenario(units = default_prescriptions,  
                                                   annual_demand_by_species = volumes_2001_2020,
                                                   extraction_rate_by_year = rates) 
            
            cli::cli_li(paste0("Entering fordyn_scenario()"))
            res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                                   CO2ByYear = CO2ByYear,
                                   volume_function = volume_scenario, volume_arguments = list(province = provinces[iprov]),
                                   local_control = local_control,
                                   management_scenario = scen_RSB, 
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
      
      
      for(iprov in iprovinces) {
        cli::cli_h2(paste0("PROVINCE: ", provinceStrings[iprov]))
        
        cli::cli_li(paste0("Recovering end of historical run"))
        res <- readRDS(paste0("Rdata/historic/", provinceStrings[iprov], "_2011_2020.rds"))
        
        cli::cli_li(paste0("Re-assigning management units by dominant species and excluding plots from management"))
        next_sf <- res$next_sf
        next_sf$management_unit <- assign_management_unit(next_sf$dominant_tree_species, default_prescriptions)
        next_sf$management_unit[(next_sf$LowQuality) | (next_sf$managment_ASEA==0)] <- NA
        next_sf$management_arguments[(next_sf$LowQuality) | (next_sf$managment_ASEA==0)] <- list(NULL)
        res$next_sf <- next_sf 
        
        yearsIni <- seq(2021 , 2091, by=10)
        yearsFin <- seq(2030, 2100, by=10)
        for(iy in 1:length(yearsIni)) {
          
          res_file <- paste0("Rdata/ASEA/ASEA_",provinceStrings[iprov], "_",climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds")
          
          if(!file.exists(res_file) || overwrite) {
            cli::cli_li(paste0("Loading interpolator for years ", yearsIni[iy]," to ", yearsFin[iy]))
            interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                                paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                       climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                       yearsIni[iy], "_", yearsFin[iy],".nc"))
            interpolator <- meteoland::read_interpolator(interpolator_file)
            file.remove(interpolator_file)
            
            cli::cli_li(paste0("CO2 levels for years ", yearsIni[iy]," to ", yearsFin[iy]))
            CO2ByYear <- CO2_ppm |> 
              dplyr::filter(Year %in% yearsIni[iy]:yearsFin[iy]) 
            CO2ByYear <- CO2ByYear[[toupper(climate_scen)]]
            names(CO2ByYear) <- yearsIni[iy]:yearsFin[iy]
            
            
            cli::cli_li(paste0("Defining management scenario (30% extraction rates)"))
            volumes <- aprofit_decade_prov_spp[,1:4] |>
              dplyr::filter(Province == provinceStrings[iprov]) |>
              dplyr::group_by(Species) |>
              dplyr::summarise(Volume = mean(Volume, na.rm=TRUE), .groups = "drop") 
            volumes_2001_2020 <- volumes$Volume
            names(volumes_2001_2020) <- volumes$Species
            rates <- rep(30, 10) # 30% extraction rate
            names(rates) <- as.character(yearsIni[iy]:yearsFin[iy])
            scen_ASEA <- create_management_scenario(units = default_prescriptions,  
                                                    annual_demand_by_species = volumes_2001_2020,
                                                    extraction_rate_by_year = rates) 
            
            cli::cli_li(paste0("Entering fordyn_scenario()"))
            res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                                   CO2ByYear = CO2ByYear,
                                   volume_function = volume_scenario, volume_arguments = list(province = provinces[iprov]),
                                   local_control = local_control,
                                   management_scenario = scen_ASEA, 
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


# Acompanyament de l'ecosistema per l'adaptació al canvi global (ACG) --------

if(ACG_2021_2100) {
  climate_models <- "mpiesm_rca4"
  climate_scens <- c("rcp45", "rcp85")
  for(climate_model in climate_models) {
    for(climate_scen in climate_scens) {
      cli::cli_h1(paste0("SIMULATION 2021-2100 / ACG / ", climate_model, " / ", climate_scen))
      
      for(iprov in iprovinces) {
        cli::cli_h2(paste0("PROVINCE: ", provinceStrings[iprov]))
        
        cli::cli_li(paste0("Recovering end of historical run"))
        res <- readRDS(paste0("Rdata/historic/", provinceStrings[iprov], "_2011_2020.rds"))
        
        cli::cli_li(paste0("Re-assigning management units by dominant species and excluding plots from management"))
        next_sf <- res$next_sf
        next_sf$management_unit <- assign_management_unit(next_sf$dominant_tree_species, adaptation_prescriptions)
        next_sf$management_unit[next_sf$managment_ACG==0] <- NA
        next_sf$management_arguments[next_sf$managment_ACG==0] <- list(NULL)
        res$next_sf <- next_sf 
        
        yearsIni <- seq(2021 , 2091, by=10)
        yearsFin <- seq(2030, 2100, by=10)
        for(iy in 1:length(yearsIni)) {
          
          res_file <- paste0("Rdata/ACG/ACG_", provinceStrings[iprov],"_",climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds")
          if(!file.exists(res_file) || overwrite) {
            cli::cli_li(paste0("Loading interpolator for years ", yearsIni[iy]," to ", yearsFin[iy]))
            interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                                paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                       climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                       yearsIni[iy], "_", yearsFin[iy],".nc"))
            interpolator <- meteoland::read_interpolator(interpolator_file)
            file.remove(interpolator_file)
            
            cli::cli_li(paste0("CO2 levels for years ", yearsIni[iy]," to ", yearsFin[iy]))
            CO2ByYear <- CO2_ppm |> 
              dplyr::filter(Year %in% yearsIni[iy]:yearsFin[iy]) 
            CO2ByYear <- CO2ByYear[[toupper(climate_scen)]]
            names(CO2ByYear) <- yearsIni[iy]:yearsFin[iy]
            
            
            cli::cli_li(paste0("Defining management scenario (no demand and adaptation prescriptions)"))
            scen_ACG <- create_management_scenario(units = adaptation_prescriptions) 
            
            cli::cli_li(paste0("Entering fordyn_scenario()"))
            res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                                   CO2ByYear = CO2ByYear,
                                   volume_function = volume_scenario, volume_arguments = list(province = provinces[iprov]),
                                   local_control = local_control,
                                   management_scenario = scen_ACG, 
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
      
      for(iprov in iprovinces) {
        cli::cli_h2(paste0("PROVINCE: ", provinceStrings[iprov]))
        
        cli::cli_li(paste0("Recovering end of historical run"))
        res <- readRDS(paste0("Rdata/historic/", provinceStrings[iprov], "_2011_2020.rds"))
        
        cli::cli_li(paste0("Re-assigning management units to missing"))
        next_sf <- res$next_sf
        next_sf$management_unit <- NA
        next_sf$management_arguments <- list(NULL)
        res$next_sf <- next_sf
        
        yearsIni <- seq(2021 , 2091, by=10)
        yearsFin <- seq(2030, 2100, by=10)
        for(iy in 1:length(yearsIni)) {
          
          res_file <- paste0("Rdata/NOG/NOG_", provinceStrings[iprov],"_", climate_model,"_",climate_scen,"_", yearsIni[iy],"_", yearsFin[iy],".rds")
          if(!file.exists(res_file) || overwrite) {
            cli::cli_li(paste0("Loading interpolator for years ", yearsIni[iy]," to ", yearsFin[iy]))
            interpolator_file <- EMFdatautils::download_emfdata(climate_base,
                                                                paste0("Products/InterpolationData/Catalunya/Projections/", 
                                                                       climate_model, "_", climate_scen,"_daily_interpolator_", 
                                                                       yearsIni[iy], "_", yearsFin[iy],".nc"))
            interpolator <- meteoland::read_interpolator(interpolator_file)
            file.remove(interpolator_file)
            
            cli::cli_li(paste0("CO2 levels for years ", yearsIni[iy]," to ", yearsFin[iy]))
            CO2ByYear <- CO2_ppm |> 
              dplyr::filter(Year %in% yearsIni[iy]:yearsFin[iy]) 
            CO2ByYear <- CO2ByYear[[toupper(climate_scen)]]
            names(CO2ByYear) <- yearsIni[iy]:yearsFin[iy]
            
            cli::cli_li(paste0("Defining management scenario (no management)"))
            scen_NOG <- create_management_scenario(units = 1) 
            
            cli::cli_li(paste0("Entering fordyn_scenario()"))
            res <- fordyn_scenario(res, SpParamsMED, meteo = interpolator,
                                   CO2ByYear = CO2ByYear,
                                   volume_function = volume_scenario, volume_arguments = list(province = provinces[iprov]),
                                   local_control = local_control,
                                   management_scenario = scen_NOG, 
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


