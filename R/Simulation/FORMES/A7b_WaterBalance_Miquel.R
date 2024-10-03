#########################################################################################################
##
## Calculate Water Balance (script por el servidor del CREAF)
##
#########################################################################################################

rm(list=ls())
library(medfate) # 3.2.0
library(medfateland) #2.0.5
library(medfateutils) # 0.1.7
library(tidyverse)

# INITIAL SETTINGS -----
scn_type <- "RSB"
rcp <- "85"

overwrite <- FALSE

# set directory (for meteo, scenarios and savings)
# setwd("~/work/model_application/FORESFUTURE_WB/" )

# parallelize
parallelize = T
num_cores = parallel::detectCores()-2

##### ----- #####

# load nfiplot
nfiplot <- readRDS("nfiplot.rds") # Load nfiplot file

nfiplot$IDPARCELA <- sub("^0+", "", nfiplot$IDPARCELA) 
nfiplot$Provincia <- sub("^0+", "", nfiplot$Provincia)


# summary medfate
summary_scenario_spwb <- function(object, ...) {
  summary_wb <- medfate::summary.spwb(object, 
                                      output = "WaterBalance", 
                                      freq = "years", FUN = sum, na.rm=TRUE)
  summary_fire_max <- medfate::summary.spwb(object, 
                                            output = "FireHazard", 
                                            freq = "years", 
                                            FUN = max, na.rm = TRUE)
  summary_fire_min <- medfate::summary.spwb(object, 
                                            output = "FireHazard", 
                                            freq = "years", 
                                            FUN = min, na.rm = TRUE)
  summary_stand_mean <- medfate::summary.spwb(object, 
                                              output="Stand", 
                                              freq="years", 
                                              FUN = mean, na.rm=TRUE)
  colnames(summary_stand_mean)[c(4,6)] <- c("LAI_mean", "Cm_mean")
  summary_stand_min <- medfate::summary.spwb(object, 
                                             output="Stand", 
                                             freq="years", 
                                             FUN = min, months=6:9, na.rm=TRUE)
  colnames(summary_stand_min)[c(4,6)] <- c("LAI_min", "Cm_min")
  summary_stand_max <- medfate::summary.spwb(object, 
                                             output="Stand", 
                                             freq="years", 
                                             FUN = max, months=6:9, na.rm=TRUE)
  colnames(summary_stand_max)[c(4,6)] <- c("LAI_max", "Cm_max")
  lai_coh_year <- summary(object, output="Plants$LAI", FUN = max)
  plc_coh_year <- summary(object, output="Plants$StemPLC", FUN = max)
  stress_coh_year <- summary(object, output="Plants$PlantStress", FUN = max)
  summary_stress <- data.frame(PlantStress = rowSums(stress_coh_year*lai_coh_year)/rowSums(lai_coh_year),
                               StemPLC = rowSums(plc_coh_year*lai_coh_year)/rowSums(lai_coh_year))
  return(cbind(summary_wb,
               summary_fire_max[, c(13,14), drop = FALSE],
               summary_fire_min[, c(1,2,3), drop = FALSE],
               summary_stand_mean[,c(4,6), drop = FALSE], # LAI, Cm average (for regulation)
               summary_stand_min[,c(4,6), drop = FALSE], # LAI, Cm min in summer
               summary_stand_max[,c(4,6), drop = FALSE], # LAI, Cm max in summer
               summary_stress))
}

projection_meteo <- function(ids,  start_year = NULL, end_year=NULL, rcp=NULL) {
  interpolated_data <- readRDS(paste0("Interpolated_climate_projections/nfi_",rcp,"_", start_year, "_", end_year,".rds"))
  interpolated_data$IDPARCELA <- sub("^0+", "", interpolated_data$IDPARCELA)
  
  meteo <- vector("list", length(ids))
  pb=txtProgressBar(1, length(ids), style=3)
  for(i in 1:length(ids)) {
    setTxtProgressBar(pb, i)
    id <- ids[i]
    names(meteo)[i] <- id
    meteo[[i]] <- interpolated_data$interpolated_data[[which(interpolated_data$IDPARCELA==id)]]
  }
  # remove meteo and free memory
  rm(interpolated_data)
  gc()
  return(meteo)
}


# base sf object
sf <- nfiplot |>
  select(id, IDPARCELA, Provincia, elevation, slope, aspect, soil, forest)


# medfateland parameters
local_control = defaultControl()
local_control$fireHazardResults = T
local_control$fireHazardStandardWind = 11
local_control$cavitationRefill = "rate"


############ PROJECTIONS ###########
# inputs for directory
pattern <- "\\d{4}-\\d{4}"
prov <- c(8,17,25,43)
dirs <- list.files(paste0("Scenarios/",scn_type , "/", paste0(scn_type, "_", rcp)), full.names = T)

WB_medfate_OCCC_projections <- function(){
  for(d in dirs){ # for each directory
    # retrieve years range
    year_range <- unlist(strsplit(str_extract(d, pattern), "-"))
    start_year <- as.numeric(year_range[1])
    end_year <- as.numeric(year_range[2])
    if(end_year-start_year > 9){stop("Wrong year range for projections")}
    
    for(p in prov){ # for each province
      
      file_wb <- paste0("WB/", scn_type, "/wb_",rcp,"_",  p, "_", start_year, "_", end_year, ".rds")
      if(!file.exists(file_wb) || overwrite) {
        cat("Calculating WB for province ", p, " and years ", start_year, "-", end_year, "\n")
        
        # create sf_prov by province
        sf_prov <- sf |>
          filter(Provincia == p) 
        
        # read scenario 
        scn <- readRDS(paste0(d,"/", p, ".rds"))
        
        treeData <- scn$treeDataSequence
        if(length(unique(treeData$Step))>2){stop("too many steps present in scenario")}
        
        # transform treedata according to medfate
        treeData_mod <- treeData %>%
          filter(Step==1) %>%
          select(-c("Step", "N")) %>% 
          mutate(DBH = DBH*10, Dn2 = DBH) %>% #  DBH from cm to mm
          rename(Ht=H, Dn1 = DBH, Especie=Species) %>%
          relocate(Dn2, .after = Dn1)
        
        treeData_forest <- IFN2forest(treeData_mod,
                                      IFN_species_mapping = IFN_species_mapping,
                                      SpParams = SpParamsMED)
        
        # replace original N
        cat("Replacing original N...", "\n")
        for(i in names(treeData_forest)){
          # print(i)
          if(length(treeData_forest[[i]]$treeData$N)==length(treeData[treeData$ID== i & treeData$Step== 1,"N"])){
            treeData_forest[[i]]$treeData$N <- treeData[treeData$ID== i & treeData$Step== 1,"N"]
          } else {warning("Plot ", i, " doesn't have the same length as treeData")}
          
          # replace in sf_prov$forest the treeData
          sf_prov[sf_prov$IDPARCELA == i,]$forest[[1]]$treeData <- treeData_forest[[i]]$treeData
        }
        
        cat("loading meteo...", "\n")
        sf_prov$meteo <- projection_meteo(sf_prov$IDPARCELA, start_year = start_year,
                                          end_year = end_year, rcp = rcp)
        sf_prov$land_cover_type <- "wildland"
        
        res <- spwb_spatial(sf_prov,
                            SpParamsMED,
                            summary_function = summary_scenario_spwb,
                            local_control = local_control,
                            keep_results = F,
                            parallelize = parallelize,
                            num_cores = num_cores)
        
        # save results 
        saveRDS(res, file_wb) 
      }
      
    }
  }
}

WB_medfate_OCCC_projections()












