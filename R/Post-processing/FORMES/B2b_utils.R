library(IFNallometry)
library(medfuels)

provinces <- c(8,17,25,43)
provinceStrings <- c("Barcelona", "Girona", "Lleida", "Tarragona")


cli::cli_li(paste0("Defining function to load and check historical interpolator"))
load_interpolator <- function(interpolator_file, year) {
  interp <- meteoland::read_interpolator(interpolator_file)
  file.remove(interpolator_file)
  # Set missing wind speed values to NA
  ws0 <- interp$WindSpeed==0
  ws0[is.na(ws0)] <- FALSE
  if(sum(ws0)>0) {
    cli::cli_li(paste0("Number of 0 windspeed for year ", year, ": ", sum(ws0)))
    interp$WindSpeed[ws0] <- NA
  }
  return(interp)
}

cli::cli_li(paste0("Defining tree biomass function"))
tree_biomass_scenario<-function(x, SpParams, as.CO2 = TRUE){
  if(inherits(x, "forest")) x <- x$treeData
  ntree <- nrow(x)
  if(ntree>0) {
    IFN_codes <- species_characterParameter(x$Species, SpParams, "IFNcodes")
    IFN_codes_split <- strsplit(IFN_codes, "/")
    IFN_codes <- sapply(IFN_codes_split, function(x) return(x[[1]]))
    y <- data.frame(ID = rep("XX", ntree), 
                    Species = IFN_codes,
                    DBH = x$DBH,
                    H = x$Height/100,
                    N = x$N
    )
    biom <- IFNallometry::IFNbiomass(y, as.CO2 = as.CO2, verbose = FALSE)
    return(bind_cols(x,biom[,-c(1,2)]))
  }
  return(numeric(0))
}

tree_biomass_scenario_FORMES<-function(x, SpParams, as.CO2 = TRUE){
  if(inherits(x, "forest")) x <- x$treeData
  ntree <- nrow(x)
  if(ntree>0) {
    IFN_codes <- x$Species
    # IFN_codes_split <- strsplit(IFN_codes, "/")
    # IFN_codes <- sapply(IFN_codes_split, function(x) return(x[[1]]))
    y <- data.frame(ID = rep("XX", ntree), 
                    Species = IFN_codes,
                    DBH = x$DBH,
                    H = x$H,
                    N = x$N
    )
    biom <- IFNallometry::IFNbiomass(y, as.CO2 = as.CO2, verbose = FALSE)
    return(bind_cols(x,biom[,-c(1,2)]))
  }
  return(numeric(0))
}


cli::cli_li(paste0("Defining shrub biomass function"))
shrub_biomass_scenario<-function(x, SpParams, as.CO2 = TRUE){
  if(inherits(x, "forest")) x <- x$shrubData
  nshrub <- nrow(x)
  if(nshrub>0) {
    y <- data.frame(plot = x$id,
                    species = x$Species,
                    H = x$Height,
                    C = x$Cover
    )
    map <- c("Adenocarpus spp."="Adenocarpus telonensis",
             "Anagyris spp." = "Anagyris foetida",
             "Artemisia spp." = "Artemisia campestris",
             "Asparagus spp." = "Asparagus acutifolius",
             "Astragalus spp." = "Astragalus sempervirens",
             "Bupleurum spp." = "Bupleurum fruticescens",
             "Calicotome spp." = "Calicotome spinosa",
             "Cistus salvifolius" = "Cistus salviifolius",
             "Corema spp." = "Corema album",
             "Coronilla spp." = "Coronilla emerus",
             "Cotoneaster spp." = "Cotoneaster integerrimus",
             "Daphne spp." ="Daphne laureola",
             "Dorycnium spp." = "Dorycnium pentaphyllum",
             "Echium spp." = "Echium vulgare",
             "Ephedra spp." = "Ephedra fragilis",
             "Erinacea spp." = "Erinacea anthyllis",
             "Euphorbia spp." = "Euphorbia characias",
             "Genista spp." = "Genista scorpius",
             "Genistella spp." = "Genistella tridentata",
             "Halimium spp." = "Halimium umbellatum",
             "Helianthemum spp." = "Helianthemum apenninum",
             "Helichrysum spp." = "Helichrysum stoechas",
             "Ilex spp." = "Ilex aquifolium",
             "Lavandula spp." = "Lavandula stoechas",
             "Lithodora spp." = "Lithospermum fruticosum",
             "Lavandula spp." = "Lavandula stoechas",
             "Lonicera spp." = "Lonicera implexa",
             "Ononis spp." = "Ononis spinosa",
             "Osyris spp." = "Osyris alba",
             "Phlomis spp." = "Phlomis lychnitis",
             "Retama spp." = "Retama sphaerocarpa",
             "Rhamnus spp." = "Rhamnus alaternus",
             "Rhododendron spp." = "Rhododendron ferrugineum",
             "Ribes spp." = "Ribes uva-crispa",
             "Rosa spp." = "Rosa canina",
             "Rubus spp." = "Rubus ulmifolius",
             "Ruscus spp." = "Ruscus aculeatus",
             "Salix spp." = "Salix purpurea",
             "Salsola spp." = "Salsola genistoides",
             "Salvia rosmarinus" = "Rosmarinus officinalis",
             "Sambucus spp." = "Sambucus nigra",
             "Santolina spp." = "Santolina chamaecyparissus",
             "Sideritis spp." = "Sideritis hirsuta",
             "Spartium spp." = "Spartium junceum",
             "Spiraea spp." = "Spiraea crenata",
             "Teline spp." = "Teline spp.",
             "Teucrium spp." = "Teucrium polium",
             "Thymelaea spp." = "Thymelaea hirsuta",
             "Thymus spp." = "Thymus vulgaris",
             "Ulex spp." = "Ulex parviflorus",
             "Vaccinium spp." = "Vaccinium myrtillus",
             "Viburnum spp." = "Viburnum tinus")
    for(i in 1:length(map)) {
      y$species[y$species==names(map)[i]] <- as.character(map[i])
    }
    vines <- c("Ampelodesmos mauritanica","Clematis spp.", "Hedera helix", "Smilax aspera")
    vine_sel <- (y$species %in% vines)
    y <- y[!vine_sel, , drop = FALSE]
    x <- x[!vine_sel, , drop=FALSE]
    biom_above <- medfuels::shrubspeciesfuel(y, type = "total")
    area <- medfuels::individualshrubarea(y)
    N <- (y$C*100)/area # density of individuals per ha
    biom_above_individual <- biom_above/N
    biom_below_individual <- 0.732*((biom_above_individual*1000)^0.9427)/1000 # From kg above to kg below
    biom_below <- biom_below_individual*N
    biom_above <- biom_above * 10 # From kg/m2 to Mg/ha
    biom_below <- biom_below * 10 # From kg/m2 to Mg/ha
    if(as.CO2) {
      biom_above <- biom_above*0.5*(44/12) # from Mg dry weight to Mg CO2
      biom_below <- biom_below*0.5*(44/12) # from Mg dry weight to Mg CO2
    }
    res <- cbind(x, biom_above, biom_below)
    res <- res |>
      dplyr::rename(Aerial = biom_above, Roots = biom_below)
    return(res)
  }
  return(numeric(0))
}

cli::cli_li(paste0("Defining volume function"))
volume_scenario<-function(x, SpParams, province){
  if(inherits(x, "forest")) x <- x$treeData
  ntree <- nrow(x)
  if(ntree>0) {
    IFN_codes <- species_characterParameter(x$Species, SpParams, "IFNcodes")
    IFN_codes_split <- strsplit(IFN_codes, "/")
    IFN_codes <- sapply(IFN_codes_split, function(x) return(x[[1]]))
    y <- data.frame(ID = rep("XX", ntree), 
                    Province = rep(province, ntree),
                    Species = IFN_codes,
                    DBH = x$DBH,
                    H = x$Height/100,
                    N = x$N
    )
    vol <- IFNallometry::IFNvolume(y)
    vcc <- pmax(0,vol$VCC)
    vcc[x$DBH < 7.5] <- 0
    return(vcc) #m3/ha
  }
  return(numeric(0))
}

volume_scenario_FORMES<-function(x, SpParams, province){
  if(inherits(x, "forest")) x <- x$treeData
  ntree <- nrow(x)
  if(ntree>0) {
    IFN_codes <- x$Species
    # IFN_codes_split <- strsplit(IFN_codes, "/")
    # IFN_codes <- sapply(IFN_codes_split, function(x) return(x[[1]]))
    y <- data.frame(ID = rep("XX", ntree), 
                    Province = rep(province, ntree),
                    Species = IFN_codes,
                    DBH = x$DBH,
                    H = x$H,
                    N = x$N
    )
    vol <- IFNallometry::IFNvolume(y)
    vcc <- pmax(0,vol$VCC)
    vcc[x$DBH < 7.5] <- 0
    return(vcc) #m3/ha
  }
  return(numeric(0))
}

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
  summary_stand_mean <- medfate::summary.fordyn(object, 
                                                output="Stand", 
                                                freq="years", 
                                                FUN = mean, na.rm=TRUE)
  colnames(summary_stand_mean)[c(4,6)] <- c("LAI_mean", "Cm_mean")
  summary_stand_min <- medfate::summary.fordyn(object, 
                                               output="Stand", 
                                               freq="years", 
                                               FUN = min, months=6:9, na.rm=TRUE)
  colnames(summary_stand_min)[c(4,6)] <- c("LAI_min", "Cm_min")
  summary_stand_max <- medfate::summary.fordyn(object, 
                                               output="Stand", 
                                               freq="years", 
                                               FUN = max, months=6:9, na.rm=TRUE)
  colnames(summary_stand_max)[c(4,6)] <- c("LAI_max", "Cm_max")
  f_sum <- t(sapply(object$ForestStructures, function(x) {unlist(summary(x, SpParams))}))
  summary_meteo <- t(sapply(object$GrowthResults, function(x) {
    c(Pdaymax = max(x$weather$Precipitation, na.rm = TRUE), 
      MAT = mean(x$weather$MeanTemperature, na.rm=TRUE))
  }))
  lai_coh_year <- summary(object, output="Plants$LAI", FUN = max)
  plc_coh_year <- summary(object, output="Plants$StemPLC", FUN = max)
  stress_coh_year <- summary(object, output="Plants$PlantStress", FUN = max)
  summary_stress <- data.frame(PlantStress = rowSums(stress_coh_year*lai_coh_year)/rowSums(lai_coh_year),
                               StemPLC = rowSums(plc_coh_year*lai_coh_year)/rowSums(lai_coh_year))
  summary_fmc <- t(sapply(object$GrowthResults, function(x) {
      cfmc_overstory<- x$FireHazard$CFMC_overstory
      cfmc_understory<- x$FireHazard$CFMC_understory
      dfmc <- x$FireHazard$DFMC
      c(CFMC_overstory_min = min(cfmc_overstory, na.rm = TRUE),
        CFMC_understory_min = min(cfmc_understory, na.rm = TRUE),
        DFMC_min = min(dfmc, na.rm = TRUE),
        N120 = sum(cfmc_understory<120, na.rm=TRUE),
        N100 = sum(cfmc_understory<100, na.rm=TRUE),
        N80 = sum(cfmc_understory<80, na.rm=TRUE),
        N880 = sum(dfmc < 8 & cfmc_understory<80), na.rm=TRUE)
  }))
  return(cbind(summary_wb,
               summary_cb,
               f_sum[-1, , drop = FALSE], 
               summary_fire[,c(13,14), drop = FALSE],
               summary_stand_mean[,c(4,6), drop = FALSE], # LAI, Cm average (for regulation)
               summary_stand_min[,c(4,6), drop = FALSE], # LAI, Cm min in summer
               summary_stand_max[,c(4,6), drop = FALSE], # LAI, Cm max in summer
               summary_stress,
               summary_fmc,
               summary_meteo))
}

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

cli::cli_li(paste0("Defining function to assign management unit"))
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
