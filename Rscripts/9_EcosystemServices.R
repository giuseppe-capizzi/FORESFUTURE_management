library(ggplot2)
library(cowplot)
library(tidyverse)
library(medfate)
library(sf)
library(tidyverse)
library(tidyterra)

nfiplot <- dplyr::bind_rows(readRDS(paste0("Rdata/nfiplot.rds")))
lonlat <- sf::st_transform(nfiplot[,"id"], crs = 4326)
lonlat$longitude <- sf::st_coordinates(lonlat)[,1]
longitude_df <- sf::st_drop_geometry(lonlat)
K_LS <- sf::read_sf("Data/ErosionData.gpkg")
comarques <- sf::read_sf("Data/Comarques/comarques.shp")
cat <- sf::st_union(comarques)

# ES1 - Annual timber extraction (m3/ha/yr) per 20-year periods -----------
ES1_function <- function(ALL, model) {
  if(model=="FORMES") {
    ALL$Management <- ALL$Management[ALL$Year=="2021-2030"][1]
    ALL$Year[ALL$Year=="2001-2010"] = "2005"
    ALL$Year[ALL$Year=="2011-2020"] = "2015"
    ALL$Year[ALL$Year=="2021-2030"] = "2025"
    ALL$Year[ALL$Year=="2031-2040"] = "2035"
    ALL$Year[ALL$Year=="2041-2050"] = "2045"
    ALL$Year[ALL$Year=="2051-2060"] = "2055"
    ALL$Year[ALL$Year=="2061-2070"] = "2065"
    ALL$Year[ALL$Year=="2071-2080"] = "2075"
    ALL$Year[ALL$Year=="2081-2090"] = "2085"
    ALL$Year[ALL$Year=="2091-2100"] = "2095"
    ALL$Year <- as.numeric(ALL$Year)
  } 
  ES1 <- ALL |>
    filter(Year!=2000) |>
    select(Climate, Management, Province, id, Year, VolumeStructure, VolumeAdultFirewood, VolumeSaplingFirewood, CutStructure, CutAdultFirewood, CutSaplingFirewood) |>
    mutate(Period = as.character(cut(Year, breaks = c(2000,2020,2040,2060,2080,2100), labels = c("2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100"))),
           MidYear = as.numeric(as.character(cut(Year, breaks = c(2000,2020,2040,2060,2080,2100), labels = c(2010, 2030, 2050, 2070, 2090))))) |>
    group_by(Climate, Management, Province, id, Period, MidYear) |>
    summarise(ES1_VolumeStructure = mean(VolumeStructure, na.rm=TRUE),
              ES1_VolumeAdultFirewood = mean(VolumeAdultFirewood, na.rm=TRUE),
              ES1_VolumeSaplingFirewood = mean(VolumeSaplingFirewood, na.rm=TRUE),
              ES1_CutStructure = mean(CutStructure, na.rm=TRUE),
              ES1_CutAdultFirewood = mean(CutAdultFirewood, na.rm=TRUE),
              ES1_CutSaplingFirewood = mean(CutSaplingFirewood, na.rm=TRUE),
              .groups = "drop") |>
    tidyr::replace_na(list(ES1_CutStructure = 0, ES1_VolumeAdultFirewood = 0, ES1_VolumeSaplingFirewood = 0,
                           ES1_CutStructure = 0, ES1_CutAdultFirewood = 0, ES1_CutSaplingFirewood = 0)) |>
    mutate(Model = model) |>
    relocate(Model, .after = MidYear)|>
    dplyr::mutate(Climate = toupper(Climate))
  return(ES1)
}

# ES2 - Carbon sequestration rate (Mg C/ha/yr) per 20-year periods -----------
ES2_function <- function(ALL, model) {
  ALL1 <- ALL
  if(model=="FORMES") {
    ALL1$Management <- ALL1$Management[ALL1$Year=="2021-2030"][1]
    ALL1$Year[ALL1$Year=="2001-2010"] = "2010"
    ALL1$Year[ALL1$Year=="2011-2020"] = "2020"
    ALL1$Year[ALL1$Year=="2021-2030"] = "2030"
    ALL1$Year[ALL1$Year=="2031-2040"] = "2040"
    ALL1$Year[ALL1$Year=="2041-2050"] = "2050"
    ALL1$Year[ALL1$Year=="2051-2060"] = "2060"
    ALL1$Year[ALL1$Year=="2061-2070"] = "2070"
    ALL1$Year[ALL1$Year=="2071-2080"] = "2080"
    ALL1$Year[ALL1$Year=="2081-2090"] = "2090"
    ALL1$Year[ALL1$Year=="2091-2100"] = "2100"
    ALL1$Year <- as.numeric(ALL1$Year)
  } 
  StockBiomass <- ALL1 |>
    filter(Year!=2000) |>
    select(Climate, Management, Province, id, Year, AdultTreeBiomass, SaplingTreeBiomass, ShrubBiomass) |>
    mutate(Period = as.character(cut(Year, breaks = c(2000,2020,2040,2060,2080,2100), 
                                     labels = c("P1", "P2", "P3", "P4", "P5")))) |>
    group_by(Climate, Management, Province, id, Period) |>
    summarise(ES2_AdultTreeBiomass = mean(AdultTreeBiomass, na.rm=TRUE),
              ES2_SaplingTreeBiomass = mean(SaplingTreeBiomass, na.rm=TRUE),
              ES2_ShrubBiomass = mean(ShrubBiomass, na.rm=TRUE),
              .groups = "drop") |>
    tidyr::replace_na(list(ES2_AdultTreeBiomass = 0, ES2_SaplingTreeBiomass = 0, ES2_ShrubBiomass = 0)) |>
    mutate(ES2_LiveBiomass = ES2_AdultTreeBiomass + ES2_SaplingTreeBiomass+ES2_ShrubBiomass)
  
  BT<-ALL1 |>
    filter(Year %in% c(2000,2020,2040,2060,2080,2100)) |>
    select(Climate, Management, Province, id, Year, AdultTreeBiomass) |>
    pivot_wider(values_from = AdultTreeBiomass, names_from = Year)
  BT$P1 <- BT$`2020` - BT$`2000`
  BT$P2 <- BT$`2040` - BT$`2020`
  BT$P3 <- BT$`2060` - BT$`2040`
  BT$P4 <- BT$`2080` - BT$`2060`
  BT$P5 <- BT$`2100` - BT$`2080`
  BATC <- BT |>
    select(Climate, Management, Province, id, "P1", "P2", "P3", "P4", "P5") |>
    pivot_longer(cols = 5:9, names_to = "Period", values_to = "AdultTreeBiomassChange") |>
    mutate(AdultTreeBiomassChange = AdultTreeBiomassChange/20) |>
    ungroup()
  rm(BT)
  BT<-ALL1 |>
    filter(Year %in% c(2000,2020,2040,2060,2080,2100)) |>
    select(Climate, Management, Province, id, Year, SaplingTreeBiomass) |>
    pivot_wider(values_from = SaplingTreeBiomass, names_from = Year)
  BT$P1 <- BT$`2020` - BT$`2000`
  BT$P2 <- BT$`2040` - BT$`2020`
  BT$P3 <- BT$`2060` - BT$`2040`
  BT$P4 <- BT$`2080` - BT$`2060`
  BT$P5 <- BT$`2100` - BT$`2080`
  BSTC <- BT |>
    select(Climate, Management, Province, id, "P1", "P2", "P3", "P4", "P5") |>
    pivot_longer(cols = 5:9, names_to = "Period", values_to = "SaplingTreeBiomassChange") |>
    mutate(SaplingTreeBiomassChange = SaplingTreeBiomassChange/20) |>
    ungroup()
  rm(BT)
  
  BS<-ALL1 |>
    filter(Year %in% c(2000,2020,2040,2060,2080,2100)) |>
    select(Climate, Management, Province, id, Year, ShrubBiomass) |>
    pivot_wider(values_from = ShrubBiomass, names_from = Year)
  BS$P1 <- BS$`2020` - BS$`2000`
  BS$P2 <- BS$`2040` - BS$`2020`
  BS$P3 <- BS$`2060` - BS$`2040`
  BS$P4 <- BS$`2080` - BS$`2060`
  BS$P5 <- BS$`2100` - BS$`2080`
  BSC <- BS |>
    select(Climate, Management, Province, id, P1, P2, P3, P4, P5) |>
    pivot_longer(cols = 5:9, names_to = "Period", values_to = "ShrubBiomassChange")|>
    mutate(ShrubBiomassChange = ShrubBiomassChange/20) |>
    ungroup()
  rm(BS)
  
  CutBiomass <- ALL1 |>
    filter(Year!=2000) |>
    select(Climate, Management, Province, id, Year, CutBiomassAdultTree, CutBiomassSaplingTree, CutBiomassShrub,
           CutBiomassStructure, CutBiomassAdultFirewood) |>
    mutate(Period = as.character(cut(Year, 
                                     breaks = c(2000,2020,2040,2060,2080,2100), 
                                     labels = c("P1", "P2", "P3", "P4", "P5")
                                     ))) |>
    group_by(Climate, Management, Province, id, Period) |>
    summarise(CutBiomassAdultTree = mean(CutBiomassAdultTree, na.rm=TRUE),
              CutBiomassSaplingTree = mean(CutBiomassSaplingTree, na.rm=TRUE),
              CutBiomassShrub = mean(CutBiomassShrub, na.rm=TRUE),
              CutBiomassStructure = mean(CutBiomassStructure, na.rm=TRUE),
              CutBiomassAdultFirewood = mean(CutBiomassAdultFirewood, na.rm=TRUE),
              .groups = "drop") 
    

  ES2 <- StockBiomass |>
    left_join(CutBiomass, by=c("Climate", "Management", "Province", "id", "Period")) |>
    left_join(BATC, by=c("Climate", "Management", "Province", "id", "Period")) |>
    left_join(BSTC, by=c("Climate", "Management", "Province", "id", "Period")) |>
    left_join(BSC, by=c("Climate", "Management", "Province", "id", "Period")) |>
    tidyr::replace_na(list(AdultTreeBiomassChange = 0, CutBiomassAdultTree = 0, CutBiomassStructure = 0, CutBiomassAdultFirewood = 0, 
                           SaplingTreeBiomassChange = 0, CutBiomassSaplingTree = 0, 
                           ShrubBiomassChange = 0, CutBiomassShrub = 0)) |>
    mutate(ES2_AdultTreeBiomassChange = AdultTreeBiomassChange,
           ES2_CutBiomassStructure = CutBiomassStructure,
           ES2_AdultTreeBiomassSequestr = AdultTreeBiomassChange + CutBiomassStructure, ## Assumes only trees cut for structure involve C sequestration
           ES2_SaplingTreeBiomassSequestr = SaplingTreeBiomassChange,
           ES2_ShrubBiomassSequestr = ShrubBiomassChange,
           ES2_LiveBiomassSequestr = ES2_AdultTreeBiomassSequestr+ES2_SaplingTreeBiomassSequestr+ES2_ShrubBiomassSequestr) |>
    select(-c(10:17))
  
  ES2$Period[ES2$Period=="P1"] <- "2001-2020"
  ES2$Period[ES2$Period=="P2"] <- "2021-2040"
  ES2$Period[ES2$Period=="P3"] <- "2041-2060"
  ES2$Period[ES2$Period=="P4"] <- "2061-2080"
  ES2$Period[ES2$Period=="P5"] <- "2081-2100"
  ES2 <- ES2 |> 
    dplyr::mutate(Climate = toupper(Climate))|>
    mutate(Model = model) |>
    relocate(Model, .after = Period)
  return(ES2)
}

# ES3 - Water provision (l/m3/yr) -----------------------------------------
ES3_function <- function(ALL, model) {
  ES3 <- ALL |>
    filter(Year!=2000) |>
    select(Climate, Management, Province, id, Year, BlueWater, Precipitation) |>
    mutate(Period = as.character(cut(Year, breaks = c(2000,2020,2040,2060,2080,2100), labels = c("2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100"))),
           MidYear = as.numeric(as.character(cut(Year, breaks = c(2000,2020,2040,2060,2080,2100), labels = c(2010, 2030, 2050, 2070, 2090))))) |>
    group_by(Climate, Management, Province, id, Period, MidYear) |>
    summarise(ES3_BlueWater = mean(BlueWater, na.rm=TRUE),
              Precipitation = mean(Precipitation, na.rm=TRUE),
              .groups = "drop") |>
    mutate(ES3_RunoffCoefficient = 100*(ES3_BlueWater/Precipitation)) |>
    mutate(Model = model)  |>
    relocate(Model, .after = MidYear) |>
    select(-Precipitation)|>
    dplyr::mutate(Climate = toupper(Climate))
  return(ES3)
}

# ES4 - Erosion control (Mg/ha/yr) -----------------------------------------------
ES4_function <- function(ALL, model) {
  
  # constants
  a = 2
  b0 = 0.117
  b1 = -0.015

  ALL_SEL <- ALL |>
    filter(Year!=2000) |>
    select(Climate, Management, Province, id, Year, Pdaymax, Precipitation, PARground)|>
    mutate(Period = as.character(cut(Year, breaks = c(2000,2020,2040,2060,2080,2100), labels = c("2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100"))),
           MidYear = as.numeric(as.character(cut(Year, breaks = c(2000,2020,2040,2060,2080,2100), labels = c(2010, 2030, 2050, 2070, 2090))))) |>
    mutate(idparcela = as.character(as.numeric(substr(id, 1,6))))

  ES4<- ALL_SEL |>
    left_join(longitude_df, by="id") |>
    group_by(Climate, Management, Province, id, Period, MidYear) |>
    summarise(ES4_RainfallErosivity = mean(b0*Precipitation*sqrt(Pdaymax)*(a+b1*longitude)),
              .groups = "drop",
              C = mean(PARground/100, na.rm=TRUE)) |>
    left_join(K_LS, by="id") |>
    mutate(ES4_StructuralImpact = Kst*LS*ES4_RainfallErosivity,
           ES4_ErosionMitigation = ES4_StructuralImpact*(1-C)) |>
    select(-c(C, LS, K, Kst)) |>
    mutate(Model = model) |>
    relocate(Model, .after = MidYear) |>
    relocate(geom, .after = ES4_ErosionMitigation) |>
    dplyr::mutate(Climate = toupper(Climate))
  return(ES4)
}

# ES5 - Recreational value [0-1] ------------------------------------------
ES5_function <- function(ALL, model) {
  recr_fun<- function(maxDBH, cvDBH, LAI, maxShrubCover, treeRichness, shrubRichness, na.rm = TRUE) {
    n <- length(maxDBH)
    
    # Individual functions
    val_maxdbh <- 1/(1 + exp((5/25)*(50 - maxDBH)))
    val_lai <- 1/(1 + exp((60/25)*(2 - LAI)))
    val_treerich <- 1/(1 + exp((40/25)*(4 - treeRichness)))
    val_shrubrich <- 1/(1 + exp((40/25)*(4 - shrubRichness)))
    val_maxcov <- dnorm(maxShrubCover, 50, 20)/dnorm(50,50,20)
    val_cvdbh <-  1/(1 + exp((200/25)*(0.5 - cvDBH)))

    # Initial weights
    w_maxdbh <- rep(0.2, n)
    w_cvdbh <- rep(0.1, n)
    w_lai <- rep(0.3, n)
    w_maxcov <- rep(0.1,n)
    w_treerich <- rep(0.2,n)
    w_shrubrich <- rep(0.1,n)
    
    # Avoid missing
    w_maxdbh[is.na(maxDBH)] <- 0
    w_cvdbh[is.na(cvDBH)] <- 0
    w_lai[is.na(LAI)] <- 0
    w_maxcov[is.na(maxShrubCover)] <- 0
    w_treerich[is.na(treeRichness)] <- 0
    w_shrubrich[is.na(shrubRichness)] <- 0
    val_maxdbh[is.na(maxDBH)] <- 0
    val_cvdbh[is.na(cvDBH)] <- 0
    val_lai[is.na(LAI)] <- 0
    val_maxcov[is.na(maxShrubCover)] <- 0
    val_treerich[is.na(treeRichness)] <- 0
    val_shrubrich[is.na(shrubRichness)] <- 0
    
    val_sum <- w_maxdbh*val_maxdbh + w_cvdbh*val_cvdbh +w_lai*val_lai + w_maxcov*val_maxcov + w_treerich*val_treerich + w_shrubrich*val_shrubrich
    w_sum <- w_maxdbh + w_cvdbh +w_lai + w_maxcov + w_treerich + w_shrubrich
    return(mean(val_sum/w_sum, na.rm = na.rm))
  }
  ALL_SEL <- ALL |>
    filter(Year!=2000) |>
    select(Climate, Management, Province, id, Year, cvDBH, maxDBH, MaxShrubCover, LAI_max, TreeRichness, ShrubRichness)|>
    mutate(Period = as.character(cut(Year, breaks = c(2000,2020,2040,2060,2080,2100), labels = c("2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100"))),
           MidYear = as.numeric(as.character(cut(Year, breaks = c(2000,2020,2040,2060,2080,2100), labels = c(2010, 2030, 2050, 2070, 2090))))) |>
    mutate(idparcela = as.character(as.numeric(substr(id, 1,6))))
  
  ES5<- ALL_SEL |>
    group_by(Climate, Management, Province, id, Period, MidYear) |>
    summarise(ES5_RecreationalValue = recr_fun(maxDBH, cvDBH, LAI_max, MaxShrubCover, TreeRichness, ShrubRichness),
              .groups = "drop") |>
    mutate(Model = model)  |>
    relocate(Model, .after = MidYear) |>
    dplyr::mutate(Climate = toupper(Climate))
  return(ES5)
}

# ES6 - Fire risk per 20-year periods -----------
ES6_function <- function(ALL, model) {
  if(model=="FORMES") {
    ALL$Management <- ALL$Management[ALL$Year=="2021-2030"][1]
    ALL$Year[ALL$Year=="2001-2010"] = "2005"
    ALL$Year[ALL$Year=="2011-2020"] = "2015"
    ALL$Year[ALL$Year=="2021-2030"] = "2025"
    ALL$Year[ALL$Year=="2031-2040"] = "2035"
    ALL$Year[ALL$Year=="2041-2050"] = "2045"
    ALL$Year[ALL$Year=="2051-2060"] = "2055"
    ALL$Year[ALL$Year=="2061-2070"] = "2065"
    ALL$Year[ALL$Year=="2071-2080"] = "2075"
    ALL$Year[ALL$Year=="2081-2090"] = "2085"
    ALL$Year[ALL$Year=="2091-2100"] = "2095"
    ALL$Year <- as.numeric(ALL$Year)
  } 
  ES6 <- ALL |>
    filter(Year!=2000) |>
    select(Climate, Management, Province, id, Year, SFP, CFP) |>
    mutate(Period = as.character(cut(Year, breaks = c(2000,2020,2040,2060,2080,2100), labels = c("2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100"))),
           MidYear = as.numeric(as.character(cut(Year, breaks = c(2000,2020,2040,2060,2080,2100), labels = c(2010, 2030, 2050, 2070, 2090))))) |>
    group_by(Climate, Management, Province, id, Period, MidYear) |>
    summarise(ES6_SurfaceFirePotential = mean(SFP, na.rm=TRUE),
              ES6_CrownFirePotential = mean(CFP, na.rm=TRUE),
              .groups = "drop") |>
    tidyr::replace_na(list(ES6_SurfaceFirePotential = 0, ES6_CrownFirePotential = 0)) |>
    mutate(Model = model) |>
    relocate(Model, .after = MidYear)|>
    dplyr::mutate(Climate = toupper(Climate))
  return(ES6)
}

# ES - ALL ----------------------------------------------------------------
generate_ES_table <- function(test = FALSE, model = "MEDFATE") {
  ES_function<-function(ALL, model) {
    ES1 <- ES1_function(ALL, model)
    ES2 <- ES2_function(ALL, model)
    ES <- ES1 |>
      left_join(ES2, by=c("Climate", "Management", "Province", "id", "Period", "Model"))
    if(model=="MEDFATE") {
      ES3 <- ES3_function(ALL, model)
      ES4 <- ES4_function(ALL, model)
      ES <- ES |>
        left_join(ES3, by=c("Climate", "Management", "Province", "id", "Period", "MidYear","Model"))|>
        left_join(ES4, by=c("Climate", "Management", "Province", "id", "Period", "MidYear","Model"))
      ES5 <- ES5_function(ALL, model)
      ES <- ES |>
        left_join(ES5, by=c("Climate", "Management", "Province", "id", "Period", "MidYear","Model"))
      ES6 <- ES6_function(ALL, model)
      ES <- ES |>
        left_join(ES6, by=c("Climate", "Management", "Province", "id", "Period", "MidYear","Model"))
    }
    return(ES)
  }
  if(model=="MEDFATE") {
    if(test) {
      cli::cli_progress_step("BAU/RCP45")
      BAU_rcp45 <- ES_function(readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_BAU_mpiesm_rca4_rcp45.rds"), model)
      cli::cli_progress_step("BAU/RCP85")
      BAU_rcp85 <- ES_function(readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_BAU_mpiesm_rca4_rcp85.rds"), model)
      cli::cli_progress_step("AMF/RCP45")
      AMF_rcp45 <- ES_function(readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_AMF_mpiesm_rca4_rcp45.rds"), model)
      cli::cli_progress_step("AMF/RCP85")
      AMF_rcp85 <- ES_function(readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_AMF_mpiesm_rca4_rcp85.rds"), model)
      cli::cli_progress_step("RSB/RCP45")
      RSB_rcp45 <- ES_function(readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_RSB_mpiesm_rca4_rcp45.rds"), model)
      cli::cli_progress_step("RSB/RCP85")
      RSB_rcp85 <- ES_function(readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_RSB_mpiesm_rca4_rcp85.rds"), model)
      cli::cli_progress_step("ASEA/RCP45")
      ASEA_rcp45 <- ES_function(readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_ASEA_mpiesm_rca4_rcp45.rds"), model)
      cli::cli_progress_step("ASEA/RCP85")
      ASEA_rcp85 <- ES_function(readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_ASEA_mpiesm_rca4_rcp85.rds"), model)
      cli::cli_progress_step("ACG/RCP45")
      ACG_rcp45 <- ES_function(readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_ACG_mpiesm_rca4_rcp45.rds"), model)
      cli::cli_progress_step("ACG/RCP85")
      ACG_rcp85 <- ES_function(readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_ACG_mpiesm_rca4_rcp85.rds"), model)
      # cli::cli_progress_step("NOG/RCP45")
      # NOG_rcp45 <- ES_function(readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_NOG_mpiesm_rca4_rcp45.rds"), model)
      # cli::cli_progress_step("NOG/RCP85")
      # NOG_rcp85 <- ES_function(readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_NOG_mpiesm_rca4_rcp85.rds"), model)
    } else {
      cli::cli_progress_step("BAU/RCP45")
      BAU_rcp45 <- ES_function(readRDS("Rdata/MEDFATE/annual_indicators/BAU_mpiesm_rca4_rcp45.rds"), model)
      cli::cli_progress_step("BAU/RCP85")
      BAU_rcp85 <- ES_function(readRDS("Rdata/MEDFATE/annual_indicators/BAU_mpiesm_rca4_rcp85.rds"), model)
      cli::cli_progress_step("AMF/RCP45")
      AMF_rcp45 <- ES_function(readRDS("Rdata/MEDFATE/annual_indicators/AMF_mpiesm_rca4_rcp45.rds"), model)
      cli::cli_progress_step("AMF/RCP85")
      AMF_rcp85 <- ES_function(readRDS("Rdata/MEDFATE/annual_indicators/AMF_mpiesm_rca4_rcp85.rds"), model)
      cli::cli_progress_step("RSB/RCP45")
      RSB_rcp45 <- ES_function(readRDS("Rdata/MEDFATE/annual_indicators/RSB_mpiesm_rca4_rcp45.rds"), model)
      cli::cli_progress_step("RSB/RCP85")
      RSB_rcp85 <- ES_function(readRDS("Rdata/MEDFATE/annual_indicators/RSB_mpiesm_rca4_rcp85.rds"), model)
      cli::cli_progress_step("ASEA/RCP45")
      ASEA_rcp45 <- ES_function(readRDS("Rdata/MEDFATE/annual_indicators/ASEA_mpiesm_rca4_rcp45.rds"), model)
      cli::cli_progress_step("ASEA/RCP85")
      ASEA_rcp85 <- ES_function(readRDS("Rdata/MEDFATE/annual_indicators/ASEA_mpiesm_rca4_rcp85.rds"), model)
      cli::cli_progress_step("ACG/RCP45")
      ACG_rcp45 <- ES_function(readRDS("Rdata/MEDFATE/annual_indicators/ACG_mpiesm_rca4_rcp45.rds"), model)
      cli::cli_progress_step("ACG/RCP85")
      ACG_rcp85 <- ES_function(readRDS("Rdata/MEDFATE/annual_indicators/ACG_mpiesm_rca4_rcp85.rds"), model)
      # cli::cli_progress_step("NOG/RCP45")
      # NOG_rcp45 <- ES_function(readRDS("Rdata/MEDFATE/annual_indicators/NOG_mpiesm_rca4_rcp45.rds"), model)
      # cli::cli_progress_step("NOG/RCP85")
      # NOG_rcp85 <- ES_function(readRDS("Rdata/MEDFATE/annual_indicators/NOG_mpiesm_rca4_rcp85.rds"), model)
    }
  } else {
    cli::cli_progress_step("BAU/RCP45")
    BAU_rcp45 <- ES_function(readRDS("Rdata/FORMES/annual_indicators/BAU_mpiesm_rca4_rcp45.rds"), model)
    cli::cli_progress_step("BAU/RCP85")
    BAU_rcp85 <- ES_function(readRDS("Rdata/FORMES/annual_indicators/BAU_mpiesm_rca4_rcp85.rds"), model)
    cli::cli_progress_step("AMF/RCP45")
    AMF_rcp45 <- ES_function(readRDS("Rdata/FORMES/annual_indicators/AMF_mpiesm_rca4_rcp45.rds"), model)
    cli::cli_progress_step("AMF/RCP85")
    AMF_rcp85 <- ES_function(readRDS("Rdata/FORMES/annual_indicators/AMF_mpiesm_rca4_rcp85.rds"), model)
    cli::cli_progress_step("RSB/RCP45")
    RSB_rcp45 <- ES_function(readRDS("Rdata/FORMES/annual_indicators/RSB_mpiesm_rca4_rcp45.rds"), model)
    cli::cli_progress_step("RSB/RCP85")
    RSB_rcp85 <- ES_function(readRDS("Rdata/FORMES/annual_indicators/RSB_mpiesm_rca4_rcp85.rds"), model)
    cli::cli_progress_step("ASEA/RCP45")
    ASEA_rcp45 <- ES_function(readRDS("Rdata/FORMES/annual_indicators/ASEA_mpiesm_rca4_rcp45.rds"), model)
    cli::cli_progress_step("ASEA/RCP85")
    ASEA_rcp85 <- ES_function(readRDS("Rdata/FORMES/annual_indicators/ASEA_mpiesm_rca4_rcp85.rds"), model)
    cli::cli_progress_step("ACG/RCP45")
    ACG_rcp45 <- ES_function(readRDS("Rdata/FORMES/annual_indicators/ACG_mpiesm_rca4_rcp45.rds"), model)
    cli::cli_progress_step("ACG/RCP85")
    ACG_rcp85 <- ES_function(readRDS("Rdata/FORMES/annual_indicators/ACG_mpiesm_rca4_rcp85.rds"), model)
    # cli::cli_progress_step("NOG/RCP45")
    # NOG_rcp45 <- ES_function(readRDS("Rdata/FORMES/annual_indicators/NOG_mpiesm_rca4_rcp45.rds"), model)
    # cli::cli_progress_step("NOG/RCP85")
    # NOG_rcp85 <- ES_function(readRDS("Rdata/FORMES/annual_indicators/NOG_mpiesm_rca4_rcp85.rds"), model)
  }
  
  cli::cli_progress_step("Binding")
  ALL <- bind_rows(BAU_rcp45, BAU_rcp85,
                   AMF_rcp45, AMF_rcp85,
                   RSB_rcp45, RSB_rcp85,
                   ASEA_rcp45, ASEA_rcp85,
                   ACG_rcp45, ACG_rcp85)
                   # NOG_rcp45, NOG_rcp85)
  return(ALL)
}

test <-FALSE
ES_ALL_MEDFATE <- generate_ES_table(test, model = "MEDFATE")
ES_ALL_MEDFATE_sf <- ES_ALL_MEDFATE |>
  left_join(nfiplot[,c("id")], by="id") |>
  sf::st_as_sf()
saveRDS(ES_ALL_MEDFATE_sf, "Rdata/ES_MEDFATE.rds")
# saveRDS(ES_ALL_MEDFATE_sf, "Rdata/ES_MEDFATE_test.rds")

ES_ALL_FORMES <- generate_ES_table(FALSE, model = "FORMES")
nfiplot_formes <- nfiplot |>
  mutate(id = as.character(as.numeric(IDPARCELA)))
ES_ALL_FORMES_sf <- ES_ALL_FORMES |>
  left_join(nfiplot_formes[,c("id")], by="id") |>
  sf::st_as_sf()
saveRDS(ES_ALL_FORMES_sf, "Rdata/ES_FORMES.rds")

# Bind and save
ES_ALL_MEDFATE_sf <- readRDS("Rdata/ES_MEDFATE.rds")
ES_ALL_FORMES_sf <- readRDS("Rdata/ES_FORMES.rds")
ES_ALL_sf <- dplyr::bind_rows(ES_ALL_MEDFATE_sf,
                           ES_ALL_FORMES_sf)
saveRDS(ES_ALL_sf, "Rdata/ES_ALL.rds")

ES_ALL_sf  = readRDS("Rdata/ES_ALL.rds")
ES_ALL <- sf::st_drop_geometry(ES_ALL_sf)


# Plots/Map/Table functions  -------------------------------------------------------------------

plot_ES <- function(ES_all, var, ylab, ylim, add_formes = TRUE) {
  ES_sum <- ES_all |>
    filter(!(Management %in% c("NOG", "NOGEST"))) |>
    group_by(Climate, Management, Period, MidYear, Model) |>
    summarise(ES = mean({{var}}, na.rm=TRUE), 
              ES_se = sd({{var}}, na.rm=TRUE)/sqrt(n()),
              ES_q25 = quantile({{var}}, probs=0.25, na.rm = TRUE),
              ES_q75 = quantile({{var}}, probs=0.75, na.rm = TRUE),
              .groups = "drop")
    
  p1<-ggplot(ES_sum[ES_sum$Climate=="RCP45" & ES_sum$Model =="MEDFATE",])+
    geom_point(aes(x = MidYear, y = ES, col = Management))+
    geom_ribbon(aes(x = MidYear, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
    geom_line(aes(x = MidYear, y = ES, col = Management))+
    scale_x_continuous("",breaks = unique(ES_sum$MidYear),
                       labels = unique(ES_sum$Period))+
    scale_color_discrete("Escenari")+
    ylab(ylab)+ ylim(ylim)+labs(title = "MEDFATE / RCP 4.5")+theme_bw()
  l <- get_legend(p1)
  p2<-ggplot(ES_sum[ES_sum$Climate=="RCP85" & ES_sum$Model =="MEDFATE",])+
    geom_point(aes(x = MidYear, y = ES, col = Management))+
    geom_ribbon(aes(x = MidYear, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
    geom_line(aes(x = MidYear, y = ES, col = Management))+
    scale_x_continuous("",breaks = unique(ES_sum$MidYear),
                       labels = unique(ES_sum$Period))+
    ylab("")+ ylim(ylim)+labs(title = "MEDFATE / RCP 8.5")+ theme_bw()
  pA <- plot_grid(p1 + theme(legend.position = "none"), 
                  p2+ theme(legend.position = "none"), nrow = 1)
  if(add_formes) {
    p3<-ggplot(ES_sum[ES_sum$Climate=="RCP45" & ES_sum$Model =="FORMES",])+
      geom_point(aes(x = MidYear, y = ES, col = Management))+
      geom_ribbon(aes(x = MidYear, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
      geom_line(aes(x = MidYear, y = ES, col = Management))+
      scale_x_continuous("",breaks = unique(ES_sum$MidYear),
                         labels = unique(ES_sum$Period))+
      ylab(ylab)+ ylim(ylim)+labs(title = "FORMES / RCP 4.5")+theme_bw()
    l <- get_legend(p1)
    p4<-ggplot(ES_sum[ES_sum$Climate=="RCP85"& ES_sum$Model =="FORMES",])+
      geom_point(aes(x = MidYear, y = ES, col = Management))+
      geom_ribbon(aes(x = MidYear, ymin = ES - ES_se*1.96, ymax = ES + ES_se*1.96, fill = Management), alpha = 0.3)+
      geom_line(aes(x = MidYear, y = ES, col = Management))+
      scale_x_continuous("",breaks = unique(ES_sum$MidYear),
                         labels = unique(ES_sum$Period))+
      ylab("")+ ylim(ylim)+labs(title = "FORMES / RCP 8.5")+ theme_bw()
    pB <- plot_grid(p3 + theme(legend.position = "none"), 
                    p4+ theme(legend.position = "none"), nrow = 1)
    pALL <- plot_grid(pA,pB, nrow = 2)
  } else {
    pALL <- pA
  }
  return(plot_grid(pALL, l, rel_widths = c(1,0.2)))
}

table_ES <- function(ES_all, var) {
 ES_sum <- ES_all |>
    filter(!(Management %in% c("NOG", "NOGEST"))) |>
    group_by(Climate, Management, Period, MidYear, Model) |>
    summarise(ES = mean({{var}}, na.rm=TRUE), .groups = "drop") |>
    select(-MidYear) |>
    filter(Period %in% unique(ES_ALL$Period)[c(1,3,5)]) |>
    pivot_wider(names_from = c(Climate,Period), names_sort = TRUE, values_from = ES) |>
    arrange(Management,Model)
 return(ES_sum)
}

sf::st_bbox(nfiplot)
xmin <- 260000
xmax <- 520000
ymin <- 4495000
ymax <- 4750000

map_scenario<-function(sf_ALL, var, climate_scen, breaks, breaks_diff, units, type = "div", palette = "YlGnBu", res = 5000) {
  r <-terra::rast(resolution  = c(res,res), xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)
  limits <- c(min(breaks), max(breaks))
  limits_diff <- c(min(breaks_diff), max(breaks_diff))
  leg_pos <- c(0.85,0.2)
  
  sf_1 <- sf_ALL |>
    filter(Period == "2001-2020", Climate == climate_scen, Management == "BAU") |>
    select(Management, all_of(var)) |>
    pivot_wider(names_from = "Management", values_from = var)

  sf_2 <- sf_ALL |>
    filter(Period == "2041-2060", Climate == climate_scen) |>
    select(Management, all_of(var)) |>
    pivot_wider(names_from = "Management", values_from = var) |>
    mutate(diff_AMF = AMF - BAU,
           diff_RSB = RSB - BAU,
           diff_ASEA = ASEA - BAU,
           diff_ACG = ACG - BAU) 

  sf_3 <- sf_ALL |>
    filter(Period == "2081-2100", Climate == climate_scen) |>
    select(Management, all_of(var)) |>
    pivot_wider(names_from = "Management", values_from = var) |>
    mutate(diff_AMF = AMF - BAU,
           diff_RSB = RSB - BAU,
           diff_ASEA = ASEA - BAU,
           diff_ACG = ACG - BAU)

  raster_01<-terra::rasterize(terra::vect(sf_1),r, "BAU", fun = mean, na.rm = TRUE)
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "BAU", fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "BAU", fun = mean, na.rm = TRUE)
  names(raster_01) <- "m1"
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g1<-ggplot()+
    geom_spatraster(aes(fill=m1), data = raster_01)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, palette = palette, 
                         breaks = breaks, limits = limits, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "BAU", subtitle = "2011")+
    theme_bw()
  g2<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, palette = palette, 
                         breaks = breaks, limits = limits, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2051")+
    theme_bw()
  g3<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, palette = palette, 
                         breaks = breaks, limits = limits, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2091")+
    theme_bw()
  ## ASEA
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "diff_ASEA", fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "diff_ASEA", fun = mean, na.rm = TRUE)
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g22<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff, limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "ASEA Diff.", subtitle = "2051")+
    theme_bw()
  g23<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2091")+
    theme_bw()
  ## ACG
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "diff_ACG", fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "diff_ACG", fun = mean, na.rm = TRUE)
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g32<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff, limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "ACG Diff.", subtitle = "2051")+
    theme_bw()
  g33<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2091")+
    theme_bw()
  ## AMF
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "diff_AMF", fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "diff_AMF", fun = mean, na.rm = TRUE)
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g42<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff, limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "AMF Diff.", subtitle = "2051")+
    theme_bw()
  g43<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2091")+
    theme_bw()
  ## RSB
  raster_02<-terra::rasterize(terra::vect(sf_2),r,  "diff_RSB", fun = mean, na.rm = TRUE)
  raster_03<-terra::rasterize(terra::vect(sf_3),r,  "diff_RSB", fun = mean, na.rm = TRUE)
  names(raster_02) <- "m2"
  names(raster_03) <- "m3"
  g52<-ggplot()+
    geom_spatraster(aes(fill=m2), data = raster_02)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff, limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "RSB Diff.", subtitle = "2051")+
    theme_bw()
  g53<-ggplot()+
    geom_spatraster(aes(fill=m3), data = raster_03)+
    geom_sf(col = "black", fill = NA, data = cat)+
    scale_fill_fermenter(type=type, breaks = breaks_diff,  limits = limits_diff, na.value=NA,
                         direction = 1)+
    labs(fill=units, title = "", subtitle = "2091")+
    theme_bw()
  ll <- plot_grid(get_legend(g1), get_legend(g22), nrow = 1)
  g<-plot_grid(g1+theme(legend.position = "none"),
               g2+theme(legend.position = "none"),
               g3+theme(legend.position =  "none"), 
               ll,
               g22+theme(legend.position = "none"), 
               g23+theme(legend.position = "none"),
               NULL,
               g32+theme(legend.position = "none"), 
               g33+theme(legend.position = "none"),
               NULL,
               g42+theme(legend.position = "none"), 
               g43+theme(legend.position = "none"),
               NULL,
               g52+theme(legend.position = "none"), 
               g53+theme(legend.position = "none"),
               nrow = 5, ncol = 3)
  return(g)
}

# ES1_VolumeStructure -----------------------------------------------------

table_ES(ES_ALL, ES1_VolumeStructure)
d_ES <- plot_ES(ES_ALL, ES1_VolumeStructure, "Stock fusta estructural (m3/ha)", c(0,250), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES1_VolumeStructure.png",d_ES, width = 10, height = 5, bg = "white")
breaks = seq(0,200, by=25)
breaks_diff = c(-100,-50, -25, -5, 5, 25, 50, 100)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES1_VolumeStructure", climate_scen = "RCP45", 
                        breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha")
ggsave2("Plots/ES_maps/ES1_VolumeStructure_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
# m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES1_VolumeStructure", climate_scen = "RCP45", 
#                      breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha")
# ggsave2("Plots/ES_maps/ES1_VolumeStructure_formes_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES1_VolumeStructure", climate_scen = "RCP85", 
                        breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha")
ggsave2("Plots/ES_maps/ES1_VolumeStructure_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")
# m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES1_VolumeStructure", climate_scen = "RCP85", 
#                      breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha")
# ggsave2("Plots/ES_maps/ES1_VolumeStructure_formes_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES1_VolumeAdultFirewood -------------------------------------------------
table_ES(ES_ALL, ES1_VolumeAdultFirewood)
d_ES <- plot_ES(ES_ALL, ES1_VolumeAdultFirewood, "Stock llenyes (m3/ha)", c(0,100), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES1_VolumeAdultFirewood.png",d_ES, width = 10, height = 5, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES1_VolumeAdultFirewood)
breaks = seq(0,200, by=25)
breaks_diff = c(-100,-50, -25, -5, 5, 25, 50, 100)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES1_VolumeAdultFirewood", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha")
ggsave2("Plots/ES_maps/ES1_VolumeAdultFirewood_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
# m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES1_VolumeAdultFirewood", climate_scen = "RCP45", 
#                      breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha")
# ggsave2("Plots/ES_maps/ES1_VolumeAdultFirewood_formes_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES1_VolumeAdultFirewood", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha")
ggsave2("Plots/ES_maps/ES1_VolumeAdultFirewood_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")
# m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES1_VolumeAdultFirewood", climate_scen = "RCP85", 
#                      breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha")
# ggsave2("Plots/ES_maps/ES1_VolumeAdultFirewood_formes_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES1_CutStructure --------------------------------------------------------
table_ES(ES_ALL, ES1_CutStructure)
d_ES <- plot_ES(ES_ALL, ES1_CutStructure, "Provisió de fusta estructural (m3/ha/any)", c(0,2.1), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES1_CutStructure.png",d_ES, width = 10, height = 5, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES1_CutStructure)
summary(ES_ALL_FORMES_sf$ES1_CutStructure)
breaks = c(0,0.1,0.5,1,2,5,30)
breaks_diff = c(-30, -5, -2, -1,-0.5, 0.5,1, 2,5, 30)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES1_CutStructure", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha/any")
ggsave2("Plots/ES_maps/ES1_CutStructure_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
# m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES1_CutStructure", climate_scen = "RCP45", 
#                      breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha/any")
# ggsave2("Plots/ES_maps/ES1_CutStructure_formes_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES1_CutStructure", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha/any")
ggsave2("Plots/ES_maps/ES1_CutStructure_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")
# m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES1_CutStructure", climate_scen = "RCP85", 
#                      breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha/any")
# ggsave2("Plots/ES_maps/ES1_CutStructure_formes_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES1_CutAdultFirewood ----------------------------------------------------
table_ES(ES_ALL, ES1_CutAdultFirewood)
d_ES <- plot_ES(ES_ALL, ES1_CutAdultFirewood, ylab= "Provisió de llenyes (m3/ha/any)", ylim = c(0,5), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES1_CutAdultFirewood.png",d_ES, width = 10, height = 5, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES1_CutAdultFirewood)
summary(ES_ALL_FORMES_sf$ES1_CutAdultFirewood)
breaks = c(0,0.1,0.2, 0.5,1,2,5,20)
breaks_diff = c(-20, -5, -2, -1,-0.5, -0.2, 0.2, 0.5, 1, 2,5, 20)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES1_CutAdultFirewood", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha/any")
ggsave2("Plots/ES_maps/ES1_CutAdultFirewood_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
# m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES1_CutAdultFirewood", climate_scen = "RCP45", 
#                      breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha/any")
# ggsave2("Plots/ES_maps/ES1_CutAdultFirewood_formes_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES1_CutAdultFirewood", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha/any")
ggsave2("Plots/ES_maps/ES1_CutAdultFirewood_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")
# m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES1_CutAdultFirewood", climate_scen = "RCP85", 
#                      breaks = breaks, breaks_diff = breaks_diff, units = "m3/ha/any")
# ggsave2("Plots/ES_maps/ES1_CutAdultFirewood_formes_rcp85.png",m_ES, width = 13, height = 22, bg = "white")

# ES2_AdultTreeBiomass --------------------------------------------
table_ES(ES_ALL, ES2_AdultTreeBiomass)
d_ES <- plot_ES(ES_ALL, ES2_AdultTreeBiomass, ylab = "Stock de carboni arbres (Mg C/ha)", ylim = c(0,1000), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES2_AdultTreeBiomass.png",d_ES, width = 10, height = 5, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES2_AdultTreeBiomass)
summary(ES_ALL_FORMES_sf$ES2_AdultTreeBiomass)
breaks = c(0,25,50,100,200, 500, 1000, 3000)
breaks_diff = c(-1000, -500, -200, -100, -50, -25, 25, 50, 100 ,200, 500, 1000)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES2_AdultTreeBiomass", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_AdultTreeBiomass_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
# m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES2_AdultTreeBiomass", climate_scen = "RCP45", 
#                      breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
# ggsave2("Plots/ES_maps/ES2_AdultTreeBiomass_formes_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES2_AdultTreeBiomass", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_AdultTreeBiomass_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")
# m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES2_AdultTreeBiomass", climate_scen = "RCP85", 
#                      breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
# ggsave2("Plots/ES_maps/ES2_AdultTreeBiomass_formes_rcp85.png",m_ES, width = 13, height = 22, bg = "white")

# ES2_AdultTreeBiomassChange --------------------------------------------
table_ES(ES_ALL, ES2_AdultTreeBiomassChange)
d_ES <- plot_ES(ES_ALL, ES2_AdultTreeBiomassChange, ylab = "Embornal de carboni arbres (Mg C/ha/any)", ylim = c(-5,10), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES2_AdultTreeBiomassChange.png",d_ES, width = 10, height = 5, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES2_AdultTreeBiomassChange)
summary(ES_ALL_FORMES_sf$ES2_AdultTreeBiomassChange)
breaks = c(-10,-0.5, 0.5,1,2,5,10,50)
breaks_diff = c(-20, -5, -2, -1,-0.5, -0.2, 0.2, 0.5, 1, 2,5, 20)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES2_AdultTreeBiomassChange", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_AdultTreeBiomassChange_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
# m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES2_AdultTreeBiomassChange", climate_scen = "RCP45", 
#                      breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
# ggsave2("Plots/ES_maps/ES2_AdultTreeBiomassChange_formes_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES2_AdultTreeBiomassChange", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_AdultTreeBiomassChange_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")
# m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES2_AdultTreeBiomassChange", climate_scen = "RCP85", 
#                      breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
# ggsave2("Plots/ES_maps/ES2_AdultTreeBiomassChange_formes_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES2_CutBiomassStructure --------------------------------------------
table_ES(ES_ALL, ES2_CutBiomassStructure)
d_ES <- plot_ES(ES_ALL, ES2_CutBiomassStructure, ylab = "Embornal de carboni fusta estructural (Mg C/ha/any)", ylim = c(-5,10), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES2_CutBiomassStructure.png",d_ES, width = 10, height = 5, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES2_CutBiomassStructure)
summary(ES_ALL_FORMES_sf$ES2_CutBiomassStructure)
breaks = c(-10,-0.5, 0.5,1,2,5,10,50)
breaks_diff = c(-20, -5, -2, -1,-0.5, -0.2, 0.2, 0.5, 1, 2,5, 20)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES2_CutBiomassStructure", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_CutBiomassStructure_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
# m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES2_CutBiomassStructure", climate_scen = "RCP45", 
#                      breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
# ggsave2("Plots/ES_maps/ES2_CutBiomassStructure_formes_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES2_CutBiomassStructure", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_CutBiomassStructure_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")
# m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES2_CutBiomassStructure", climate_scen = "RCP85", 
#                      breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
# ggsave2("Plots/ES_maps/ES2_CutBiomassStructure_formes_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES2_AdultTreeBiomassSequestr --------------------------------------------
table_ES(ES_ALL, ES2_AdultTreeBiomassSequestr)
d_ES <- plot_ES(ES_ALL, ES2_AdultTreeBiomassSequestr, ylab = "Embornal de carboni arbres+fusta (Mg C/ha/any)", ylim = c(-5,10), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES2_AdultTreeBiomassSequestr.png",d_ES, width = 10, height = 5, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES2_AdultTreeBiomassSequestr)
summary(ES_ALL_FORMES_sf$ES2_AdultTreeBiomassSequestr)
breaks = c(-10,-0.5, 0.5,1,2,5,10,50)
breaks_diff = c(-20, -5, -2, -1,-0.5, -0.2, 0.2, 0.5, 1, 2,5, 20)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES2_AdultTreeBiomassSequestr", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_AdultTreeBiomassSequestr_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
# m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES2_AdultTreeBiomassSequestr", climate_scen = "RCP45", 
#                      breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
# ggsave2("Plots/ES_maps/ES2_AdultTreeBiomassSequestr_formes_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES2_AdultTreeBiomassSequestr", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_AdultTreeBiomassSequestr_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")
# m_ES <- map_scenario(ES_ALL_FORMES_sf, var = "ES2_AdultTreeBiomassSequestr", climate_scen = "RCP85", 
#                      breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
# ggsave2("Plots/ES_maps/ES2_AdultTreeBiomassSequestr_formes_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES2_LiveBiomassSequestr --------------------------------------------
table_ES(ES_ALL, ES2_LiveBiomassSequestr)
d_ES <- plot_ES(ES_ALL, ES2_LiveBiomassSequestr, ylab = "Embornal de carboni total (Mg C/ha/any)", ylim = c(-5,10), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES2_LiveBiomassSequestr.png",d_ES, width = 10, height = 5, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES2_LiveBiomassSequestr)
summary(ES_ALL_FORMES_sf$ES2_LiveBiomassSequestr)
breaks = c(-10,-0.5, 0.5,1,2,5,10,50)
breaks_diff = c(-20, -5, -2, -1,-0.5, -0.2, 0.2, 0.5, 1, 2,5, 20)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES2_LiveBiomassSequestr", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_LiveBiomassSequestr_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES2_LiveBiomassSequestr", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "MgC/ha/any")
ggsave2("Plots/ES_maps/ES2_LiveBiomassSequestr_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES3_BlueWater -----------------------------------------------------------
table_ES(ES_ALL, ES3_BlueWater)
d_ES <- plot_ES(ES_ALL, ES3_BlueWater, ylab = "Aigua blava (mm/any)", ylim = c(150,300), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES3_BlueWater.png",d_ES, width = 10, height = 5, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES3_BlueWater)
breaks = seq(0,320, by=40)
breaks_diff = seq(-200,200, by = 40)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES3_BlueWater", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "mm/any")
ggsave2("Plots/ES_maps/ES3_BlueWater_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES3_BlueWater", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "mm/any")
ggsave2("Plots/ES_maps/ES3_BlueWater_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES3_RunoffCoefficient ---------------------------------------------------
table_ES(ES_ALL, ES3_RunoffCoefficient)
d_ES <- plot_ES(ES_ALL, ES3_RunoffCoefficient, ylab = "Coeficient d'escolament [%]", ylim = c(23,45), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES3_RunoffCoefficient.png",d_ES, width = 10, height = 5, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES3_RunoffCoefficient)
breaks = c(0,5,10,20,40,60,80, 100)
breaks_diff = c(-50,-10,-20,-5, -2,2,5,10,20,50)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES3_RunoffCoefficient", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "%")
ggsave2("Plots/ES_maps/ES3_RunoffCoefficient_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES3_RunoffCoefficient", climate_scen = "RCP85", 
             breaks = breaks, breaks_diff = breaks_diff, units = "%")
ggsave2("Plots/ES_maps/ES3_RunoffCoefficient_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES4_ErosionMitigation ---------------------------------------------------
table_ES(ES_ALL, ES4_ErosionMitigation)
d_ES <- plot_ES(ES_ALL, ES4_ErosionMitigation, ylab = "Mitigació de l'erosió (Mg/ha/any)", ylim = c(100,150), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES4_ErosionMitigation.png",d_ES, width = 10, height = 5, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES4_ErosionMitigation)
breaks = c(0,25, 50, 100, 150, 200, 300, 4000)
breaks_diff = c(-100, -50,-10,-5, 5,10,50, 100)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES4_ErosionMitigation", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "Mg/ha/any")
ggsave2("Plots/ES_maps/ES4_ErosionMitigation_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES4_ErosionMitigation", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "Mg/ha/any")
ggsave2("Plots/ES_maps/ES4_ErosionMitigation_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES5_RecreationalValue ---------------------------------------------------
table_ES(ES_ALL, ES5_RecreationalValue)
d_ES <- plot_ES(ES_ALL, ES5_RecreationalValue, ylab = "Valor recreatiu [0-1]", ylim = c(0.4,0.55), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES5_RecreationalValue.png",d_ES, width = 10, height = 5, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES5_RecreationalValue)
breaks = c(0,0.05,0.1,0.2,0.4,0.6,0.8, 1)
breaks_diff = c(-0.5,-0.3, -0.2, -0.1, -0.05, 0.05, 0.1, 0.2, 0.5)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES5_RecreationalValue", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "[0-1]")
ggsave2("Plots/ES_maps/ES5_RecreationalValue_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES5_RecreationalValue", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "[0-1]")
ggsave2("Plots/ES_maps/ES5_RecreationalValue_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


# ES6_SurfaceFirePotential  ---------------------------------------------------
table_ES(ES_ALL, ES6_SurfaceFirePotential)
d_ES <- plot_ES(ES_ALL, ES6_SurfaceFirePotential, ylab = "Risk d'incendi de superficie [0-9]", ylim = c(7,9), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES6_SurfaceFirePotential.png",d_ES, width = 10, height = 5, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES6_SurfaceFirePotential)
breaks = seq(0,9, by=1)
breaks_diff = c(-5,-4,-3,-2,-1,1,2,3,4,5)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES6_SurfaceFirePotential", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "[0-1]")
ggsave2("Plots/ES_maps/ES6_SurfaceFirePotential_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES6_SurfaceFirePotential", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "[0-1]")
ggsave2("Plots/ES_maps/ES6_SurfaceFirePotential_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")

# ES6_CrownFirePotential  ---------------------------------------------------
table_ES(ES_ALL, ES6_CrownFirePotential)
d_ES <- plot_ES(ES_ALL, ES6_CrownFirePotential, ylab = "Risk d'incendi de capçada [0-9]", ylim = c(4,7), add_formes = FALSE)
ggsave2("Plots/ES_dynamics/ES6_CrownFirePotential.png",d_ES, width = 10, height = 5, bg = "white")
summary(ES_ALL_MEDFATE_sf$ES6_CrownFirePotential)
breaks = seq(0,9, by=1)
breaks_diff = c(-5,-4,-3,-2,-1,1,2,3,4,5)
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES6_CrownFirePotential", climate_scen = "RCP45", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "[0-1]")
ggsave2("Plots/ES_maps/ES6_CrownFirePotential_medfate_rcp45.png",m_ES, width = 13, height = 22, bg = "white")
m_ES <- map_scenario(ES_ALL_MEDFATE_sf, var = "ES6_CrownFirePotential", climate_scen = "RCP85", 
                     breaks = breaks, breaks_diff = breaks_diff, units = "[0-1]")
ggsave2("Plots/ES_maps/ES6_CrownFirePotential_medfate_rcp85.png",m_ES, width = 13, height = 22, bg = "white")


