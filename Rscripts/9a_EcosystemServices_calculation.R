library(tidyverse)
library(tidyterra)
library(medfate)
library(sf)


# Load spatial data -------------------------------------------------------
nfiplot <- dplyr::bind_rows(readRDS(paste0("Rdata/nfiplot.rds")))
lonlat <- sf::st_transform(nfiplot[,"id"], crs = 4326)
lonlat$longitude <- sf::st_coordinates(lonlat)[,1]
longitude_df <- sf::st_drop_geometry(lonlat)
K_LS <- sf::read_sf("Data/ErosionData.gpkg")
comarques <- sf::read_sf("Data/Comarques/comarques.shp")

# ES1 - Annual timber extraction (m3/ha/yr) per 10-year and 20-year periods -----------
ES1_function_period <- function(ALL, model) {
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
  ALL_COMPLETE <- ALL |>
    ungroup() |>
    filter(Year!=2000) |>
    select(Climate, Management, Province, id, Year, CutStructure, CutAdultFirewood, CutSaplingFirewood) |>
    group_by(Climate, Management, Province) |>
    complete(id, Year, fill = list(CutStructure = 0, CutAdultFirewood =0, CutSaplingFirewood = 0))
    
  ES1_20 <- ALL_COMPLETE |>
    mutate(Period = as.character(cut(Year, breaks = c(2000,2020,2040,2060,2080,2100), labels = c("2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100"))),
           MidYear = as.numeric(as.character(cut(Year, breaks = c(2000,2020,2040,2060,2080,2100), 
                                                 labels = c(2010, 2030, 2050, 2070, 2090))))) |>
    group_by(Climate, Management, Province, id, Period, MidYear) |>
    summarise(ES1_CutStructure = mean(CutStructure, na.rm=TRUE),
              ES1_CutAdultFirewood = mean(CutAdultFirewood, na.rm=TRUE),
              ES1_CutSaplingFirewood = mean(CutSaplingFirewood, na.rm=TRUE),
              .groups = "drop") |>
    tidyr::replace_na(list(ES1_CutStructure = 0, ES1_CutAdultFirewood = 0, ES1_CutSaplingFirewood = 0)) |>
    mutate(Model = model) |>
    relocate(Model, .after = MidYear)|>
    dplyr::mutate(Climate = toupper(Climate))
  ES1_10 <- ALL_COMPLETE |>
    mutate(Period = as.character(cut(Year, breaks = seq(2000,2100, by = 10), 
                                     labels = paste0(seq(2001,2091, by=10), "-",seq(2010,2100, by=10)))),
           MidYear = as.numeric(as.character(cut(Year, breaks = seq(2000,2100, by = 10), 
                                                 labels = seq(2005, 2095, by=10))))) |>
    group_by(Climate, Management, Province, id, Period, MidYear) |>
    summarise(ES1_CutStructure = mean(CutStructure, na.rm=TRUE),
              ES1_CutAdultFirewood = mean(CutAdultFirewood, na.rm=TRUE),
              ES1_CutSaplingFirewood = mean(CutSaplingFirewood, na.rm=TRUE),
              .groups = "drop") |>
    tidyr::replace_na(list(ES1_CutStructure = 0, ES1_CutAdultFirewood = 0, ES1_CutSaplingFirewood = 0)) |>
    mutate(Model = model) |>
    relocate(Model, .after = MidYear)|>
    dplyr::mutate(Climate = toupper(Climate))
  ES1 <- bind_rows(ES1_10, ES1_20)
  return(ES1)
}
# ES1 - Timber stocks (m3/ha/yr) every 10 years -----------
ES1_function_state <- function(ALL, model) {
  if(model=="FORMES") {
    ALL$Management <- ALL$Management[ALL$Year=="2021-2030"][1]
    ALL$Year[ALL$Year=="2001-2010"] = "2010"
    ALL$Year[ALL$Year=="2011-2020"] = "2020"
    ALL$Year[ALL$Year=="2021-2030"] = "2030"
    ALL$Year[ALL$Year=="2031-2040"] = "2040"
    ALL$Year[ALL$Year=="2041-2050"] = "2050"
    ALL$Year[ALL$Year=="2051-2060"] = "2060"
    ALL$Year[ALL$Year=="2061-2070"] = "2070"
    ALL$Year[ALL$Year=="2071-2080"] = "2080"
    ALL$Year[ALL$Year=="2081-2090"] = "2090"
    ALL$Year[ALL$Year=="2091-2100"] = "2100"
    ALL$Year <- as.numeric(ALL$Year)
  } 
  ES1 <- ALL |>
    ungroup() |>
    select(Climate, Management, Province, id, Year, VolumeStructure, VolumeAdultFirewood, VolumeSaplingFirewood) |>
    group_by(Climate, Management, Province) |>
    complete(id, Year, fill = list(VolumeStructure = 0, VolumeAdultFirewood = 0, VolumeSaplingFirewood = 0))|>
    filter(Year %in% c(seq(2000,2100, by =10))) |>
    rename(ES1_VolumeStructure = VolumeStructure,
           ES1_VolumeAdultFirewood = VolumeAdultFirewood,
           ES1_VolumeSaplingFirewood = VolumeSaplingFirewood) |>
    tidyr::replace_na(list(ES1_VolumeStructure = 0, ES1_VolumeAdultFirewood = 0, ES1_VolumeSaplingFirewood = 0)) |>
    mutate(Model = model) |>
    relocate(Model, .after = Year)|>
    dplyr::mutate(Climate = toupper(Climate))
  return(ES1)
}

# ES2 - Carbon sequestration rate (Mg C/ha/yr) per 20-year periods -----------
ES2_function_period <- function(ALL, model) {
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
  
  diff_20 <- function(x) {
    x |>
      mutate(P1 = `2020` - `2000`,
             P2 = `2040` - `2020`,
             P3 = `2060` - `2040`,
             P4 = `2080` - `2060`,
             P5 = `2100` - `2080`)
  }
  diff_10 <- function(x) {
    x |>
      mutate(P1 = `2010` - `2000`,
             P2 = `2020` - `2010`,
             P3 = `2030` - `2020`,
             P4 = `2040` - `2030`,
             P5 = `2050` - `2040`,
             P6 = `2060` - `2050`,
             P7 = `2070` - `2060`,
             P8 = `2080` - `2070`,
             P9 = `2090` - `2080`,
             P10 = `2100` - `2090`)
  }
  
  BATC_20 <-ALL1 |>
    ungroup() |>
    filter(Year %in% seq(2000,2100, by=20)) |>
    select(Climate, Management, Province, id, Year, AdultTreeBiomass) |>
    group_by(Climate, Management, Province) |>
    complete(id, Year, fill = list(AdultTreeBiomass = 0))|>
    pivot_wider(values_from = AdultTreeBiomass, names_from = Year) |>
    diff_20() |>
    select(Climate, Management, Province, id, "P1", "P2", "P3", "P4", "P5") |>
    pivot_longer(cols = 5:9, names_to = "Period", values_to = "AdultTreeBiomassChange") |>
    mutate(AdultTreeBiomassChange = AdultTreeBiomassChange/20) |>
    ungroup()
  
  BATC_10 <-ALL1 |>
    ungroup() |>
    filter(Year %in% seq(2000,2100, by=10)) |>
    select(Climate, Management, Province, id, Year, AdultTreeBiomass) |>
    group_by(Climate, Management, Province) |>
    complete(id, Year, fill = list(AdultTreeBiomass = 0))|>
    pivot_wider(values_from = AdultTreeBiomass, names_from = Year) |>
    diff_10() |>
    select(Climate, Management, Province, id, "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10") |>
    pivot_longer(cols = 5:14, names_to = "Period", values_to = "AdultTreeBiomassChange") |>
    mutate(AdultTreeBiomassChange = AdultTreeBiomassChange/20) |>
    ungroup()
  
  BSTC_20<-ALL1 |>
    ungroup() |>
    filter(Year %in% seq(2000,2100, by=20)) |>
    select(Climate, Management, Province, id, Year, SaplingTreeBiomass) |>
    group_by(Climate, Management, Province) |>
    complete(id, Year, fill = list(SaplingTreeBiomass = 0))|>
    pivot_wider(values_from = SaplingTreeBiomass, names_from = Year) |>
    diff_20() |>
    select(Climate, Management, Province, id, "P1", "P2", "P3", "P4", "P5") |>
    pivot_longer(cols = 5:9, names_to = "Period", values_to = "SaplingTreeBiomassChange") |>
    mutate(SaplingTreeBiomassChange = SaplingTreeBiomassChange/20) |>
    ungroup()

  BSTC_10<-ALL1 |>
    ungroup() |>
    filter(Year %in% seq(2000,2100, by=10)) |>
    select(Climate, Management, Province, id, Year, SaplingTreeBiomass) |>
    group_by(Climate, Management, Province) |>
    complete(id, Year, fill = list(SaplingTreeBiomass = 0))|>
    pivot_wider(values_from = SaplingTreeBiomass, names_from = Year) |>
    diff_10() |>
    select(Climate, Management, Province, id, "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10") |>
    pivot_longer(cols = 5:14, names_to = "Period", values_to = "SaplingTreeBiomassChange") |>
    mutate(SaplingTreeBiomassChange = SaplingTreeBiomassChange/20) |>
    ungroup()
  
  BSC_20 <-ALL1 |>
    ungroup() |>
    filter(Year %in% seq(2000,2100, by=20)) |>
    select(Climate, Management, Province, id, Year, ShrubBiomass) |>
    group_by(Climate, Management, Province) |>
    complete(id, Year, fill = list(ShrubBiomass = 0))|>
    pivot_wider(values_from = ShrubBiomass, names_from = Year) |>
    diff_20() |>
    select(Climate, Management, Province, id, P1, P2, P3, P4, P5) |>
    pivot_longer(cols = 5:9, names_to = "Period", values_to = "ShrubBiomassChange")|>
    mutate(ShrubBiomassChange = ShrubBiomassChange/20) |>
    ungroup()
  
  
  BSC_10 <-ALL1 |>
    ungroup() |>
    filter(Year %in% seq(2000,2100, by=10)) |>
    select(Climate, Management, Province, id, Year, ShrubBiomass) |>
    group_by(Climate, Management, Province) |>
    complete(id, Year, fill = list(ShrubBiomass = 0))|>
    pivot_wider(values_from = ShrubBiomass, names_from = Year) |>
    diff_10() |>
    select(Climate, Management, Province, id, "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10") |>
    pivot_longer(cols = 5:14, names_to = "Period", values_to = "ShrubBiomassChange")|>
    mutate(ShrubBiomassChange = ShrubBiomassChange/20) |>
    ungroup()

  CutBiomass_20 <- ALL1 |>
    ungroup() |>
    filter(Year!=2000) |>
    select(Climate, Management, Province, id, Year, CutBiomassAdultTree, CutBiomassSaplingTree, CutBiomassShrub,
           CutBiomassStructure, CutBiomassAdultFirewood) |>
    group_by(Climate, Management, Province) |>
    complete(id, Year, fill = list(CutBiomassAdultTree = 0, CutBiomassSaplingTree = 0, CutBiomassShrub = 0,
                                   CutBiomassStructure = 0, CutBiomassAdultFirewood = 0))|>
    mutate(Period = as.character(cut(Year, 
                                     breaks = seq(2000,2100, by=20), 
                                     labels = c("P1", "P2", "P3", "P4", "P5")))) |>
    group_by(Climate, Management, Province, id, Period) |>
    summarise(CutBiomassAdultTree = mean(CutBiomassAdultTree, na.rm=TRUE),
              CutBiomassSaplingTree = mean(CutBiomassSaplingTree, na.rm=TRUE),
              CutBiomassShrub = mean(CutBiomassShrub, na.rm=TRUE),
              CutBiomassStructure = mean(CutBiomassStructure, na.rm=TRUE),
              CutBiomassAdultFirewood = mean(CutBiomassAdultFirewood, na.rm=TRUE),
              .groups = "drop") 

  CutBiomass_10 <- ALL1 |>
    ungroup() |>
    filter(Year!=2000) |>
    select(Climate, Management, Province, id, Year, CutBiomassAdultTree, CutBiomassSaplingTree, CutBiomassShrub,
           CutBiomassStructure, CutBiomassAdultFirewood) |>
    group_by(Climate, Management, Province) |>
    complete(id, Year, fill = list(CutBiomassAdultTree = 0, CutBiomassSaplingTree = 0, CutBiomassShrub = 0,
                                   CutBiomassStructure = 0, CutBiomassAdultFirewood = 0))|>
    mutate(Period = as.character(cut(Year, 
                                     breaks = seq(2000,2100, by=10), 
                                     labels = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10")))) |>
    group_by(Climate, Management, Province, id, Period) |>
    summarise(CutBiomassAdultTree = mean(CutBiomassAdultTree, na.rm=TRUE),
              CutBiomassSaplingTree = mean(CutBiomassSaplingTree, na.rm=TRUE),
              CutBiomassShrub = mean(CutBiomassShrub, na.rm=TRUE),
              CutBiomassStructure = mean(CutBiomassStructure, na.rm=TRUE),
              CutBiomassAdultFirewood = mean(CutBiomassAdultFirewood, na.rm=TRUE),
              .groups = "drop") 
  
  ES2_20 <- CutBiomass_20 |>
    left_join(BATC_20, by=c("Climate", "Management", "Province", "id", "Period")) |>
    left_join(BSTC_20, by=c("Climate", "Management", "Province", "id", "Period")) |>
    left_join(BSC_20, by=c("Climate", "Management", "Province", "id", "Period")) |>
    tidyr::replace_na(list(AdultTreeBiomassChange = 0, CutBiomassAdultTree = 0, CutBiomassStructure = 0, CutBiomassAdultFirewood = 0, 
                           SaplingTreeBiomassChange = 0, CutBiomassSaplingTree = 0, 
                           ShrubBiomassChange = 0, CutBiomassShrub = 0)) |>
    mutate(ES2_AdultTreeBiomassChange = AdultTreeBiomassChange,
           ES2_CutBiomassStructure = CutBiomassStructure,
           ES2_AdultTreeBiomassSequestr = AdultTreeBiomassChange + CutBiomassStructure, ## Assumes only trees cut for structure involve C sequestration
           ES2_SaplingTreeBiomassSequestr = SaplingTreeBiomassChange,
           ES2_ShrubBiomassSequestr = ShrubBiomassChange,
           ES2_LiveBiomassSequestr = ES2_AdultTreeBiomassSequestr+ES2_SaplingTreeBiomassSequestr+ES2_ShrubBiomassSequestr) |>
    select(-c(6:13))

  ES2_20$Period[ES2_20$Period=="P1"] <- "2001-2020"
  ES2_20$Period[ES2_20$Period=="P2"] <- "2021-2040"
  ES2_20$Period[ES2_20$Period=="P3"] <- "2041-2060"
  ES2_20$Period[ES2_20$Period=="P4"] <- "2061-2080"
  ES2_20$Period[ES2_20$Period=="P5"] <- "2081-2100"
  
  ES2_20 <- ES2_20 |> 
    dplyr::mutate(Climate = toupper(Climate))|>
    mutate(Model = model) |>
    relocate(Model, .after = Period)
  
  ES2_10 <- CutBiomass_10 |>
    left_join(BATC_10, by=c("Climate", "Management", "Province", "id", "Period")) |>
    left_join(BSTC_10, by=c("Climate", "Management", "Province", "id", "Period")) |>
    left_join(BSC_10, by=c("Climate", "Management", "Province", "id", "Period")) |>
    tidyr::replace_na(list(AdultTreeBiomassChange = 0, CutBiomassAdultTree = 0, CutBiomassStructure = 0, CutBiomassAdultFirewood = 0, 
                           SaplingTreeBiomassChange = 0, CutBiomassSaplingTree = 0, 
                           ShrubBiomassChange = 0, CutBiomassShrub = 0)) |>
    mutate(ES2_AdultTreeBiomassChange = AdultTreeBiomassChange,
           ES2_CutBiomassStructure = CutBiomassStructure,
           ES2_AdultTreeBiomassSequestr = AdultTreeBiomassChange + CutBiomassStructure, ## Assumes only trees cut for structure involve C sequestration
           ES2_SaplingTreeBiomassSequestr = SaplingTreeBiomassChange,
           ES2_ShrubBiomassSequestr = ShrubBiomassChange,
           ES2_LiveBiomassSequestr = ES2_AdultTreeBiomassSequestr+ES2_SaplingTreeBiomassSequestr+ES2_ShrubBiomassSequestr) |>
    select(-c(6:13))
  
  ES2_10$Period[ES2_10$Period=="P1"] <- "2001-2010"
  ES2_10$Period[ES2_10$Period=="P2"] <- "2011-2020"
  ES2_10$Period[ES2_10$Period=="P3"] <- "2021-2030"
  ES2_10$Period[ES2_10$Period=="P4"] <- "2031-2040"
  ES2_10$Period[ES2_10$Period=="P5"] <- "2041-2050"
  ES2_10$Period[ES2_10$Period=="P6"] <- "2051-2060"
  ES2_10$Period[ES2_10$Period=="P7"] <- "2061-2070"
  ES2_10$Period[ES2_10$Period=="P8"] <- "2071-2080"
  ES2_10$Period[ES2_10$Period=="P9"] <- "2081-2090"
  ES2_10$Period[ES2_10$Period=="P10"] <- "2091-2100"
  
  ES2_10 <- ES2_10 |> 
    dplyr::mutate(Climate = toupper(Climate))|>
    mutate(Model = model) |>
    relocate(Model, .after = Period)
  
  ES2 <- bind_rows(ES2_10, ES2_20)
  return(ES2)
}

# ES2 - Carbon stocks (Mg C/ha) every 10 years -----------
ES2_function_state <- function(ALL, model) {
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
  ES2 <- ALL1 |>
    ungroup() |>
    select(Climate, Management, Province, id, Year, AdultTreeBiomass, SaplingTreeBiomass, ShrubBiomass) |>
    group_by(Climate, Management, Province) |>
    complete(id, Year, fill = list(AdultTreeBiomass = 0, SaplingTreeBiomass = 0, ShrubBiomass = 0))|>
    filter(Year %in% c(seq(2000,2100, by =10))) |>
    rename(ES2_AdultTreeBiomass = AdultTreeBiomass,
           ES2_SaplingTreeBiomass = SaplingTreeBiomass,
           ES2_ShrubBiomass = ShrubBiomass) |>
    tidyr::replace_na(list(ES2_AdultTreeBiomass = 0, ES2_SaplingTreeBiomass = 0, ES2_ShrubBiomass = 0)) |>
    mutate(ES2_LiveBiomass = ES2_AdultTreeBiomass + ES2_SaplingTreeBiomass+ES2_ShrubBiomass)|> 
    dplyr::mutate(Climate = toupper(Climate))|>
    mutate(Model = model) |>
    relocate(Model, .after = Year)|>
    dplyr::mutate(Climate = toupper(Climate))
  return(ES2)
}

# ES3 - Water provision (l/m3/yr) -----------------------------------------
ES3_function_period <- function(ALL, model) {
  ALL1 <- ALL
  if(model=="FORMES") {
    ALL1$Management <- ALL1$Management[ALL1$Year=="2021-2030"][1]
    ALL1$Year[ALL1$Year=="2001-2010"] = "2005"
    ALL1$Year[ALL1$Year=="2011-2020"] = "2015"
    ALL1$Year[ALL1$Year=="2021-2030"] = "2025"
    ALL1$Year[ALL1$Year=="2031-2040"] = "2035"
    ALL1$Year[ALL1$Year=="2041-2050"] = "2045"
    ALL1$Year[ALL1$Year=="2051-2060"] = "2055"
    ALL1$Year[ALL1$Year=="2061-2070"] = "2065"
    ALL1$Year[ALL1$Year=="2071-2080"] = "2075"
    ALL1$Year[ALL1$Year=="2081-2090"] = "2085"
    ALL1$Year[ALL1$Year=="2091-2100"] = "2095"
    ALL1$Year <- as.numeric(ALL1$Year)
  } 
  ES3_10 <- ALL1 |>
    filter(Year!=2000) |>
    select(Climate, Management, Province, id, Year, BlueWater, Precipitation) |>
    mutate(Period = as.character(cut(Year, breaks = seq(2000,2100, by = 10), 
                                     labels = paste0(seq(2001,2091, by=10), "-",seq(2010,2100, by=10)))),
           MidYear = as.numeric(as.character(cut(Year, breaks = seq(2000,2100, by = 10), 
                                                 labels = seq(2005, 2095, by=10))))) |>
    group_by(Climate, Management, Province, id, Period, MidYear) |>
    summarise(ES3_BlueWater = mean(BlueWater, na.rm=TRUE),
              Precipitation = mean(Precipitation, na.rm=TRUE),
              .groups = "drop") |>
    mutate(ES3_RunoffCoefficient = 100*(ES3_BlueWater/Precipitation)) |>
    mutate(Model = model)  |>
    relocate(Model, .after = MidYear) |>
    select(-Precipitation)|>
    dplyr::mutate(Climate = toupper(Climate))
  ES3_20 <- ALL1 |>
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
  ES3 <- bind_rows(ES3_10, ES3_20)
  return(ES3)
}

# ES4 - Erosion control (Mg/ha/yr) -----------------------------------------------
ES4_function_period <- function(ALL, model) {
  
  # constants
  a = 2
  b0 = 0.117
  b1 = -0.015

  ALL_SEL <- ALL |>
    filter(Year!=2000) |>
    select(Climate, Management, Province, id, Year, Pdaymax, Precipitation, PARground)|>
    mutate(idparcela = as.character(as.numeric(substr(id, 1,6)))) |>
    left_join(longitude_df, by="id") |>
    mutate(RainfallErosivity = b0*Precipitation*sqrt(Pdaymax)*(a+b1*longitude),
           C = PARground/100)

  ES4_10 <- ALL_SEL |>
    mutate(Period = as.character(cut(Year, breaks = seq(2000,2100, by = 10), 
                                     labels = paste0(seq(2001,2091, by=10), "-",seq(2010,2100, by=10)))),
           MidYear = as.numeric(as.character(cut(Year, breaks = seq(2000,2100, by = 10), 
                                                 labels = seq(2005, 2095, by=10))))) |>
    group_by(Climate, Management, Province, id, Period, MidYear) |>
    summarise(ES4_RainfallErosivity = mean(RainfallErosivity, na.rm = TRUE),  
              C = mean(C, na.rm=TRUE), 
              .groups = "drop") |>
    left_join(sf::st_drop_geometry(K_LS), by="id") |>
    mutate(ES4_StructuralImpact = Kst*LS*ES4_RainfallErosivity,
           ES4_ErosionMitigation = ES4_StructuralImpact*(1-C)) |>
    select(-c(C, LS, K, Kst)) |>
    mutate(Model = model) |>
    relocate(Model, .after = MidYear) |>
    dplyr::mutate(Climate = toupper(Climate))
  
  ES4_20 <- ALL_SEL |>
    mutate(Period = as.character(cut(Year, breaks = c(2000,2020,2040,2060,2080,2100), labels = c("2001-2020", "2021-2040", "2041-2060", "2061-2080", "2081-2100"))),
         MidYear = as.numeric(as.character(cut(Year, breaks = c(2000,2020,2040,2060,2080,2100), labels = c(2010, 2030, 2050, 2070, 2090))))) |>
    group_by(Climate, Management, Province, id, Period, MidYear) |>
    summarise(ES4_RainfallErosivity = mean(RainfallErosivity, na.rm = TRUE),  
              C = mean(C, na.rm=TRUE), 
              .groups = "drop") |>
    left_join(sf::st_drop_geometry(K_LS), by="id") |>
    mutate(ES4_StructuralImpact = Kst*LS*ES4_RainfallErosivity,
           ES4_ErosionMitigation = ES4_StructuralImpact*(1-C)) |>
    select(-c(C, LS, K, Kst)) |>
    mutate(Model = model) |>
    relocate(Model, .after = MidYear) |>
    dplyr::mutate(Climate = toupper(Climate))
  
  ES4 <- bind_rows(ES4_10, ES4_20)
  return(ES4)
}

# ES5 - Recreational value [0-1] ------------------------------------------
ES5_function_state <- function(ALL, model) {
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
  recr_fun<- function(maxDBH, cvDBH, LAI, maxShrubCover, treeRichness, shrubRichness) {
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
    return(val_sum/w_sum)
  }
  ES5 <- ALL1 |>
    ungroup() |>
    filter(Year %in% c(seq(2000,2100, by =10))) |>
    select(Climate, Management, Province, id, Year, cvDBH, maxDBH, MaxShrubCover, LAI_max, TreeRichness, ShrubRichness)|>
    group_by(Climate, Management, Province) |>
    complete(id, Year, fill = list(cvDBH = NA, maxDBH = 0, MaxShrubCover = 0,
                                   LAI_max = 0, TreeRichness = 0, ShrubRichness = 0)) |>
    ungroup() |>
    mutate(ES5_RecreationalValue = recr_fun(maxDBH, cvDBH, LAI_max, MaxShrubCover, TreeRichness, ShrubRichness))|>
    select(Climate, Management, Province, id, Year, ES5_RecreationalValue)|>
    mutate(Model = model)  |>
    relocate(Model, .after = Year) |>
    dplyr::mutate(Climate = toupper(Climate))
  return(ES5)
}

# ES6 - Fire risk per 20-year periods -----------
ES6_function_period <- function(ALL, model) {
  ALL1 <- ALL
  if(model=="FORMES") {
    ALL1$Management <- ALL1$Management[ALL1$Year=="2021-2030"][1]
    ALL1$Year[ALL1$Year=="2001-2010"] = "2005"
    ALL1$Year[ALL1$Year=="2011-2020"] = "2015"
    ALL1$Year[ALL1$Year=="2021-2030"] = "2025"
    ALL1$Year[ALL1$Year=="2031-2040"] = "2035"
    ALL1$Year[ALL1$Year=="2041-2050"] = "2045"
    ALL1$Year[ALL1$Year=="2051-2060"] = "2055"
    ALL1$Year[ALL1$Year=="2061-2070"] = "2065"
    ALL1$Year[ALL1$Year=="2071-2080"] = "2075"
    ALL1$Year[ALL1$Year=="2081-2090"] = "2085"
    ALL1$Year[ALL1$Year=="2091-2100"] = "2095"
    ALL1$Year <- as.numeric(ALL1$Year)
  } 
  ES6_10 <- ALL1 |>
    ungroup() |>
    select(Climate, Management, Province, id, Year, SFP, CFP) |>
    group_by(Climate, Management, Province) |>
    complete(id, Year, fill = list(SFP = 0, CFP = 0))|>
    mutate(Period = as.character(cut(Year, breaks = seq(2000,2100, by = 10), 
                                     labels = paste0(seq(2001,2091, by=10), "-",seq(2010,2100, by=10)))),
           MidYear = as.numeric(as.character(cut(Year, breaks = seq(2000,2100, by = 10), 
                                                 labels = seq(2005, 2095, by=10))))) |>
    group_by(Climate, Management, Province, id, Period, MidYear) |>
    summarise(ES6_SurfaceFirePotential = mean(SFP, na.rm=TRUE),
              ES6_CrownFirePotential = mean(CFP, na.rm=TRUE),
              .groups = "drop") |>
    tidyr::replace_na(list(ES6_SurfaceFirePotential = 0, ES6_CrownFirePotential = 0)) |>
    mutate(Model = model) |>
    relocate(Model, .after = MidYear)|>
    dplyr::mutate(Climate = toupper(Climate))
  ES6_20 <- ALL1 |>
    ungroup() |>
    select(Climate, Management, Province, id, Year, SFP, CFP) |>
    group_by(Climate, Management, Province) |>
    complete(id, Year, fill = list(SFP = 0, CFP = 0))|>
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
  ES6 <- bind_rows(ES6_10, ES6_20)
  return(ES6)
}

# ES - ALL ----------------------------------------------------------------
generate_ES_table <- function(type = "period", test = FALSE, model = "MEDFATE") {
  ES_function<-function(type = "state", ALL, model) {
    if(type=="period") {
      ES1 <- ES1_function_period(ALL, model)
      ES2 <- ES2_function_period(ALL, model)
      ES <- ES1 |>
        left_join(ES2, by=c("Climate", "Management", "Province", "id", "Period", "Model"))
      
      if(model=="MEDFATE") {
        ES3 <- ES3_function_period(ALL, model)
        ES4 <- ES4_function_period(ALL, model)
        ES <- ES |>
          left_join(ES3, by=c("Climate", "Management", "Province", "id", "Period", "MidYear","Model"))|>
          left_join(ES4, by=c("Climate", "Management", "Province", "id", "Period", "MidYear","Model"))
        ES6 <- ES6_function_period(ALL, model)
        ES <- ES |>
          left_join(ES6, by=c("Climate", "Management", "Province", "id", "Period", "MidYear","Model"))
      }
    } else {
      ES1s <- ES1_function_state(ALL, model)
      ES2s <- ES2_function_state(ALL, model)
      ES <- ES1s |>
        left_join(ES2s, by=c("Climate", "Management", "Province", "id", "Year","Model"))
      if(model=="MEDFATE") {
        ES5s <- ES5_function_state(ALL, model)
        ES <- ES |>
          left_join(ES5s, by=c("Climate", "Management", "Province", "id", "Year","Model"))
      }
    }
    return(ES)
  }
  if(model=="MEDFATE") {
    if(test) {
      cli::cli_progress_step("BAU/RCP45")
      BAU_rcp45 <- ES_function(type, readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_BAU_mpiesm_rca4_rcp45.rds"), model)
      cli::cli_progress_step("BAU/RCP85")
      BAU_rcp85 <- ES_function(type, readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_BAU_mpiesm_rca4_rcp85.rds"), model)
      cli::cli_progress_step("AMF/RCP45")
      AMF_rcp45 <- ES_function(type, readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_AMF_mpiesm_rca4_rcp45.rds"), model)
      cli::cli_progress_step("AMF/RCP85")
      AMF_rcp85 <- ES_function(type, readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_AMF_mpiesm_rca4_rcp85.rds"), model)
      cli::cli_progress_step("RSB/RCP45")
      RSB_rcp45 <- ES_function(type, readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_RSB_mpiesm_rca4_rcp45.rds"), model)
      cli::cli_progress_step("RSB/RCP85")
      RSB_rcp85 <- ES_function(type, readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_RSB_mpiesm_rca4_rcp85.rds"), model)
      cli::cli_progress_step("ASEA/RCP45")
      ASEA_rcp45 <- ES_function(type, readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_ASEA_mpiesm_rca4_rcp45.rds"), model)
      cli::cli_progress_step("ASEA/RCP85")
      ASEA_rcp85 <- ES_function(type, readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_ASEA_mpiesm_rca4_rcp85.rds"), model)
      cli::cli_progress_step("ACG/RCP45")
      ACG_rcp45 <- ES_function(type, readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_ACG_mpiesm_rca4_rcp45.rds"), model)
      cli::cli_progress_step("ACG/RCP85")
      ACG_rcp85 <- ES_function(type, readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_ACG_mpiesm_rca4_rcp85.rds"), model)
      # cli::cli_progress_step("NOG/RCP45")
      # NOG_rcp45 <- ES_function(type, readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_NOG_mpiesm_rca4_rcp45.rds"), model)
      # cli::cli_progress_step("NOG/RCP85")
      # NOG_rcp85 <- ES_function(type, readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_NOG_mpiesm_rca4_rcp85.rds"), model)
    } else {
      cli::cli_progress_step("BAU/RCP45")
      BAU_rcp45 <- ES_function(type, readRDS("Rdata/MEDFATE/annual_indicators/BAU_mpiesm_rca4_rcp45.rds"), model)
      cli::cli_progress_step("BAU/RCP85")
      BAU_rcp85 <- ES_function(type, readRDS("Rdata/MEDFATE/annual_indicators/BAU_mpiesm_rca4_rcp85.rds"), model)
      cli::cli_progress_step("AMF/RCP45")
      AMF_rcp45 <- ES_function(type, readRDS("Rdata/MEDFATE/annual_indicators/AMF_mpiesm_rca4_rcp45.rds"), model)
      cli::cli_progress_step("AMF/RCP85")
      AMF_rcp85 <- ES_function(type, readRDS("Rdata/MEDFATE/annual_indicators/AMF_mpiesm_rca4_rcp85.rds"), model)
      cli::cli_progress_step("RSB/RCP45")
      RSB_rcp45 <- ES_function(type, readRDS("Rdata/MEDFATE/annual_indicators/RSB_mpiesm_rca4_rcp45.rds"), model)
      cli::cli_progress_step("RSB/RCP85")
      RSB_rcp85 <- ES_function(type, readRDS("Rdata/MEDFATE/annual_indicators/RSB_mpiesm_rca4_rcp85.rds"), model)
      cli::cli_progress_step("ASEA/RCP45")
      ASEA_rcp45 <- ES_function(type, readRDS("Rdata/MEDFATE/annual_indicators/ASEA_mpiesm_rca4_rcp45.rds"), model)
      cli::cli_progress_step("ASEA/RCP85")
      ASEA_rcp85 <- ES_function(type, readRDS("Rdata/MEDFATE/annual_indicators/ASEA_mpiesm_rca4_rcp85.rds"), model)
      cli::cli_progress_step("ACG/RCP45")
      ACG_rcp45 <- ES_function(type, readRDS("Rdata/MEDFATE/annual_indicators/ACG_mpiesm_rca4_rcp45.rds"), model)
      cli::cli_progress_step("ACG/RCP85")
      ACG_rcp85 <- ES_function(type, readRDS("Rdata/MEDFATE/annual_indicators/ACG_mpiesm_rca4_rcp85.rds"), model)
      # cli::cli_progress_step("NOG/RCP45")
      # NOG_rcp45 <- ES_function(type, readRDS("Rdata/MEDFATE/annual_indicators/NOG_mpiesm_rca4_rcp45.rds"), model)
      # cli::cli_progress_step("NOG/RCP85")
      # NOG_rcp85 <- ES_function(type, readRDS("Rdata/MEDFATE/annual_indicators/NOG_mpiesm_rca4_rcp85.rds"), model)
    }
  } else {
    cli::cli_progress_step("BAU/RCP45")
    BAU_rcp45 <- ES_function(type, readRDS("Rdata/FORMES/annual_indicators/BAU_mpiesm_rca4_rcp45.rds"), model)
    cli::cli_progress_step("BAU/RCP85")
    BAU_rcp85 <- ES_function(type, readRDS("Rdata/FORMES/annual_indicators/BAU_mpiesm_rca4_rcp85.rds"), model)
    cli::cli_progress_step("AMF/RCP45")
    AMF_rcp45 <- ES_function(type, readRDS("Rdata/FORMES/annual_indicators/AMF_mpiesm_rca4_rcp45.rds"), model)
    cli::cli_progress_step("AMF/RCP85")
    AMF_rcp85 <- ES_function(type, readRDS("Rdata/FORMES/annual_indicators/AMF_mpiesm_rca4_rcp85.rds"), model)
    cli::cli_progress_step("RSB/RCP45")
    RSB_rcp45 <- ES_function(type, readRDS("Rdata/FORMES/annual_indicators/RSB_mpiesm_rca4_rcp45.rds"), model)
    cli::cli_progress_step("RSB/RCP85")
    RSB_rcp85 <- ES_function(type, readRDS("Rdata/FORMES/annual_indicators/RSB_mpiesm_rca4_rcp85.rds"), model)
    cli::cli_progress_step("ASEA/RCP45")
    ASEA_rcp45 <- ES_function(type, readRDS("Rdata/FORMES/annual_indicators/ASEA_mpiesm_rca4_rcp45.rds"), model)
    cli::cli_progress_step("ASEA/RCP85")
    ASEA_rcp85 <- ES_function(type, readRDS("Rdata/FORMES/annual_indicators/ASEA_mpiesm_rca4_rcp85.rds"), model)
    cli::cli_progress_step("ACG/RCP45")
    ACG_rcp45 <- ES_function(type, readRDS("Rdata/FORMES/annual_indicators/ACG_mpiesm_rca4_rcp45.rds"), model)
    cli::cli_progress_step("ACG/RCP85")
    ACG_rcp85 <- ES_function(type, readRDS("Rdata/FORMES/annual_indicators/ACG_mpiesm_rca4_rcp85.rds"), model)
    # cli::cli_progress_step("NOG/RCP45")
    # NOG_rcp45 <- ES_function(type, readRDS("Rdata/FORMES/annual_indicators/NOG_mpiesm_rca4_rcp45.rds"), model)
    # cli::cli_progress_step("NOG/RCP85")
    # NOG_rcp85 <- ES_function(type, readRDS("Rdata/FORMES/annual_indicators/NOG_mpiesm_rca4_rcp85.rds"), model)
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



# ES calculation ----------------------------------------------------------
ES_period_MEDFATE_test <- generate_ES_table("period", TRUE, model = "MEDFATE")
ES_period_MEDFATE_test <- ES_period_MEDFATE_test |>
  left_join(nfiplot[,c("id")], by="id") |>
  sf::st_as_sf()
saveRDS(ES_period_MEDFATE_test, "Rdata/ES_period_MEDFATE_test.rds")

ES_state_MEDFATE_test <- generate_ES_table("state", TRUE, model = "MEDFATE")
ES_state_MEDFATE_test <- ES_state_MEDFATE_test |>
  left_join(nfiplot[,c("id")], by="id") |>
  sf::st_as_sf()
saveRDS(ES_state_MEDFATE_test, "Rdata/ES_state_MEDFATE_test.rds")

ES_period_MEDFATE <- generate_ES_table("period",FALSE, model = "MEDFATE")
ES_period_MEDFATE <- ES_period_MEDFATE |>
  left_join(nfiplot[,c("id")], by="id") |>
  sf::st_as_sf()
saveRDS(ES_period_MEDFATE, "Rdata/ES_period_MEDFATE.rds")

ES_state_MEDFATE <- generate_ES_table("state", FALSE, model = "MEDFATE")
ES_state_MEDFATE <- ES_state_MEDFATE |>
  left_join(nfiplot[,c("id")], by="id") |>
  sf::st_as_sf()
saveRDS(ES_state_MEDFATE, "Rdata/ES_state_MEDFATE.rds")

ES_period_FORMES <- generate_ES_table("period", FALSE, model = "FORMES")
nfiplot_formes <- nfiplot |>
  mutate(id = as.character(as.numeric(IDPARCELA)))
ES_period_FORMES <- ES_period_FORMES |>
  left_join(nfiplot_formes[,c("id")], by="id") |>
  sf::st_as_sf()
saveRDS(ES_period_FORMES, "Rdata/ES_period_FORMES.rds")

ES_state_FORMES <- generate_ES_table("state",FALSE, model = "FORMES")
nfiplot_formes <- nfiplot |>
  mutate(id = as.character(as.numeric(IDPARCELA)))
ES_state_FORMES <- ES_state_FORMES |>
  left_join(nfiplot_formes[,c("id")], by="id") |>
  sf::st_as_sf()
saveRDS(ES_state_FORMES, "Rdata/ES_state_FORMES.rds")
