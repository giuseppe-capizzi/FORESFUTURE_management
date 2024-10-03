#########################################################################################################
##
## Calculate PAR ground for simulations with FORMES
##
#########################################################################################################

rm(list=ls())
library(medfate) # 3.2.0
library(medfateland) #2.0.5
library(medfateutils) # 0.1.7
library(tidyverse)

overwrite <- TRUE

# set directory (for meteo, scenarios and savings)
# setwd("~/work/model_application/FORESFUTURE_WB/" )

# load nfiplot
nfiplot <- readRDS("nfiplot.rds") # Load nfiplot file
nfiplot$IDPARCELA <- sub("^0+", "", nfiplot$IDPARCELA) 
nfiplot$Provincia <- sub("^0+", "", nfiplot$Provincia)

# base sf object
sf <- nfiplot |>
  select(id, IDPARCELA, Provincia, elevation, slope, aspect, soil, forest)


# Historic ----------------------------------------------------------------
for(p in  c(8,17,25,43)) {
  # read scenario 
  scn <- readRDS(paste0("Scenarios/BAU/BAU_2001-2020/", p, ".rds"))

  treeData <- scn$treeDataSequence
  
  ##### 2001-2010
  sf_prov <- sf |>
    filter(Provincia == p) 
  # transform treedata according to medfate
  treeData_1 <- treeData %>%
    filter(Step==1) %>%
    select(-c("Step", "N")) %>% 
    mutate(DBH = DBH*10, Dn2 = DBH) %>% #  DBH from cm to mm
    rename(Ht=H, Dn1 = DBH, Especie=Species) %>%
    relocate(Dn2, .after = Dn1)

  treeData_forest_1 <- IFN2forest(treeData_1,
                                          IFN_species_mapping = IFN_species_mapping,
                                          SpParams = SpParamsMED)
  
  # replace original N
  cat("Replacing original N...", "\n")
  for(i in names(treeData_forest_1)){
    # print(i)
    if(length(treeData_forest_1[[i]]$treeData_1$N)==length(treeData[treeData_1$ID== i & treeData_1$Step== 1,"N"])){
      treeData_forest_1[[i]]$treeData_1$N <- treeData_1[treeData_1$ID== i & treeData_1$Step== 1,"N"]
    } else {warning("Plot ", i, " doesn't have the same length as treeData")}
    
    # replace in sf_prov$forest the treeData
    sf_prov[sf_prov$IDPARCELA == i,]$forest[[1]]$treeData <- treeData_forest_1[[i]]$treeData
  }

  sf_prov$PARground <- NA
  for(i in 1:nrow(sf_prov)) {
    sf_prov$PARground[i] <- medfate::light_PARground(sf_prov$forest[[i]], SpParamsMED)
  }
  
  # save results 
  file <- paste0("PARground/historic/PARground_",  p, "_2001_2010.rds")
  saveRDS(sf_prov[,c("id", "IDPARCELA", "Provincia", "PARground")], file) 
    
  ##### 2011-2020
  sf_prov <- sf |>
    filter(Provincia == p) 
  # transform treedata according to medfate
  treeData_2 <- treeData %>%
    filter(Step==2) %>%
    select(-c("Step", "N")) %>% 
    mutate(DBH = DBH*10, Dn2 = DBH) %>% #  DBH from cm to mm
    rename(Ht=H, Dn1 = DBH, Especie=Species) %>%
    relocate(Dn2, .after = Dn1)
  
  treeData_forest_2 <- IFN2forest(treeData_2,
                                  IFN_species_mapping = IFN_species_mapping,
                                  SpParams = SpParamsMED)
  
  # replace original N
  cat("Replacing original N...", "\n")
  for(i in names(treeData_forest_2)){
    # print(i)
    if(length(treeData_forest_2[[i]]$treeData_2$N)==length(treeData[treeData_2$ID== i & treeData_2$Step== 2,"N"])){
      treeData_forest_2[[i]]$treeData_2$N <- treeData_2[treeData_2$ID== i & treeData_2$Step== 2,"N"]
    } else {warning("Plot ", i, " doesn't have the same length as treeData")}
    
    # replace in sf_prov$forest the treeData
    sf_prov[sf_prov$IDPARCELA == i,]$forest[[1]]$treeData <- treeData_forest_2[[i]]$treeData
  }
  
  sf_prov$PARground <- NA
  for(i in 1:nrow(sf_prov)) {
    sf_prov$PARground[i] <- medfate::light_PARground(sf_prov$forest[[i]], SpParamsMED)
  }
  
  # save results 
  file <- paste0("PARground/historic/PARground_",  p, "_2011_2020.rds")
  saveRDS(sf_prov[,c("id", "IDPARCELA", "Provincia", "PARground")], file) 
}


# Scenarios ---------------------------------------------------------------
for(scn_type in c("BAU", "ASEA", "AMF", "RSB", "ACG")) {
  for(rcp in c("45", "85")) {
    # load nfiplot
    nfiplot <- readRDS("nfiplot.rds") # Load nfiplot file
    
    nfiplot$IDPARCELA <- sub("^0+", "", nfiplot$IDPARCELA) 
    nfiplot$Provincia <- sub("^0+", "", nfiplot$Provincia)
    
    
    # base sf object
    sf <- nfiplot |>
      select(id, IDPARCELA, Provincia, elevation, slope, aspect, soil, forest)
    
    
    ############ PROJECTIONS ###########
    # inputs for directory
    pattern <- "\\d{4}-\\d{4}"
    prov <- c(8,17,25,43)
    dirs <- list.files(paste0("Scenarios/",scn_type , "/", paste0(scn_type, "_", rcp)), full.names = T)
    
    for(d in dirs){ # for each directory
      # retrieve years range
      year_range <- unlist(strsplit(str_extract(d, pattern), "-"))
      start_year <- as.numeric(year_range[1])
      end_year <- as.numeric(year_range[2])
      if(end_year-start_year > 9){stop("Wrong year range for projections")}
      
      for(p in prov){ # for each province
        
        file_wb <- paste0("PARground/", scn_type, "/PARground_",rcp,"_",  p, "_", start_year, "_", end_year, ".rds")
        if(!file.exists(file_wb) || overwrite) {
          cat("Calculating PARground for province ", p, " and years ", start_year, "-", end_year, "\n")
          
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
          
          sf_prov$PARground <- NA
          for(i in 1:nrow(sf_prov)) {
            sf_prov$PARground[i] <- medfate::light_PARground(sf_prov$forest[[i]], SpParamsMED)
          }
          
          # save results 
          saveRDS(sf_prov[,c("id", "IDPARCELA", "Provincia", "PARground")], file_wb) 
        }
        
      }
    }
  }
}
