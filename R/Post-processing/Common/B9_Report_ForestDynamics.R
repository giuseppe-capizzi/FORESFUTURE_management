#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#                                                                                                    #
####                        Script to generate forest dynamics graphs                              ####
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(cowplot)
library(sf)

# load data functions FORMES
load_volume_table_FORMES <- function(test = FALSE) {
  df <- data.frame()
  for(provinceName in c("Barcelona", "Girona","Lleida", "Tarragona")) {
    for(management_scen in c("BAU", "AMF", "RSB", "ASEA", "ACG", "NOG")) { # , "AMF", "RSB", "ASEA", "ACG", "NOG")
      cli::cli_progress_step(paste0("Province: ", provinceName, " Scenario: ", management_scen))
      for(climate_model in c("mpiesm_rca4")) {
        for(climate_scen in c("rcp45", "rcp85")) {
          if(test) {
            bind_file <- paste0("Rdata/Test_binded/Test_", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds")
          } else {
            bind_file <- paste0("Rdata/binded/", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds")
          }
          if(file.exists(bind_file)) {
            scen_list <- readRDS(file = bind_file)
            df <- bind_rows(df, scen_list$volume_table) 
          }
        }
      }
    }
  }
  cli::cli_progress_step("Decade summaries")
  # browser()
  df_decade <- df |>
    dplyr::mutate(Decade = Year) |>
    dplyr::group_by(Climate, Management, Province, Decade) |>
    dplyr::summarise(Growth = max(growth,0), 
                     Mortality = max(mortality,0), 
                     Extraction = sum(extracted),
                     Demand = max(nominal_demand,0),
                     ExtractionTarget = sum(extracted_target), .groups = "drop") |>
    dplyr::mutate(ExtractionRate = 100*Extraction/Growth,
                  SatisfactionRate = 100*ExtractionTarget/Demand)
  
  return(df_decade)
}
load_result_table_FORMES <- function(test = FALSE) {
  
  cli::cli_progress_step("Coordinates and area represented")
  nfiplot <- dplyr::bind_rows(readRDS(paste0("Rdata/nfiplot.rds")))
  nfiplot$id <- sub("^0+", "", nfiplot$IDPARCELA)
  if(test) {
    initial <- readRDS(paste0("Rdata/test_initial.rds"))
    initial <- initial[[1]]
    n <- nrow(nfiplot)
    n_test<-nrow(initial)
    initial$represented_area <- initial$represented_area*(n/n_test)
  } else {
    initial <- nfiplot
    initial$represented_area <- initial$area
  }
  cli::cli_progress_step("Loading annual indicators")
  if(test) {
    BAU_rcp45 <- readRDS("Rdata/Test_annual_indicators/Test_BAU_mpiesm_rca4_rcp45.rds")
    BAU_rcp85 <- readRDS("Rdata/Test_annual_indicators/Test_BAU_mpiesm_rca4_rcp85.rds")
    
    AMF_rcp45 <- readRDS("Rdata/Test_annual_indicators/Test_AMF_mpiesm_rca4_rcp45.rds")
    AMF_rcp85 <- readRDS("Rdata/Test_annual_indicators/Test_AMF_mpiesm_rca4_rcp85.rds")
    
    RSB_rcp45 <- readRDS("Rdata/Test_annual_indicators/Test_RSB_mpiesm_rca4_rcp45.rds")
    RSB_rcp85 <- readRDS("Rdata/Test_annual_indicators/Test_RSB_mpiesm_rca4_rcp85.rds")
    
    ASEA_rcp45 <- readRDS("Rdata/Test_annual_indicators/Test_ASEA_mpiesm_rca4_rcp45.rds")
    ASEA_rcp85 <- readRDS("Rdata/Test_annual_indicators/Test_ASEA_mpiesm_rca4_rcp85.rds")
    
    ACG_rcp45 <- readRDS("Rdata/Test_annual_indicators/Test_ACG_mpiesm_rca4_rcp45.rds")
    ACG_rcp85 <- readRDS("Rdata/Test_annual_indicators/Test_ACG_mpiesm_rca4_rcp85.rds")
    
    NOG_rcp45 <- readRDS("Rdata/Test_annual_indicators/Test_NOG_mpiesm_rca4_rcp45.rds")
    NOG_rcp85 <- readRDS("Rdata/Test_annual_indicators/Test_NOG_mpiesm_rca4_rcp85.rds")
  } else {
    BAU_rcp45 <- readRDS("Rdata/annual_indicators/BAU_mpiesm_rca4_rcp45.rds")
    BAU_rcp85 <- readRDS("Rdata/annual_indicators/BAU_mpiesm_rca4_rcp85.rds")
    
    AMF_rcp45 <- readRDS("Rdata/annual_indicators/AMF_mpiesm_rca4_rcp45.rds")
    AMF_rcp85 <- readRDS("Rdata/annual_indicators/AMF_mpiesm_rca4_rcp85.rds")
    
    RSB_rcp45 <- readRDS("Rdata/annual_indicators/RSB_mpiesm_rca4_rcp45.rds")
    RSB_rcp85 <- readRDS("Rdata/annual_indicators/RSB_mpiesm_rca4_rcp85.rds")
    
    ASEA_rcp45 <- readRDS("Rdata/annual_indicators/ASEA_mpiesm_rca4_rcp45.rds")
    ASEA_rcp85 <- readRDS("Rdata/annual_indicators/ASEA_mpiesm_rca4_rcp85.rds")
    
    ACG_rcp45 <- readRDS("Rdata/annual_indicators/ACG_mpiesm_rca4_rcp45.rds")
    ACG_rcp85 <- readRDS("Rdata/annual_indicators/ACG_mpiesm_rca4_rcp85.rds")
    
    NOG_rcp45 <- readRDS("Rdata/annual_indicators/NOG_mpiesm_rca4_rcp45.rds")
    NOG_rcp85 <- readRDS("Rdata/annual_indicators/NOG_mpiesm_rca4_rcp85.rds")
  }
  
  
  cli::cli_progress_step("Calculating extensive variables")
  ALL <- bind_rows(BAU_rcp45, BAU_rcp85,
                   AMF_rcp45, AMF_rcp85,
                   RSB_rcp45, RSB_rcp85,
                   ASEA_rcp45, ASEA_rcp85,
                   ACG_rcp45, ACG_rcp85
                   ,NOG_rcp45, NOG_rcp85) |>
    dplyr::left_join(sf::st_drop_geometry(initial)[,c("id", "represented_area"), drop = FALSE], by = "id") |>
    dplyr::mutate(AllVolumeExt = AllVolume*represented_area,
                  VolumeStructureExt = VolumeStructure*represented_area,
                  CutStructureExt = CutStructure*represented_area,
                  CutAllExt = CutAll*represented_area,
                  CumulativeCutAllExt = CumulativeCutAll*represented_area) |>
    dplyr::mutate(Climate = toupper(Climate))
  return(ALL)
}


# load data functions Medfate
load_volume_table_medfate <- function(test = FALSE) {
  df <- data.frame()
  for(provinceName in c("Barcelona", "Girona","Lleida", "Tarragona")) {
    for(management_scen in c("BAU", "AMF", "RSB", "ASEA", "ACG", "NOG")) {
      cli::cli_progress_step(paste0("Province: ", provinceName, " Scenario: ", management_scen))
      for(climate_model in c("mpiesm_rca4")) {
        for(climate_scen in c("rcp45", "rcp85")) {
          if(test) {
            # bind_file <- paste0("Rdata/Test_binded/Test_", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds")
          } else {
            bind_file <- paste0("Rdata/MEDFATE/binded/", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds")
          }
          if(file.exists(bind_file)) {
            scen_list <- readRDS(file = bind_file)
            df <- bind_rows(df, scen_list$volume_table) 
          }
        }
      }
    }
  }
  cli::cli_progress_step("Decade summaries")
  df_decade <- df |>
    dplyr::mutate(Decade = cut(Year, seq(2000,2100, by=10), 
                               labels = paste0(seq(2001, 2091, by=10),"-", seq(2010, 2100, by=10)))) |>
    dplyr::group_by(Climate, Management, Province, Decade) |>
    dplyr::summarise(Growth = sum(pmax(growth,0)), 
                     Mortality = sum(pmax(mortality,0)), 
                     Extraction = sum(extracted),
                     Demand = sum(pmax(nominal_demand,0)),
                     ExtractionTarget = sum(extracted_target), .groups = "drop") |>
    dplyr::mutate(ExtractionRate = 100*Extraction/Growth,
                  SatisfactionRate = 100*ExtractionTarget/Demand)
  
  return(df_decade)
}
load_result_table_medfate <- function(test = FALSE) {
  
  cli::cli_progress_step("Coordinates and area represented")
  nfiplot <- dplyr::bind_rows(readRDS(paste0("Rdata/nfiplot.rds")))
  if(test) {
    initial <- readRDS(paste0("Rdata/test_initial.rds"))
    initial <- initial[[1]]
    n <- nrow(nfiplot)
    n_test<-nrow(initial)
    initial$represented_area <- initial$represented_area*(n/n_test)
  } else {
    initial <- nfiplot
    initial$represented_area <- initial$area
  }
  cli::cli_progress_step("Loading annual indicators")
  if(test) {
    # BAU_rcp45 <- readRDS("Rdata/Test_annual_indicators/Test_BAU_mpiesm_rca4_rcp45.rds")
    # BAU_rcp85 <- readRDS("Rdata/Test_annual_indicators/Test_BAU_mpiesm_rca4_rcp85.rds")
    # 
    # AMF_rcp45 <- readRDS("Rdata/MEDFATE/Test_annual_indicators/Test_AMF_mpiesm_rca4_rcp45.rds")
    # AMF_rcp85 <- readRDS("Rdata/Test_annual_indicators/Test_AMF_mpiesm_rca4_rcp85.rds")
    # 
    # RSB_rcp45 <- readRDS("Rdata/Test_annual_indicators/Test_RSB_mpiesm_rca4_rcp45.rds")
    # RSB_rcp85 <- readRDS("Rdata/Test_annual_indicators/Test_RSB_mpiesm_rca4_rcp85.rds")
    # 
    # ASEA_rcp45 <- readRDS("Rdata/Test_annual_indicators/Test_ASEA_mpiesm_rca4_rcp45.rds")
    # ASEA_rcp85 <- readRDS("Rdata/Test_annual_indicators/Test_ASEA_mpiesm_rca4_rcp85.rds")
    # 
    # ACG_rcp45 <- readRDS("Rdata/Test_annual_indicators/Test_ACG_mpiesm_rca4_rcp45.rds")
    # ACG_rcp85 <- readRDS("Rdata/Test_annual_indicators/Test_ACG_mpiesm_rca4_rcp85.rds")
    # 
    # NOG_rcp45 <- readRDS("Rdata/Test_annual_indicators/Test_NOG_mpiesm_rca4_rcp45.rds")
    # NOG_rcp85 <- readRDS("Rdata/Test_annual_indicators/Test_NOG_mpiesm_rca4_rcp85.rds")
  } else {
    BAU_rcp45 <- readRDS("Rdata/MEDFATE/annual_indicators/BAU_mpiesm_rca4_rcp45.rds")
    BAU_rcp85 <- readRDS("Rdata/MEDFATE/annual_indicators/BAU_mpiesm_rca4_rcp85.rds")
    
    AMF_rcp45 <- readRDS("Rdata/MEDFATE/annual_indicators/AMF_mpiesm_rca4_rcp45.rds")
    AMF_rcp85 <- readRDS("Rdata/MEDFATE/annual_indicators/AMF_mpiesm_rca4_rcp85.rds")
    
    RSB_rcp45 <- readRDS("Rdata/MEDFATE/annual_indicators/RSB_mpiesm_rca4_rcp45.rds")
    RSB_rcp85 <- readRDS("Rdata/MEDFATE/annual_indicators/RSB_mpiesm_rca4_rcp85.rds")
    
    ASEA_rcp45 <- readRDS("Rdata/MEDFATE/annual_indicators/ASEA_mpiesm_rca4_rcp45.rds")
    ASEA_rcp85 <- readRDS("Rdata/MEDFATE/annual_indicators/ASEA_mpiesm_rca4_rcp85.rds")
    
    ACG_rcp45 <- readRDS("Rdata/MEDFATE/annual_indicators/ACG_mpiesm_rca4_rcp45.rds")
    ACG_rcp85 <- readRDS("Rdata/MEDFATE/annual_indicators/ACG_mpiesm_rca4_rcp85.rds")
    
    # NOG_rcp45 <- readRDS("Rdata/MEDFATE/annual_indicators/NOG_mpiesm_rca4_rcp45.rds")
    # NOG_rcp85 <- readRDS("Rdata/MEDFATE/annual_indicators/NOG_mpiesm_rca4_rcp85.rds")
  }
  
  
  
  cli::cli_progress_step("Calculating extensive variables")
  ALL <- bind_rows(BAU_rcp45, BAU_rcp85,
                   AMF_rcp45, AMF_rcp85,
                   RSB_rcp45, RSB_rcp85,
                   ASEA_rcp45, ASEA_rcp85,
                   ACG_rcp45, ACG_rcp85
                   # ,NOG_rcp45, NOG_rcp85
                   ) |>
    dplyr::left_join(sf::st_drop_geometry(initial)[,c("id", "represented_area"), drop = FALSE], by = "id") |>
    dplyr::mutate(AllVolumeExt = AllVolume*represented_area,
                  VolumeStructureExt = VolumeStructure*represented_area,
                  CutStructureExt = CutStructure*represented_area,
                  CutAllExt = CutAll*represented_area,
                  CumulativeCutAllExt = CumulativeCutAll*represented_area) |>
    dplyr::mutate(Climate = toupper(Climate))
  return(ALL)
}


# load data (FORMES and Medfate)
# select correct function and directory(?)

test <- FALSE

# FORMES
ALL_FORMES <- load_result_table_FORMES(test = test)
VOL_FORMES <- load_volume_table_FORMES(test = test)

# Medfate
ALL_medfate <- load_result_table_medfate(test = test)
VOL_medfate <- load_volume_table_medfate(test = test)

# change format of climate 
ALL_FORMES$Climate <- ifelse(ALL_FORMES$Climate=="RCP45", "RCP 4.5", "RCP 8.5")
ALL_medfate$Climate <- ifelse(ALL_medfate$Climate=="RCP45", "RCP 4.5", "RCP 8.5")

VOL_FORMES$Climate <- ifelse(VOL_FORMES$Climate=="rcp45", "RCP 4.5", "RCP 8.5")
VOL_medfate$Climate <- ifelse(VOL_medfate$Climate=="rcp45", "RCP 4.5", "RCP 8.5")

# PLOTS ----
# Plot Functions #####
plot_var_FORMES <- function(x, var, ylab, 
                     aggregateProvinces = TRUE, stand_agg_fun = "mean", 
                     provinces = c("Barcelona", "Girona", "Lleida", "Tarragona"),
                     scenarios = c("BAU", "AMF", "RSB", "ASEA", "ACG", "NOG"),
                     linewidth = 0.7, alpha = 0.2, se = FALSE, quantile_range = FALSE) {
  x <- x |>
    filter(Province %in% provinces, Management %in% scenarios)
  
  common = c("2000","2001-2010", "2011-2020")
  
  if(aggregateProvinces) {
    # browser()
    if(stand_agg_fun == "mean") {
      x_agg <- x |>
        dplyr::group_by(Climate, Management, Year) |>
        dplyr::summarise(y = mean(.data[[var]], na.rm = TRUE),
                         sd.y = sd(.data[[var]], na.rm = TRUE),
                         n.y = n(),
                         q25 = quantile(.data[[var]], probs = 0.25, na.rm = TRUE),
                         q75 = quantile(.data[[var]], probs = 0.75, na.rm = TRUE),
                         .groups="drop") |>
        dplyr::mutate(y_min = y - sd.y/sqrt(n.y)*1.96,
                      y_max = y + sd.y/sqrt(n.y)*1.96)

      g<- ggplot(x_agg)+
        facet_wrap(vars(Climate), nrow = 1)+
        geom_line(aes(x = Year, y=y, col=Management, group=Management), linewidth = 1.0)+
        geom_line(data = x_agg[x_agg$Year %in% c(common),], 
                  aes(x = Year, y=y, group=Management), col = "black", linewidth = 1.0) +
        geom_point(aes(x = Year, y=y, col=Management, group=Management)) +
        geom_point(data = x_agg[x_agg$Year %in% common,],
                   aes(x = Year, y=y, group=Management), col = "black")
      if(se) g <- g+
        geom_ribbon(aes(x = Year, ymin=y_min, ymax = y_max, fill=Management, group=Management), alpha = alpha)
      if(quantile_range) g <- g+
        geom_ribbon(aes(x = Year, ymin=q25, ymax = q75, fill=Management, group=Management), alpha = alpha)
    } else if(stand_agg_fun == "sum") {
      x_agg <- x |>
        dplyr::group_by(Climate, Management, Year) |>
        dplyr::summarise(y = sum(.data[[var]], na.rm = TRUE), .groups="drop")
      g<- ggplot(x_agg)+
        facet_wrap(vars(Climate), nrow = 1)+
        geom_line(aes(x = Year, y=y, col=Management, group=Management), linewidth = 1.0)+
        geom_line(data = x_agg[x_agg$Year %in% c(common),], 
                  aes(x = Year, y=y, group=Management), col = "black", linewidth = 1.0) 
    }
  } else {
    if(stand_agg_fun == "mean") {
      x_agg <- x |>
        dplyr::group_by(Climate, Management, Province, Year) |>
        dplyr::summarise(y = mean(.data[[var]], na.rm = TRUE),
                         sd.y = sd(.data[[var]], na.rm = TRUE),
                         q25 = quantile(.data[[var]], probs = 0.25, na.rm = TRUE),
                         q75 = quantile(.data[[var]], probs = 0.75, na.rm = TRUE),
                         n.y = n(), .groups="drop") |>
        dplyr::mutate(y_min = y - sd.y/sqrt(n.y)*1.96,
                      y_max = y + sd.y/sqrt(n.y)*1.96)
      g<-ggplot(x_agg)+
        geom_line(aes(x = Year, y=y, col=Management, group=Management), linewidth = linewidth)+
        geom_line(data = x_agg[x_agg$Year %in% common,], aes(x = Year, y=y, group=Management), col = "black", linewidth = linewidth)+
        facet_grid(rows = vars(Climate), cols = vars(Province))
      if(se) g <- g+
        geom_ribbon(aes(x = Year, ymin=y_min, ymax = y_max, fill=Management, group=Management), alpha = alpha)
      if(quantile_range) g <- g+
        geom_ribbon(aes(x = Year, ymin=q25, ymax = q75, fill=Management, group=Management), alpha = alpha)
    } else if(stand_agg_fun == "sum") {
      x_agg <- x |>
        dplyr::group_by(Climate, Management, Province, Year) |>
        dplyr::summarise(y = sum(.data[[var]], na.rm = TRUE), .groups="drop")
      g<-ggplot(x_agg)+
        geom_line(aes(x = Year, y=y, col=Management, group=Management), linewidth = linewidth)+
        geom_line(data = x_agg[x_agg$Year %in% common,], aes(x = Year, y=y, group=Management), col = "black", linewidth = linewidth)+
        facet_grid(rows = vars(Climate), cols = vars(Province))
    }
  }
  # browser()
  g <- g+
    geom_vline(xintercept = "2011-2020", linetype="dashed")+
    annotate("rect", xmin = "2000", xmax = "2011-2020", ymin = -Inf, ymax = Inf, alpha = alpha)+
    scale_fill_brewer("Escenaris de gestió", type ="qual", palette = "Set1")+
    scale_color_brewer("Escenaris de gestió", type ="qual", palette = "Set1")+
    # scale_x_continuous(limits = c(2000,2105), expand = c(0,0))+
    xlab("")+ylab(ylab)+theme_bw()+theme(panel.spacing = unit(1,"lines"))
  return(g)
}
plot_volume_var_FORMES <- function(x, var, ylab, 
                            aggregateProvinces = TRUE, 
                            provinces = c("Barcelona", "Girona", "Lleida", "Tarragona"),
                            scenarios = c("BAU", "AMF", "RSB", "ASEA", "ACG", "NOG")) {
  x <- x |>
    filter(Province %in% provinces, Management %in% scenarios)
  
  if(aggregateProvinces) {
    x <- x |>
      dplyr::group_by(Climate, Management, Decade) |>
      dplyr::summarise(Growth = sum(Growth), 
                       Mortality = sum(Mortality), 
                       Extraction = sum(Extraction),
                       Demand = sum(Demand),
                       Demand_ha_y = mean(Demand_ha_y),
                       Growth_ha_y = mean(Growth_ha_y),
                       Extraction_ha_y = mean(Extraction_ha_y),
                       ExtractionTarget = sum(ExtractionTarget),
                       Mortality_ha_y = mean(Mortality_ha_y),.groups = "drop") |>
      dplyr::mutate(ExtractionRate = 100*Extraction/Growth,
                    SatisfactionRate = 100*ExtractionTarget/Demand)
    x$y <- x[[var]]
    g<-ggplot(x, aes(x = Decade, y=y, fill=Management))+
      geom_bar(stat="identity", position=position_dodge())+
      geom_vline(xintercept = 2.5, linetype="dashed")+
      annotate("rect", xmin = 0.5, xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = 0.3)+
      scale_fill_brewer("Escenaris de gestió", type = "qual", palette = "Set1")+
      facet_wrap(vars(Climate), nrow = 1)+xlab("")+ylab(ylab)+theme_bw()+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  } else {
    x$y <- x[[var]]
    g<-ggplot(x, aes(x = Decade, y=y, fill=Management))+
      geom_bar(stat="identity", position=position_dodge())+
      geom_vline(xintercept = 2.5, linetype="dashed")+
      annotate("rect", xmin = 0.5, xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = 0.3)+
      scale_fill_brewer("Escenaris de gestió", type = "qual", palette = "Set1")+
      facet_grid(rows = vars(Climate), cols = vars(Province))+
      xlab("")+ylab(ylab)+theme_bw()+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  }
  return(g)
}
plot_var_medfate <- function(x, var, ylab, 
                     aggregateProvinces = TRUE, stand_agg_fun = "mean", 
                     provinces = c("Barcelona", "Girona", "Lleida", "Tarragona"),
                     scenarios = c("BAU", "AMF", "RSB", "ASEA", "ACG", "NOG"),
                     linewidth = 0.7, alpha = 0.2, se = FALSE, quantile_range = FALSE) {
  x <- x |>
    filter(Province %in% provinces, Management %in% scenarios)
  
  common = 2000:2020
  
  if(aggregateProvinces) {
    if(stand_agg_fun == "mean") {
      x_agg <- x |>
        dplyr::group_by(Climate, Management, Year) |>
        dplyr::summarise(y = mean(.data[[var]], na.rm = TRUE),
                         sd.y = sd(.data[[var]], na.rm = TRUE),
                         n.y = n(),
                         q25 = quantile(.data[[var]], probs = 0.25, na.rm = TRUE),
                         q75 = quantile(.data[[var]], probs = 0.75, na.rm = TRUE),
                         .groups="drop") |>
        dplyr::mutate(y_min = y - sd.y/sqrt(n.y)*1.96,
                      y_max = y + sd.y/sqrt(n.y)*1.96)
      g<- ggplot(x_agg)+
        facet_wrap(vars(Climate), nrow = 1)+
        geom_line(aes(x = Year, y=y, col=Management), linewidth = 1.0)+
        geom_line(data = x_agg[x_agg$Year %in% common,], 
                  aes(x = Year, y=y), col = "black", linewidth = 1.0)
      if(se) g <- g+
        geom_ribbon(aes(x = Year, ymin=y_min, ymax = y_max, fill=Management), alpha = alpha)
      if(quantile_range) g <- g+
        geom_ribbon(aes(x = Year, ymin=q25, ymax = q75, fill=Management), alpha = alpha)
    } else if(stand_agg_fun == "sum") {
      x_agg <- x |>
        dplyr::group_by(Climate, Management, Year) |>
        dplyr::summarise(y = sum(.data[[var]], na.rm = TRUE), .groups="drop")
      g<- ggplot(x_agg)+
        facet_wrap(vars(Climate), nrow = 1)+
        geom_line(aes(x = Year, y=y, col=Management), linewidth = 1.0)+
        geom_line(data = x_agg[x_agg$Year %in% common,], 
                  aes(x = Year, y=y), col = "black", linewidth = 1.0)
    }
  } else {
    if(stand_agg_fun == "mean") {
      x_agg <- x |>
        dplyr::group_by(Climate, Management, Province, Year) |>
        dplyr::summarise(y = mean(.data[[var]], na.rm = TRUE),
                         sd.y = sd(.data[[var]], na.rm = TRUE),
                         q25 = quantile(.data[[var]], probs = 0.25, na.rm = TRUE),
                         q75 = quantile(.data[[var]], probs = 0.75, na.rm = TRUE),
                         n.y = n(), .groups="drop") |>
        dplyr::mutate(y_min = y - sd.y/sqrt(n.y)*1.96,
                      y_max = y + sd.y/sqrt(n.y)*1.96)
      g<-ggplot(x_agg)+
        geom_line(aes(x = Year, y=y, col=Management), linewidth = linewidth)+
        geom_line(data = x_agg[x_agg$Year %in% common,], aes(x = Year, y=y), col = "black", linewidth = linewidth)+
        facet_grid(rows = vars(Climate), cols = vars(Province))
      if(se) g <- g+
        geom_ribbon(aes(x = Year, ymin=y_min, ymax = y_max, fill=Management), alpha = alpha)
      if(quantile_range) g <- g+
        geom_ribbon(aes(x = Year, ymin=q25, ymax = q75, fill=Management), alpha = alpha)
    } else if(stand_agg_fun == "sum") {
      x_agg <- x |>
        dplyr::group_by(Climate, Management, Province, Year) |>
        dplyr::summarise(y = sum(.data[[var]], na.rm = TRUE), .groups="drop")
      g<-ggplot(x_agg)+
        geom_line(aes(x = Year, y=y, col=Management), linewidth = linewidth)+
        geom_line(data = x_agg[x_agg$Year %in% common,], aes(x = Year, y=y), col = "black", linewidth = linewidth)+
        facet_grid(rows = vars(Climate), cols = vars(Province))
    }
  }
  g <- g+
    geom_vline(xintercept = 2020, linetype="dashed")+
    annotate("rect", xmin = 2000, xmax = 2020, ymin = -Inf, ymax = Inf, alpha = alpha)+
    scale_fill_brewer("Escenaris de gestió", type ="qual", palette = "Set1")+
    scale_color_brewer("Escenaris de gestió", type ="qual", palette = "Set1")+
    scale_x_continuous(limits = c(2000,2105), expand = c(0,0))+
    xlab("")+ylab(ylab)+theme_bw()+theme(panel.spacing = unit(1,"lines"))
  return(g)
}
plot_volume_var_medfate <- function(x, var, ylab, 
                            aggregateProvinces = TRUE, 
                            provinces = c("Barcelona", "Girona", "Lleida", "Tarragona"),
                            scenarios = c("BAU", "AMF", "RSB", "ASEA", "ACG", "NOG")) {
  x <- x |>
    filter(Province %in% provinces, Management %in% scenarios)
  
  if(aggregateProvinces) {
    x <- x |>
      dplyr::group_by(Climate, Management, Decade) |>
      dplyr::summarise(Growth = sum(Growth), 
                       Mortality = sum(Mortality), 
                       Extraction = sum(Extraction),
                       Demand = sum(Demand),
                       Demand_ha_y = mean(Demand_ha_y),
                       Growth_ha_y = mean(Growth_ha_y),
                       Extraction_ha_y = mean(Extraction_ha_y),
                       ExtractionTarget = sum(ExtractionTarget),
                       Mortality_ha_y = mean(Mortality_ha_y) , .groups = "drop") |>
      dplyr::mutate(ExtractionRate = 100*Extraction/Growth,
                    SatisfactionRate = 100*ExtractionTarget/Demand)
    x$y <- x[[var]]
    g<-ggplot(x, aes(x = Decade, y=y, fill=Management))+
      geom_bar(stat="identity", position=position_dodge())+
      geom_vline(xintercept = 2.5, linetype="dashed")+
      annotate("rect", xmin = 0.5, xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = 0.3)+
      scale_fill_brewer("Escenaris de gestió", type = "qual", palette = "Set1")+
      facet_wrap(vars(Climate), nrow = 1)+xlab("")+ylab(ylab)+theme_bw()+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  } else {
    x$y <- x[[var]]
    g<-ggplot(x, aes(x = Decade, y=y, fill=Management))+
      geom_bar(stat="identity", position=position_dodge())+
      geom_vline(xintercept = 2.5, linetype="dashed")+
      annotate("rect", xmin = 0.5, xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = 0.3)+
      scale_fill_brewer("Escenaris de gestió", type = "qual", palette = "Set1")+
      facet_grid(rows = vars(Climate), cols = vars(Province))+
      xlab("")+ylab(ylab)+theme_bw()+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  }
  return(g)
}

# settings of plots
target_scenarios <- c("BAU","AMF","ACG", "RSB", "ASEA")
target_provinces <- c("Barcelona", "Girona", "Lleida", "Tarragona")
aggregateProvinces <- T
se <- T

save_dir <- "Plots/comparison_models_2/"

##### calculate area and add variables per ha/y to VOL ##### 
nfiplot = readRDS("Rdata/nfiplot.rds")

# create a vector for areas of provinces
area_prov <- c(
  "Barcelona"=sum(st_drop_geometry(nfiplot[nfiplot$Provincia == "08", "area"])),
  "Girona" = sum(st_drop_geometry(nfiplot[nfiplot$Provincia == "17", "area"])),
  "Tarragona"=sum(st_drop_geometry(nfiplot[nfiplot$Provincia == "25", "area"])),
  "Lleida"=sum(st_drop_geometry(nfiplot[nfiplot$Provincia == "43", "area"]))
)


# MEDFATE
# Demand/ha/y
VOL_medfate$Demand_ha_y <- 0

VOL_medfate[VOL_medfate$Province == "Barcelona", "Demand_ha_y"] <- VOL_medfate[VOL_medfate$Province == "Barcelona", "Demand"]/area_prov["Barcelona"]/10
VOL_medfate[VOL_medfate$Province == "Girona", "Demand_ha_y"]<- VOL_medfate[VOL_medfate$Province == "Girona", "Demand"]/area_prov["Girona"]/10
VOL_medfate[VOL_medfate$Province == "Lleida", "Demand_ha_y"]<- VOL_medfate[VOL_medfate$Province == "Lleida", "Demand"]/area_prov["Lleida"]/10
VOL_medfate[VOL_medfate$Province == "Tarragona", "Demand_ha_y"]<- VOL_medfate[VOL_medfate$Province == "Tarragona", "Demand"]/area_prov["Tarragona"]/10

# Growth/ha/y
VOL_medfate$Growth_ha_y <- 0

VOL_medfate[VOL_medfate$Province == "Barcelona", "Growth_ha_y"] <- VOL_medfate[VOL_medfate$Province == "Barcelona", "Growth"]/area_prov["Barcelona"]/10
VOL_medfate[VOL_medfate$Province == "Girona", "Growth_ha_y"]<- VOL_medfate[VOL_medfate$Province == "Girona", "Growth"]/area_prov["Girona"]/10
VOL_medfate[VOL_medfate$Province == "Lleida", "Growth_ha_y"]<- VOL_medfate[VOL_medfate$Province == "Lleida", "Growth"]/area_prov["Lleida"]/10
VOL_medfate[VOL_medfate$Province == "Tarragona", "Growth_ha_y"]<- VOL_medfate[VOL_medfate$Province == "Tarragona", "Growth"]/area_prov["Tarragona"]/10

# Extraction/ha/y
VOL_medfate$Extraction_ha_y <- 0

VOL_medfate[VOL_medfate$Province == "Barcelona", "Extraction_ha_y"] <- VOL_medfate[VOL_medfate$Province == "Barcelona", "Extraction"]/area_prov["Barcelona"]/10
VOL_medfate[VOL_medfate$Province == "Girona", "Extraction_ha_y"]<- VOL_medfate[VOL_medfate$Province == "Girona", "Extraction"]/area_prov["Girona"]/10
VOL_medfate[VOL_medfate$Province == "Lleida", "Extraction_ha_y"]<- VOL_medfate[VOL_medfate$Province == "Lleida", "Extraction"]/area_prov["Lleida"]/10
VOL_medfate[VOL_medfate$Province == "Tarragona", "Extraction_ha_y"]<- VOL_medfate[VOL_medfate$Province == "Tarragona", "Extraction"]/area_prov["Tarragona"]/10

# Mortalitat/ha/y
VOL_medfate$Mortality_ha_y <- 0

VOL_medfate[VOL_medfate$Province == "Barcelona", "Mortality_ha_y"] <- VOL_medfate[VOL_medfate$Province == "Barcelona", "Mortality"]/area_prov["Barcelona"]/10
VOL_medfate[VOL_medfate$Province == "Girona", "Mortality_ha_y"]<- VOL_medfate[VOL_medfate$Province == "Girona", "Mortality"]/area_prov["Girona"]/10
VOL_medfate[VOL_medfate$Province == "Lleida", "Mortality_ha_y"]<- VOL_medfate[VOL_medfate$Province == "Lleida", "Mortality"]/area_prov["Lleida"]/10
VOL_medfate[VOL_medfate$Province == "Tarragona", "Mortality_ha_y"]<- VOL_medfate[VOL_medfate$Province == "Tarragona", "Mortality"]/area_prov["Tarragona"]/10



# FORMES
# Demand/ha/y
VOL_FORMES$Demand_ha_y <- 0

VOL_FORMES[VOL_FORMES$Province == "Barcelona", "Demand_ha_y"] <- VOL_FORMES[VOL_FORMES$Province == "Barcelona", "Demand"]/area_prov["Barcelona"]/10
VOL_FORMES[VOL_FORMES$Province == "Girona", "Demand_ha_y"]<- VOL_FORMES[VOL_FORMES$Province == "Girona", "Demand"]/area_prov["Girona"]/10
VOL_FORMES[VOL_FORMES$Province == "Lleida", "Demand_ha_y"]<- VOL_FORMES[VOL_FORMES$Province == "Lleida", "Demand"]/area_prov["Lleida"]/10
VOL_FORMES[VOL_FORMES$Province == "Tarragona", "Demand_ha_y"]<- VOL_FORMES[VOL_FORMES$Province == "Tarragona", "Demand"]/area_prov["Tarragona"]/10

# Growth/ha/y
VOL_FORMES$Growth_ha_y <- 0

VOL_FORMES[VOL_FORMES$Province == "Barcelona", "Growth_ha_y"] <- VOL_FORMES[VOL_FORMES$Province == "Barcelona", "Growth"]/area_prov["Barcelona"]/10
VOL_FORMES[VOL_FORMES$Province == "Girona", "Growth_ha_y"]<- VOL_FORMES[VOL_FORMES$Province == "Girona", "Growth"]/area_prov["Girona"]/10
VOL_FORMES[VOL_FORMES$Province == "Lleida", "Growth_ha_y"]<- VOL_FORMES[VOL_FORMES$Province == "Lleida", "Growth"]/area_prov["Lleida"]/10
VOL_FORMES[VOL_FORMES$Province == "Tarragona", "Growth_ha_y"]<- VOL_FORMES[VOL_FORMES$Province == "Tarragona", "Growth"]/area_prov["Tarragona"]/10

# Extraction/ha/y
VOL_FORMES$Extraction_ha_y <- 0

VOL_FORMES[VOL_FORMES$Province == "Barcelona", "Extraction_ha_y"] <- VOL_FORMES[VOL_FORMES$Province == "Barcelona", "Extraction"]/area_prov["Barcelona"]/10
VOL_FORMES[VOL_FORMES$Province == "Girona", "Extraction_ha_y"]<- VOL_FORMES[VOL_FORMES$Province == "Girona", "Extraction"]/area_prov["Girona"]/10
VOL_FORMES[VOL_FORMES$Province == "Lleida", "Extraction_ha_y"]<- VOL_FORMES[VOL_FORMES$Province == "Lleida", "Extraction"]/area_prov["Lleida"]/10
VOL_FORMES[VOL_FORMES$Province == "Tarragona", "Extraction_ha_y"]<- VOL_FORMES[VOL_FORMES$Province == "Tarragona", "Extraction"]/area_prov["Tarragona"]/10

# Mortalitat/ha/y
VOL_FORMES$Mortality_ha_y <- 0

VOL_FORMES[VOL_FORMES$Province == "Barcelona", "Mortality_ha_y"] <- VOL_FORMES[VOL_FORMES$Province == "Barcelona", "Mortality"]/area_prov["Barcelona"]/10
VOL_FORMES[VOL_FORMES$Province == "Girona", "Mortality_ha_y"]<- VOL_FORMES[VOL_FORMES$Province == "Girona", "Mortality"]/area_prov["Girona"]/10
VOL_FORMES[VOL_FORMES$Province == "Lleida", "Mortality_ha_y"]<- VOL_FORMES[VOL_FORMES$Province == "Lleida", "Mortality"]/area_prov["Lleida"]/10
VOL_FORMES[VOL_FORMES$Province == "Tarragona", "Mortality_ha_y"]<- VOL_FORMES[VOL_FORMES$Province == "Tarragona", "Mortality"]/area_prov["Tarragona"]/10

VOL_FORMES[VOL_FORMES$Management == "ACG" & !VOL_FORMES$Decade %in% c("2001-2010","2011-2020"), "Demand_ha_y"] = 0 


# remove year 2000 in FORMES
VOL_FORMES <- VOL_FORMES %>% filter(!VOL_FORMES$Decade == "2000")


# 1 Area basal (m2/ha) - Linies (amb punts a formes) #####
variable <- "BasalArea"
titolo <- "Àrea basal (m2/ha)"
titolo <- expression(paste("Area basal (" ~ m^3 ,"/ha)"))

p1_med<-plot_var_medfate(ALL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORDYN") + theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=12), plot.title = element_text(size=22)) 

p1_for<-plot_var_FORMES(ALL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORMES") + theme(strip.text.x = element_text(size = 18),
                                 axis.text.y = element_text(size=12),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 legend.text = element_text(size=12),
                                 legend.title = element_text(size=15),
                                 legend.key.size = unit(0.6, 'cm'))
leg = get_legend(p1_for)

p1_for<-plot_var_FORMES(ALL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORMES") + theme(strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22))
# limits 
data_med <- ggplot_build(p1_med)
data_for <- ggplot_build(p1_for)

min_global <- min(min(data_med$plot$data$y), min(data_for$plot$data$y))
max_global <- max(max(data_med$plot$data$y), max(data_for$plot$data$y))

p1_med<-plot_var_medfate(ALL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORDYN") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                  axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                  axis.title.y = element_text(size = 18),
                                  plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  ylim(min_global, max_global)
p1_for<-plot_var_FORMES(ALL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORMES") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  ylim(min_global, max_global) +scale_x_discrete(expand = expansion(add = c(0, 0.5)))
p1_for

# combine 
p1 <- plot_grid(p1_med, p1_for, nrow = 2)
pp_leg = plot_grid(p1, leg, ncol=2, rel_widths = c(0.82, 0.18))
ggsave(paste0(save_dir, paste0(variable,".png")), pp_leg, width = 12, height = 10)

rm(p1_med, p1_for, p1, pp_leg)
gc()
#2 Densitat adults (ind/ha) - Linies (amb punts a formes) #####
variable <- "TreeDensity"
titolo <- "Densitat (ind/ha)"

p1_med<-plot_var_medfate(ALL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORDYN") + theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=12), plot.title = element_text(size=22)) 

p1_for<-plot_var_FORMES(ALL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORMES") + theme(strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 legend.text = element_text(size=12),
                                 legend.title = element_text(size=15),
                                 legend.key.size = unit(0.6, 'cm'))
leg = get_legend(p1_for)

p1_for<-plot_var_FORMES(ALL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORMES") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22))

# limits 
data_med <- ggplot_build(p1_med)
data_for <- ggplot_build(p1_for)

min_global <- min(min(data_med$plot$data$y), min(data_for$plot$data$y))
max_global <- max(max(data_med$plot$data$y), max(data_for$plot$data$y))

p1_med<-plot_var_medfate(ALL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORDYN") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  ylim(min_global, max_global)
p1_for<-plot_var_FORMES(ALL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORMES") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  ylim(min_global, max_global)+scale_x_discrete(expand = expansion(add = c(0, 0.5)))


# combine 
p1 <- plot_grid(p1_med, p1_for, nrow = 2)
pp_leg = plot_grid(p1, leg, ncol=2, rel_widths = c(0.82, 0.18))
ggsave(paste0(save_dir, paste0(variable,".png")), pp_leg, width = 12, height = 10)

rm(p1_med, p1_for, p1, pp_leg)
gc()
#3 DBH mitjà dels adults (cm)- Linies (amb punts a formes) #####
variable <- "meanDBH"
titolo <- "Diàmetre (cm)"
p1_med<-plot_var_medfate(ALL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORDYN") + theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=12), plot.title = element_text(size=22)) 

p1_for<-plot_var_FORMES(ALL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORMES") + theme(strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 legend.text = element_text(size=12),
                                 legend.title = element_text(size=15),
                                 legend.key.size = unit(0.6, 'cm'))
leg = get_legend(p1_for)

p1_for<-plot_var_FORMES(ALL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORMES") + theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12), plot.title = element_text(size=22))

# limits 
data_med <- ggplot_build(p1_med)
data_for <- ggplot_build(p1_for)

min_global <- min(min(data_med$plot$data$y), min(data_for$plot$data$y))
max_global <- max(max(data_med$plot$data$y), max(data_for$plot$data$y))

p1_med<-plot_var_medfate(ALL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORDYN") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  ylim(min_global, max_global)
p1_for<-plot_var_FORMES(ALL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORMES") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  ylim(min_global, max_global)+scale_x_discrete(expand = expansion(add = c(0, 0.5)))


# combine 
p1 <- plot_grid(p1_med, p1_for, nrow = 2)
pp_leg = plot_grid(p1, leg, ncol=2, rel_widths = c(0.82, 0.18))
ggsave(paste0(save_dir, paste0(variable,".png")), pp_leg, width = 12, height = 10)

rm(p1_med, p1_for, p1, pp_leg)
gc()
#4 CV DBH dels adults - Linies (amb punts a formes) #####
variable <- "cvDBH"
titolo <- "Coeficient de variació de DBH"
p1_med<-plot_var_medfate(ALL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORDYN") + theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=12), plot.title = element_text(size=22)) 

p1_for<-plot_var_FORMES(ALL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORMES") + theme(strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 legend.text = element_text(size=12),
                                 legend.title = element_text(size=15),
                                 legend.key.size = unit(0.6, 'cm'))
leg = get_legend(p1_for)

p1_for<-plot_var_FORMES(ALL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORMES") + theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12), plot.title = element_text(size=22))

# limits 
data_med <- ggplot_build(p1_med)
data_for <- ggplot_build(p1_for)

min_global <- min(min(data_med$plot$data$y), min(data_for$plot$data$y))
max_global <- max(max(data_med$plot$data$y), max(data_for$plot$data$y))

p1_med<-plot_var_medfate(ALL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORDYN") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  ylim(min_global, max_global)
p1_for<-plot_var_FORMES(ALL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORMES") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  ylim(min_global, max_global)+scale_x_discrete(expand = expansion(add = c(0, 0.5)))


# combine 
p1 <- plot_grid(p1_med, p1_for, nrow = 2)
pp_leg = plot_grid(p1, leg, ncol=2, rel_widths = c(0.82, 0.18))
ggsave(paste0(save_dir, paste0(variable,".png")), pp_leg, width = 12, height = 10)

rm(p1_med, p1_for, p1, pp_leg)
gc()
#5 Volum en stock (m3/ha) - Linies (amb punts a formes) #####
variable <- "AllVolume"
titolo <- expression(paste("Volum en peu (" ~ m^3 ,"/ha)"))


p1_med<-plot_var_medfate(ALL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORDYN") + theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=12), plot.title = element_text(size=22)) 

p1_for<-plot_var_FORMES(ALL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORMES") + theme(strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 legend.text = element_text(size=12),
                                 legend.title = element_text(size=15),
                                 legend.key.size = unit(0.6, 'cm'))
leg = get_legend(p1_for)

p1_for<-plot_var_FORMES(ALL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORMES") + theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12), plot.title = element_text(size=22))

# limits 
data_med <- ggplot_build(p1_med)
data_for <- ggplot_build(p1_for)

min_global <- min(min(data_med$plot$data$y), min(data_for$plot$data$y))
max_global <- max(max(data_med$plot$data$y), max(data_for$plot$data$y))

p1_med<-plot_var_medfate(ALL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORDYN") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  ylim(min_global, max_global)
p1_for<-plot_var_FORMES(ALL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORMES") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  ylim(min_global, max_global)+scale_x_discrete(expand = expansion(add = c(0, 0.5)))


# combine 
p1 <- plot_grid(p1_med, p1_for, nrow = 2)
pp_leg = plot_grid(p1, leg, ncol=2, rel_widths = c(0.82, 0.18))
ggsave(paste0(save_dir, paste0(variable,".png")), pp_leg, width = 12, height = 10)

rm(p1_med, p1_for, p1, pp_leg)
gc()
#6 LAI (m2/m2) - Linies (amb punts a formes) #####
variable <- "Tree_lai"
titolo <- "LAI Arbre (m2/m2)"
titolo <- expression(paste("LAI Arbre (" ~ m^2 ,"/", ~ m^2, ")"))
p1_med<-plot_var_medfate(ALL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORDYN") + theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=12), plot.title = element_text(size=22))

p1_for<-plot_var_FORMES(ALL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORMES") + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12), plot.title = element_text(size=22))
leg = get_legend(p1_for)

p1_for<-plot_var_FORMES(ALL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORMES") + theme(legend.position = "none",axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12), plot.title = element_text(size=22))

# limits
data_med <- ggplot_build(p1_med)
data_for <- ggplot_build(p1_for)

min_global <- min(min(data_med$plot$data$y), min(data_for$plot$data$y))
max_global <- max(max(data_med$plot$data$y), max(data_for$plot$data$y))

p1_med<-plot_var_medfate(ALL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORDYN") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  ylim(min_global, max_global)
p1_for<-plot_var_FORMES(ALL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se) +
  labs(title = "FORMES") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  ylim(min_global, max_global)


# combine
p1 <- plot_grid(p1_med, p1_for, nrow = 2)
pp_leg = plot_grid(p1, leg, ncol=2, rel_widths = c(0.82, 0.18))
ggsave(paste0(save_dir, paste0(variable,".png")), pp_leg, width = 12, height = 10)

rm(p1_med, p1_for, p1, pp_leg)
gc()
#7 Demanda (m3/ha/any) – Barres (cada 10 anys) #####
target_scenarios <- c("BAU","AMF", "RSB", "ASEA", "ACG")

variable <- "Demand_ha_y"
titolo <- expression(paste("Demanda de fusta (" ~ m^3 ,"/ha/any)"))

p1_med<- plot_volume_var_medfate(VOL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
 labs(title = "FORDYN") + theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=12), plot.title = element_text(size=22))
p1_for<- plot_volume_var_FORMES(VOL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORMES") + theme(strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 legend.text = element_text(size=12),
                                 legend.title = element_text(size=15),
                                 legend.key.size = unit(0.6, 'cm'))

leg = get_legend(p1_for)

p1_for<- plot_volume_var_FORMES(VOL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORMES") + theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12), plot.title = element_text(size=22))

# limits
data_med <- ggplot_build(p1_med)
data_for <- ggplot_build(p1_for)

min_global <- min(min(data_med$plot$data$y), min(data_for$plot$data$y))
max_global <- max(max(data_med$plot$data$y), max(data_for$plot$data$y))

p1_med<- plot_volume_var_medfate(VOL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORDYN") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  ylim(min_global, max_global)
p1_for<- plot_volume_var_FORMES(VOL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORMES") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  ylim(min_global, max_global)


# combine
p1 <- plot_grid(p1_med, p1_for, nrow = 2)
pp_leg = plot_grid(p1, leg, ncol=2, rel_widths = c(0.82, 0.18))
ggsave(paste0(save_dir, paste0(variable,".png")), pp_leg, width = 12, height = 10)

rm(p1_med, p1_for, p1, pp_leg)
gc()


#8 Creixement (m3/ha/any) - Variació + extracció - Barres (cada 10 anys) #####
target_scenarios <- c("BAU","AMF", "RSB","ACG", "ASEA")

variable <- "Growth_ha_y"
titolo <- "Creixement (m3/ha/any)"
titolo <- expression(paste("Creixement (" ~ m^3 ,"/ha/any)"))

p1_med<- plot_volume_var_medfate(VOL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORDYN") + theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=12), plot.title = element_text(size=22)) 
p1_for<- plot_volume_var_FORMES(VOL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORMES") + theme(strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 legend.text = element_text(size=12),
                                 legend.title = element_text(size=15),
                                 legend.key.size = unit(0.6, 'cm'))

leg = get_legend(p1_for)

p1_for<- plot_volume_var_FORMES(VOL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORMES") + theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12), plot.title = element_text(size=22))

# limits 
data_med <- ggplot_build(p1_med)
data_for <- ggplot_build(p1_for)

min_global <- min(min(data_med$plot$data$y), min(data_for$plot$data$y))
max_global <- max(max(data_med$plot$data$y), max(data_for$plot$data$y))

p1_med<- plot_volume_var_medfate(VOL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORDYN") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  coord_cartesian(ylim=c(min_global, max_global+0.5))

p1_for<- plot_volume_var_FORMES(VOL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORMES") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  coord_cartesian(ylim=c(min_global, max_global+0.5))


# combine 
p1 <- plot_grid(p1_med, p1_for, nrow = 2)
pp_leg = plot_grid(p1, leg, ncol=2, rel_widths = c(0.82, 0.18))
ggsave(paste0(save_dir, paste0(variable,".png")), pp_leg, width = 12, height = 10)

rm(p1_med, p1_for, p1, pp_leg)
gc()


#9 Extracció (m3/ha/any) - Barres (cada 10 anys) #####
target_scenarios <- c("BAU","AMF", "RSB","ACG", "ASEA")

variable <- "Extraction_ha_y"
titolo <- "Extracció de fusta (m3/ha/any)"
titolo <- expression(paste("Extracció de fusta (" ~ m^3 ,"/ha/any)"))

p1_med<- plot_volume_var_medfate(VOL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORDYN") + theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=12), plot.title = element_text(size=22)) 
p1_for<- plot_volume_var_FORMES(VOL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORMES") + theme(strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 legend.text = element_text(size=12),
                                 legend.title = element_text(size=15),
                                 legend.key.size = unit(0.6, 'cm'))

leg = get_legend(p1_for)

p1_for<- plot_volume_var_FORMES(VOL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORMES") + theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12), plot.title = element_text(size=22))

# limits 
data_med <- ggplot_build(p1_med)
data_for <- ggplot_build(p1_for)

min_global <- min(min(data_med$plot$data$y), min(data_for$plot$data$y))
max_global <- max(max(data_med$plot$data$y), max(data_for$plot$data$y))

p1_med<- plot_volume_var_medfate(VOL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORDYN") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  coord_cartesian(ylim=c(min_global, max_global+0.5))

p1_for<- plot_volume_var_FORMES(VOL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORMES") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  coord_cartesian(ylim=c(min_global, max_global+0.5))


# combine 
p1 <- plot_grid(p1_med, p1_for, nrow = 2)
pp_leg = plot_grid(p1, leg, ncol=2, rel_widths = c(0.82, 0.18))
ggsave(paste0(save_dir, paste0(variable,".png")), pp_leg, width = 12, height = 10)

rm(p1_med, p1_for, p1, pp_leg)
gc()


#10 Extraction rate (% del creixement) - Barres (cada 10 anys) #####
target_scenarios <- c("BAU","AMF", "RSB","ACG", "ASEA")
# VOL_medfate$ExtractionRate
variable <- "ExtractionRate"
titolo <- "Taxa d'extracció (% del creixement)"

p1_med<- plot_volume_var_medfate(VOL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORDYN") + theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=12), plot.title = element_text(size=22)) 
p1_for<- plot_volume_var_FORMES(VOL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORMES") + theme(strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 legend.text = element_text(size=12),
                                 legend.title = element_text(size=15),
                                 legend.key.size = unit(0.6, 'cm'))

leg = get_legend(p1_for)

p1_for<- plot_volume_var_FORMES(VOL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORMES") + theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12), plot.title = element_text(size=22))

# limits 
data_med <- ggplot_build(p1_med)
data_for <- ggplot_build(p1_for)

min_global <- min(min(data_med$plot$data$y), min(data_for$plot$data$y))
max_global <- max(max(data_med$plot$data$y), max(data_for$plot$data$y))

p1_med<- plot_volume_var_medfate(VOL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORDYN") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  coord_cartesian(ylim=c(min_global, max_global+0.5)) +
  geom_hline(yintercept = 30, linetype="dotted")+
  geom_hline(yintercept = 40, linetype="dotted")+
  geom_hline(yintercept = 70, linetype="dotted")+
  geom_hline(yintercept = 100, linetype="dotted")

p1_for<- plot_volume_var_FORMES(VOL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORMES") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  coord_cartesian(ylim=c(min_global, max_global+0.5)) +
  geom_hline(yintercept = 30, linetype="dotted")+
  geom_hline(yintercept = 40, linetype="dotted")+
  geom_hline(yintercept = 70, linetype="dotted")+
  geom_hline(yintercept = 100, linetype="dotted")


# combine 
p1 <- plot_grid(p1_med, p1_for, nrow = 2)
pp_leg = plot_grid(p1, leg, ncol=2, rel_widths = c(0.82, 0.18))
ggsave(paste0(save_dir, paste0(variable,".png")), pp_leg, width = 12, height = 10)

rm(p1_med, p1_for, p1, pp_leg)
gc()

#11 Mortalitat (m3/ha/any) - Barres (cada 10 anys) #####
target_scenarios <- c("BAU","AMF", "RSB","ACG", "ASEA")

variable <- "Mortality_ha_y"
# titolo <- "Volum d'arbres morts (m3/ha/any)"
titolo <- expression(paste("Volum d'arbres morts (" ~ m^3 ,"/ha/any)"))

p1_med<- plot_volume_var_medfate(VOL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORDYN") + theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size=12), plot.title = element_text(size=22)) 
p1_for<- plot_volume_var_FORMES(VOL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORMES") + theme(strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 legend.text = element_text(size=12),
                                 legend.title = element_text(size=15),
                                 legend.key.size = unit(0.6, 'cm'))

leg = get_legend(p1_for)

p1_for<- plot_volume_var_FORMES(VOL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORMES") + theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12), plot.title = element_text(size=22))

# limits 
data_med <- ggplot_build(p1_med)
data_for <- ggplot_build(p1_for)

min_global <- min(min(data_med$plot$data$y), min(data_for$plot$data$y))
max_global <- max(max(data_med$plot$data$y), max(data_for$plot$data$y))

p1_med<- plot_volume_var_medfate(VOL_medfate, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORDYN") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  coord_cartesian(ylim=c(min_global, max_global+0.5))

p1_for<- plot_volume_var_FORMES(VOL_FORMES, variable, titolo, aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios) +
  labs(title = "FORMES") + theme(legend.position = "none",strip.text.x = element_text(size = 18),
                                 axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 12),
                                 axis.title.y = element_text(size = 18),
                                 plot.title = element_text(size=22),
                                 axis.text.y = element_text(size=12)) +
  coord_cartesian(ylim=c(min_global, max_global+0.5))


# combine 
p1 <- plot_grid(p1_med, p1_for, nrow = 2)
pp_leg = plot_grid(p1, leg, ncol=2, rel_widths = c(0.82, 0.18))
ggsave(paste0(save_dir, paste0(variable,".png")), pp_leg, width = 12, height = 10)

rm(p1_med, p1_for, p1, pp_leg)
gc()













