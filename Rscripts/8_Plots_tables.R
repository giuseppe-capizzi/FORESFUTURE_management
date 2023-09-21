library(ggplot2)
library(tidyverse)
library(medfate)

load_volume_table <- function(test = FALSE) {
  df <- data.frame()
  for(provinceName in c("Barcelona", "Girona","Lleida", "Tarragona")) {
    for(management_scen in c("BAU", "AMF", "RSB", "ASEA", "ACG", "NOG")) {
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

load_result_table <- function(test = FALSE) {
  
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
                   ACG_rcp45, ACG_rcp85,
                   NOG_rcp45, NOG_rcp85) |>
    dplyr::left_join(sf::st_drop_geometry(initial)[,c("id", "represented_area"), drop = FALSE], by = "id") |>
    dplyr::mutate(AllVolumeExt = AllVolume*represented_area,
                  VolumeStructureExt = VolumeStructure*represented_area,
                  CutStructureExt = CutStructure*represented_area,
                  CutAllExt = CutAll*represented_area,
                  CumulativeCutAllExt = CumulativeCutAll*represented_area) |>
    dplyr::mutate(Climate = toupper(Climate))
  return(ALL)
}

load_species_abundance_table <- function(test = FALSE) {
  df <- data.frame()
  for(management_scen in c("BAU", "AMF", "RSB", "ASEA", "ACG", "NOG")) {
    for(climate_model in c("mpiesm_rca4")) {
      for(climate_scen in c("rcp45", "rcp85")) {
        if(test) {
          bind_file <- paste0("Rdata/Test_species_abundance/Test_", management_scen, "_", climate_model,"_", climate_scen, ".rds")
        } else {
          bind_file <- paste0("Rdata/species_abundance/", management_scen, "_", climate_model,"_", climate_scen, ".rds")
        }
        if(file.exists(bind_file)) {
          df <- bind_rows(df, readRDS(file = bind_file)) 
        }
      }
    }
  }
  df <- df |>
    dplyr::mutate(TotalBiomass = Aerial + Roots)|>
    dplyr::left_join(SpParamsMED[,c("Name", "Genus", "Order","Family", "Group")], by=c("Species"="Name"))
    
  return(df)
}

plot_var <- function(x, var, ylab, 
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
    scale_fill_brewer("Management", type ="qual", palette = "Set1")+
    scale_color_brewer("Management", type ="qual", palette = "Set1")+
    scale_x_continuous(limits = c(2000,2105), expand = c(0,0))+
    xlab("")+ylab(ylab)+theme_bw()+theme(panel.spacing = unit(1,"lines"))
  return(g)
}

plot_mortality_pathway <- function(x, ylab, 
                                   cumulative = TRUE,
                                   provinces = c("Barcelona", "Girona", "Lleida", "Tarragona"),
                                   scenarios = c("BAU", "AMF", "RSB", "ASEA", "ACG", "NOG"),
                                   growth_form = "all") {
  x <- x |>
    filter(Province %in% provinces, Management %in% scenarios)
  
  if(cumulative) {
    if(growth_form == "tree") {
      x_agg <- x |>
        dplyr::select(Climate, Management, Province, Year, id,
                      CumulativeTreeDeadBiomass_other, CumulativeTreeDeadBiomass_starvation, CumulativeTreeDeadBiomass_dessication) |>
        dplyr::rename(DeadBiomass_other = CumulativeTreeDeadBiomass_other,
                      DeadBiomass_starvation = CumulativeTreeDeadBiomass_starvation,
                      DeadBiomass_dessication = CumulativeTreeDeadBiomass_dessication)
    } else if(growth_form == "shrub") {
      x_agg <- x |>
        dplyr::select(Climate, Management, Province, Year, id,
                      CumulativeShrubDeadBiomass_other, CumulativeShrubDeadBiomass_starvation, CumulativeShrubDeadBiomass_dessication) |>
        dplyr::rename(DeadBiomass_other = CumulativeShrubDeadBiomass_other,
                      DeadBiomass_starvation = CumulativeShrubDeadBiomass_starvation,
                      DeadBiomass_dessication = CumulativeShrubDeadBiomass_dessication)
    } else {
      x_agg <- x |>
        dplyr::select(Climate, Management, Province, Year, id,
                      CumulativeDeadBiomass_other, CumulativeDeadBiomass_starvation, CumulativeDeadBiomass_dessication) |>
        dplyr::rename(DeadBiomass_other = CumulativeDeadBiomass_other,
                      DeadBiomass_starvation = CumulativeDeadBiomass_starvation,
                      DeadBiomass_dessication = CumulativeDeadBiomass_dessication)
    }
  } else {
    if(growth_form == "tree") {
      x_agg <- x |>
        dplyr::select(Climate, Management, Province, Year, id,
                      TreeDeadBiomass_other, TreeDeadBiomass_starvation, TreeDeadBiomass_dessication) |>
        dplyr::rename(DeadBiomass_other = TreeDeadBiomass_other,
                      DeadBiomass_starvation = TreeDeadBiomass_starvation,
                      DeadBiomass_dessication = TreeDeadBiomass_dessication)
    } else if(growth_form == "shrub") {
      x_agg <- x |>
        dplyr::select(Climate, Management, Province, Year, id,
                      ShrubDeadBiomass_other, ShrubDeadBiomass_starvation, ShrubDeadBiomass_dessication) |>
        dplyr::rename(DeadBiomass_other = ShrubDeadBiomass_other,
                      DeadBiomass_starvation = ShrubDeadBiomass_starvation,
                      DeadBiomass_dessication = ShrubDeadBiomass_dessication)
    } else {
      x_agg <- x |>
        dplyr::select(Climate, Management, Province, Year, id,
                      DeadBiomass_other, DeadBiomass_starvation, DeadBiomass_dessication) |>
        dplyr::rename(DeadBiomass_other = DeadBiomass_other,
                      DeadBiomass_starvation = DeadBiomass_starvation,
                      DeadBiomass_dessication = DeadBiomass_dessication)
    }
  }
  x_agg <- x_agg |>
    dplyr::group_by(Climate, Management, Year) |>
    dplyr::summarise(DeadBiomass_other = mean(DeadBiomass_other, na.rm = TRUE),
                     DeadBiomass_starvation = mean(DeadBiomass_starvation, na.rm = TRUE),
                     DeadBiomass_dessication = mean(DeadBiomass_dessication, na.rm = TRUE), .groups="drop") |>
    dplyr::mutate(DeadBiomass_other_starvation = DeadBiomass_other + DeadBiomass_starvation,
                  DeadBiomass_all = DeadBiomass_other_starvation + DeadBiomass_dessication)

    ggplot(x_agg, aes(x = Year)) +
      geom_ribbon(aes(ymin = 0, ymax = DeadBiomass_other, fill = "Basal"))+
      geom_ribbon(aes(ymin = DeadBiomass_other, ymax = DeadBiomass_other_starvation, fill = "Starvation"))+
      geom_ribbon(aes(ymin = DeadBiomass_other_starvation, ymax = DeadBiomass_all, fill = "Dessication"))+
      geom_vline(xintercept = 2020, linetype="dashed")+
      annotate("rect", xmin = 2000, xmax = 2020, ymin = -Inf, ymax = Inf, alpha = 0.3)+
      scale_fill_manual(name = "Mortality", breaks = c("Basal", "Starvation", "Dessication"), 
                        values = c("Basal" = "blue", "Starvation"="red", "Dessication"="black"))+
      facet_grid(rows = vars(Climate), cols = vars(Management))+
      xlab("")+ylab(ylab)+theme_bw()+theme(panel.spacing = unit(1,"lines"))
}
plot_volume_var <- function(x, var, ylab, 
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
                       ExtractionTarget = sum(ExtractionTarget), .groups = "drop") |>
      dplyr::mutate(ExtractionRate = 100*Extraction/Growth,
                    SatisfactionRate = 100*ExtractionTarget/Demand)
    x$y <- x[[var]]
    g<-ggplot(x, aes(x = Decade, y=y, fill=Management))+
      geom_bar(stat="identity", position=position_dodge())+
      geom_vline(xintercept = 2.5, linetype="dashed")+
      annotate("rect", xmin = 0.5, xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = 0.3)+
      scale_fill_brewer("Management", type = "qual", palette = "Set1")+
      facet_wrap(vars(Climate), nrow = 1)+xlab("")+ylab(ylab)+theme_bw()+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  } else {
    x$y <- x[[var]]
    g<-ggplot(x, aes(x = Decade, y=y, fill=Management))+
      geom_bar(stat="identity", position=position_dodge())+
      geom_vline(xintercept = 2.5, linetype="dashed")+
      annotate("rect", xmin = 0.5, xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = 0.3)+
      scale_fill_brewer("Management", type = "qual", palette = "Set1")+
      facet_grid(rows = vars(Climate), cols = vars(Province))+
      xlab("")+ylab(ylab)+theme_bw()+
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  }
  return(g)
}


test <- FALSE
ALL <- load_result_table(test = test)
VOL <- load_volume_table(test = test)
SP <- load_species_abundance_table(test = test)


target_scenarios <- c("BAU","AMF", "RSB", "ASEA", "ACG", "NOG")
target_provinces <- c("Barcelona", "Girona", "Lleida", "Tarragona")
aggregateProvinces <- F
se <- T

p1<-plot_volume_var(VOL, "Growth", "Growth (m3)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios)
ggsave("Plots/GrowthByDecade_provinces.png", p1, width = 16, height = 8)

p2<- plot_volume_var(VOL, "Mortality", "Mortality (m3)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios)
ggsave("Plots/MortalityByDecade_provinces.png", p2, width = 16, height = 8)

p3<- plot_volume_var(VOL, "Extraction", "Extraction (m3)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios)
ggsave("Plots/ExtractionByDecade_provinces.png", p3, width = 16, height = 8)

p4<- plot_volume_var(VOL, "ExtractionRate", "Extraction rate (% of growth)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios)+
  geom_hline(yintercept = 30, linetype="dotted")+
  geom_hline(yintercept = 40, linetype="dotted")+
  geom_hline(yintercept = 70, linetype="dotted")+
  geom_hline(yintercept = 100, linetype="dotted")
ggsave("Plots/ExtractionRateByDecade_provinces.png", p4, width = 16, height = 8)

p5<- plot_volume_var(VOL, "ExtractionTarget", "Extraction target species (m3)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios)
ggsave("Plots/ExtractionTargetByDecade_provinces.png", p5, width = 16, height = 8)

p6<- plot_volume_var(VOL, "Demand", "Nominal demand (m3)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios)
ggsave("Plots/NominalDemandByDecade_provinces.png", p6, width = 16, height = 8)

p7<- plot_volume_var(VOL, "SatisfactionRate", "Satisfaction (% of demand)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios)+
  geom_hline(yintercept = 100, linetype="dotted")
ggsave("Plots/SatisfactionRate_provinces.png", p7, width = 16, height = 8)

rm(p1, p2, p3, p4, p5, p6, p7)
gc()


# Climate
plot_var(ALL, "Precipitation", "Annual precipitation (mm/yr)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
plot_var(ALL, "Pdaymax", "Maximum daily precipitation (mm/day)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
plot_var(ALL, "PET", "Annual PET (mm/yr)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
plot_var(ALL, "PPET", "Moisture index", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
plot_var(ALL, "CumulativePPET", "Cumulative moisture index", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)

# Structure
p1<-plot_var(ALL, "BasalArea", "Stand basal area (m2/ha), excl. saplings", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/BasalArea_provinces.png", p1, width = 16, height = 8)

p2 <- plot_var(ALL, "TreeDensity", "Tree density (ind/ha), excl. saplings", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/TreeDensity_provinces.png", p2, width = 16, height = 8)
# plot_var(ALL, "meanDBH", "Mean diameter (cm)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
# plot_var(ALL, "meanHeight", "Mean height (cm)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)

p3 <-plot_var(ALL, "QMD", "Quadratic mean diameter (cm)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/QMD_provinces.png", p3, width = 16, height = 8)

p4<-plot_var(ALL, "cvDBH", "Coeffient of variation of DBH (excl. saplings)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/cvDBH_provinces.png", p4, width = 16, height = 8)

p5 <-plot_var(ALL, "TreeRichness", "Tree species richness", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/TreeRichness_provinces.png", p5, width = 16, height = 8)

p6<-plot_var(ALL, "ShrubRichness", "Shrub species richness", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/ShrubRichness_provinces.png", p6, width = 16, height = 8)

p7<-plot_var(ALL, "ShrubCover", "Overall shrub cover (%)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/ShrubCover_provinces.png", p7, width = 16, height = 8)

p8<-plot_var(ALL, "LAI_max", "Woody LAI (m2/m2)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/LAImax_provinces.png", p8, width = 16, height = 8)

p9 <- plot_var(ALL, "Tree_lai", "Tree LAI (m2/m2)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/TreeLAI_provinces.png", p9, width = 16, height = 8)

p10 <- plot_var(ALL, "Shrub_lai", "Shrub LAI (m2/m2)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/ShrubLAI_provinces.png", p10, width = 16, height = 8)

p11 <- plot_var(ALL, "Herb_lai", "Herb LAI (m2/m2)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/HerbLAI_provinces.png", p11, width = 16, height = 8)

p12<-plot_var(ALL, "Total_lai", "Total LAI (m2/m2)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/TotalLAI_provinces.png", p12, width = 16, height = 8)

# Function
p13<-plot_var(ALL, "GrossPrimaryProduction", "Gross Primary Production (gC/m2/yr)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/GPP_provinces.png", p13, width = 16, height = 8)

p14<-plot_var(ALL, "NetPrimaryProduction", "Net Primary Production (gC/m2/yr)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/NPP_provinces.png", p14, width = 16, height = 8)

p15<-plot_var(ALL, "WaterUseEfficiency", "Water Use Efficiency (gC/mm)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/WUE_provinces.png", p15, width = 16, height = 8)

p16<-plot_var(ALL, "CarbonUseEfficiency", "Carbon Use Efficiency (gC synthesis/gC)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/CUE_provinces.png", p16, width = 16, height = 8)

p17<-plot_var(ALL, "StemPLC", "Maximum annual Stem PLC ([0-1])", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/StemPLC_provinces.png", p17, width = 16, height = 8)

p18<-plot_var(ALL, "PlantStress", "Maximum annual plant drought stress ([0-1])", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/PlantStress_provinces.png", p18, width = 16, height = 8)

# Timber stock
p19<-plot_var(ALL, "AllVolume", "Tree volume stock (m3/ha)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/VolumeStock_provinces.png", p19, width = 16, height = 8)

p20<-plot_var(ALL, "AllVolumeExt", "Tree volume stock (m3)", stand_agg_fun = "sum", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/VolumeStockExtensive_provinces.png", p20, width = 16, height = 8)

p21<-plot_var(ALL, "VolumeStructure", "Structural wood volume stock (m3/ha)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/VolumeStructureStock_provinces.png", p21, width = 16, height = 8)

p22<-plot_var(ALL, "VolumeFirewood", "Fire wood volume stock (m3/ha)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/VolumeFirewoodStock_provinces.png", p22, width = 16, height = 8)

rm(p19, p20, p21, p22)
gc()

# Biomass stock
p23<-plot_var(ALL, "LiveBiomass", "Woody live biomass (Mg CO2/ha)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/LiveWoodyBiomass_provinces.png", p23, width = 16, height = 8)

p24<-plot_var(ALL, "TreeBiomass", "Tree biomass (Mg CO2/ha)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/LiveTreeBiomass_provinces.png", p24, width = 16, height = 8)

p25 <- plot_var(ALL, "ShrubBiomass", "Shrub biomass (Mg CO2/ha)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/LiveShrubBiomass_provinces.png", p25, width = 16, height = 8)

rm(p23, p24, p25)
gc()

# Dead biomass
p26 <- plot_var(ALL, "DeadBiomass", "Dead biomass (Mg CO2/ha)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/DeadWoodyBiomass_provinces.png", p26, width = 16, height = 8)

p27<-plot_var(ALL, "CumulativeDeadBiomass", "Cumulative dead biomass (Mg CO2/ha)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/CumulativeDeadWoodyBiomass_provinces.png", p27, width = 16, height = 8)

p28<-plot_var(ALL, "CumulativeTreeDeadBiomass", "Cumulative tree dead biomass (Mg CO2/ha)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/CumulativeDeadTreeBiomass_provinces.png", p28, width = 16, height = 8)

p29<-plot_var(ALL, "CumulativeShrubDeadBiomass", "Cumulative shrub dead biomass (Mg CO2/ha)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/CumulativeDeadTreeBiomass_provinces.png", p29, width = 16, height = 8)

rm(p26, p27, p28, p29)
gc()

# plot_mortality_pathway(ALL, "Dead biomass (Mg CO2/ha)", growth_form = "all", cumulative = FALSE, provinces = target_provinces, scenarios = target_scenarios)
# plot_mortality_pathway(ALL, "Tree dead biomass (Mg CO2/ha)", growth_form = "tree", cumulative = FALSE, provinces = target_provinces, scenarios = target_scenarios)
# plot_mortality_pathway(ALL, "Shrub dead biomass (Mg CO2/ha)", growth_form = "shrub", cumulative = FALSE, provinces = target_provinces, scenarios = target_scenarios)

p30 <- plot_mortality_pathway(ALL, "Cumulative dead biomass (Mg CO2/ha)", growth_form = "all", cumulative = TRUE, provinces = target_provinces, scenarios = target_scenarios)
ggsave("Plots/CumulativeDeadWoodyBiomassCause_provinces.png", p30, width = 16, height = 8)

p31<-plot_mortality_pathway(ALL, "Cumulative tree dead biomass (Mg CO2/ha)", growth_form = "tree", cumulative = TRUE, provinces = target_provinces, scenarios = target_scenarios)
ggsave("Plots/CumulativeDeadTreeBiomassCause_provinces.png", p31, width = 16, height = 8)

p32<-plot_mortality_pathway(ALL, "Cumulative shrub dead biomass (Mg CO2/ha)", growth_form = "shrub", cumulative = TRUE, provinces = target_provinces, scenarios = target_scenarios)
ggsave("Plots/CumulativeDeadShrubBiomassCause_provinces.png", p32, width = 16, height = 8)

rm(p30, p31, p32)
gc()


# Wood extraction
p33<-plot_var(ALL, "CutAll", "Annual tree volume extraction (m3/ha/yr)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/AnnualTreeVolumeExtraction_provinces.png", p33, width = 16, height = 8)
# plot_var(ALL, "CutAllExt", "Annual tree volume extraction (m3/yr)", stand_agg_fun = "sum", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)

p34<-plot_var(ALL, "CumulativeCutAll", "Cumulative tree volume extraction (m3/ha)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/CumulativeTreeVolumeExtraction_provinces.png", p34, width = 16, height = 8)

# plot_var(ALL, "CumulativeCutAllExt", "Cumulative tree volume extraction (m3)", stand_agg_fun = "sum")
p35<-plot_var(ALL, "CumulativeCutFirewood", "Cumulative firewood volume extraction (m3/ha)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios)
ggsave("Plots/CumulativeFirewoodVolumeExtraction_provinces.png", p35, width = 16, height = 8)

p36<-plot_var(ALL, "CumulativeCutStructure", "Cumulative structure volume extraction (m3/ha)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios)
ggsave("Plots/CumulativeStructureVolumeExtraction_provinces.png", p36, width = 16, height = 8)

rm(p33, p34, p35, p36)
gc()

# Water supply/regulation
p37<-plot_var(ALL, "BlueWater", "Blue water (mm)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/AnnualBlueWater_provinces.png", p37, width = 16, height = 8)

p38<-plot_var(ALL, "CumulativeBlueWater", "Cumulative Blue water (mm)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/CumulativeBlueWater_provinces.png", p38, width = 16, height = 8)

p39<-plot_var(ALL, "RunoffCoefficient", "Runoff Coef. [0-1]", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/AnnualRunoffCoefficient_provinces.png", p39, width = 16, height = 8)

p40<-plot_var(ALL, "CumulativeRunoffCoefficient", "Cumulative Runoff Coef. [0-1]", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/CumulativeRunoffCoefficient_provinces.png", p40, width = 16, height = 8)

# plot_var(ALL, "RegulationCoefficient", "Regulation Coef. [0-1]")
# plot_var(ALL, "CumulativeRegulation", "Cumulative Regulation Coef. [0-1]")
p41 <- plot_var(ALL, "Cm_mean", "Cm (mm)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/CmMean_provinces.png", p41, width = 16, height = 8)

rm(p37,p38, p39, p40, p41)
gc()

# Fire hazard
p42 <- plot_var(ALL, "Tree_fuel", "Tree fine fuel (kg/m2)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/TreeFuel_provinces.png", p42, width = 16, height = 8)

p43 <- plot_var(ALL, "Shrub_fuel", "Shrub fine fuel (kg/m2)", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/ShrubFuel_provinces.png", p43, width = 16, height = 8)

p44 <- plot_var(ALL, "SFP", "Surface fire potential [0-9]", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/SurfaceFirePotential_provinces.png", p44, width = 16, height = 8)

p45 <- plot_var(ALL, "CFP", "Crown fire potential [0-9]", aggregateProvinces = aggregateProvinces, provinces = target_provinces, scenarios = target_scenarios, se = se)
ggsave("Plots/CrownFirePotential_provinces.png", p45, width = 16, height = 8)

rm(p42,p43, p44, p45)
gc()

SP |>
  dplyr::filter(GrowthForm == "Tree") |>
  dplyr::group_by(Climate, Management, Step, Year, Genus) |>
  dplyr::summarise(Biomass = sum(TotalBiomass, na.rm=TRUE), 
                   Volume = sum(Volume, na.rm=TRUE), 
                   .groups="drop") -> tree_group

SP |>
  dplyr::filter(GrowthForm == "Tree") |>
  dplyr::group_by(Climate, Management, Step, Year) |>
  dplyr::summarise(TotalBiomass = sum(TotalBiomass, na.rm=TRUE), 
                   TotalVolume = sum(Volume, na.rm=TRUE), 
                   .groups="drop") -> tree_total

tree_group |> 
  dplyr::left_join(tree_total, by=c("Climate", "Management", "Step", "Year"))|>
  dplyr::mutate(RelBiomass = Biomass/TotalBiomass) -> tree_group_rel

# target_families <- c("Betulaceae","Pinaceae", "Fagaceae", "Oleaceae", "Sapindaceae")
# tree_group_rel$Family[!(tree_group_rel$Family %in% target_families)] <- "Other"

target_genus <- c("Pinus","Fagus", "Quercus", "Acer", "Arbutus", "Castanea", "Phillyrea")
tree_group_rel$Genus[!(tree_group_rel$Genus %in% target_genus)] <- "Other"

p1 <- ggplot(tree_group_rel, aes(x = Year, y = RelBiomass)) +
  geom_col(aes(fill = Genus), position = "stack")+
  scale_fill_brewer("Genus", type = "qual", palette = "Set1")+
  facet_grid(rows = vars(Climate), cols = vars(Management))+
  theme_bw()
ggsave("kk.png", p1)

# volstruct_plot <- ALL |>  select(Climate, Management, id, Year, VolumeStructure) |>
#   pivot_wider(names_from = Year, values_from = VolumeStructure) |>
#   mutate("2000-2020" = (`2020` - `2000`)/20,
#          "2020-2040" = (`2040` - `2020`)/20,
#          "2040-2060" = (`2060` - `2040`)/20,
#          "2060-2080" = (`2080` - `2060`)/20,
#          "2080-2100" = (`2100` - `2080`)/20)|>
#   select(Climate, Management, id, "2000-2020", "2020-2040", "2040-2060", "2060-2080", "2080-2100")|>
#   pivot_longer(c("2000-2020", "2020-2040", "2040-2060", "2060-2080", "2080-2100"), names_to = "Period", values_to = "AnnualVolumeIncrement") |>
#   group_by(Climate, Management, Period) |>
#   summarise(AnnualVolumeIncrement = mean(AnnualVolumeIncrement, na.rm=TRUE), .groups = "drop")
# 
# ggplot(volstruct_plot, aes(x=Period, y=AnnualVolumeIncrement, 
#                                          fill =Management))+
#   geom_bar(stat="identity", position=position_dodge())+
#   facet_wrap(vars(Climate))+
#   ylab("Annual volume increment (m3/ha/yr.)")+
#   xlab("")+
#   scale_fill_discrete("Management", guide = guide_legend(reverse = TRUE))+
#   theme_bw()
