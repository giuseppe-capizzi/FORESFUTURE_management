library(ggplot2)
library(tidyverse)


load_volume_table <- function() {
  df <- data.frame()
  for(provinceName in c("Barcelona", "Girona","Lleida")) {
    for(management_scen in c("BAU", "AMF", "RSB", "ASEA", "ACG", "NOG")) {
      for(climate_model in c("mpiesm_rca4")) {
        for(climate_scen in c("rcp45", "rcp85")) {
          bind_file <- paste0("Rdata/binded/", provinceName, "_", management_scen, "_", climate_model,"_", climate_scen, ".rds")
          if(file.exists(bind_file)) {
            scen_list <- readRDS(file = bind_file)
            df <- bind_rows(df, scen_list$volume_table) 
          }
        }
      }
    }
  }
  df_decade <- df |>
    dplyr::mutate(Decade = cut(Year, seq(2000,2100, by=10))) |>
    dplyr::group_by(Climate, Management, Province, Decade) |>
    dplyr::summarise(Growth = sum(pmax(growth,0)), 
                     Extraction = sum(extracted),
                     Demand = sum(pmax(nominal_demand,0)),
                     ExtractionTarget = sum(extracted_target), .groups = "drop") |>
    dplyr::mutate(ExtractionRate = 100*Extraction/Growth,
                  SatisfactionRate = 100*ExtractionTarget/Demand)

  return(df_decade)
}

load_result_table <- function() {
  
  cli::cli_progress_step("Coordinates and area represented")
  nfiplot <- dplyr::bind_rows(readRDS(paste0("Rdata/nfiplot.rds")))
  initial <- readRDS(paste0("Rdata/test_initial.rds"))
  initial <- initial[[1]]
  # initial <- dplyr::bind_rows(readRDS(paste0("Rdata/test_initial.rds")))
  n <- nrow(nfiplot)
  n_test<-nrow(initial)
  initial$represented_area <- initial$represented_area*(n/n_test)
  
  cli::cli_progress_step("Loading annual indicators")
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

plot_var <- function(x, var, ylab, 
                     aggregateProvinces = TRUE, stand_agg_fun = "mean", 
                     provinces = c("Barcelona", "Girona", "Lleida", "Tarragona"),
                     scenarios = c("BAU", "AMF", "RSB", "ASEA", "ACG", "NOG")) {
  x <- x |>
    filter(Province %in% provinces, Management %in% scenarios)
  
  common = 2000:2020
  
  if(aggregateProvinces) {
    if(stand_agg_fun == "mean") {
      x_agg <- x |>
        dplyr::group_by(Climate, Management, Year) |>
        dplyr::summarise(y = mean(.data[[var]], na.rm = TRUE), .groups="drop")
    } else if(stand_agg_fun == "sum") {
      x_agg <- x |>
        dplyr::group_by(Climate, Management, Year) |>
        dplyr::summarise(y = sum(.data[[var]], na.rm = TRUE), .groups="drop")
    }
    ggplot(x_agg)+
      facet_wrap(vars(Climate), nrow = 1)+
      geom_line(aes(x = Year, y=y, col=Management), size = 1.1)+
      geom_line(data = x_agg[x_agg$Year %in% common,], 
                aes(x = Year, y=y), col = "black", size = 1.1)+
      geom_vline(xintercept = 2020, linetype="dashed")+
      scale_color_brewer("Management", type ="qual", palette = "Set1")+
      xlab("")+ylab(ylab)+theme_bw()
  } else {
    if(stand_agg_fun == "mean") {
      x_agg <- x |>
        dplyr::group_by(Climate, Management, Province, Year) |>
        dplyr::summarise(y = mean(.data[[var]], na.rm = TRUE), .groups="drop")
    } else if(stand_agg_fun == "sum") {
      x_agg <- x |>
        dplyr::group_by(Climate, Management, Province, Year) |>
        dplyr::summarise(y = sum(.data[[var]], na.rm = TRUE), .groups="drop")
    }
    ggplot(x_agg)+
      geom_line(aes(x = Year, y=y, col=Management), size = 1.1)+
      geom_line(data = x_agg[x_agg$Year %in% common,],
      aes(x = Year, y=y), col = "black", size = 1.1)+
      geom_vline(xintercept = 2020, linetype="dashed")+
      scale_color_brewer("Management", type ="qual", palette = "Set1")+
      facet_grid(rows = vars(Climate), cols = vars(Province))+
      xlab("")+ylab(ylab)+theme_bw()
  }
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
                       Extraction = sum(Extraction),
                       Demand = sum(Demand),
                       ExtractionTarget = sum(ExtractionTarget), .groups = "drop") |>
      dplyr::mutate(ExtractionRate = 100*Extraction/Growth,
                    SatisfactionRate = 100*ExtractionTarget/Demand)
    x$y <- x[[var]]
    g<-ggplot(x, aes(x = Decade, y=y, fill=Management))+
      geom_bar(stat="identity", position=position_dodge())+
      scale_fill_brewer("Management", type = "qual", palette = "Set1")+
      facet_wrap(vars(Climate), nrow = 1)+xlab("")+ylab(ylab)+theme_bw()
  } else {
    x$y <- x[[var]]
    g<-ggplot(x, aes(x = Decade, y=y, fill=Management))+
      geom_bar(stat="identity", position=position_dodge())+
      scale_fill_brewer("Management", type = "qual", palette = "Set1")+
      facet_grid(rows = vars(Climate), cols = vars(Province))+
      xlab("")+ylab(ylab)+theme_bw()
  }
  return(g)
}

ALL <- load_result_table()
VOL <- load_volume_table()

target_provinces <- c("Barcelona", "Girona")
aggregateProvinces <- FALSE

plot_volume_var(VOL, "Growth", "Growth (m3)", aggregateProvinces = aggregateProvinces, provinces = target_provinces)
plot_volume_var(VOL, "Extraction", "Extraction (m3)", aggregateProvinces = aggregateProvinces, provinces = target_provinces)
plot_volume_var(VOL, "ExtractionRate", "Extraction rate (% of growth)", aggregateProvinces = aggregateProvinces, provinces = target_provinces)+
  geom_hline(yintercept = 30, linetype="dashed")+
  geom_hline(yintercept = 40, linetype="dashed")+
  geom_hline(yintercept = 70, linetype="dashed")+
  geom_hline(yintercept = 100, linetype="dashed")

plot_volume_var(VOL, "ExtractionTarget", "Extraction target species (m3)", aggregateProvinces = aggregateProvinces, provinces = target_provinces)
plot_volume_var(VOL, "Demand", "Nominal demand (m3)", aggregateProvinces = aggregateProvinces, provinces = target_provinces)
plot_volume_var(VOL, "SatisfactionRate", "Satisfaction (% of demand)", aggregateProvinces = aggregateProvinces, provinces = target_provinces)+
  geom_hline(yintercept = 100, linetype="dashed")


# Climate
plot_var(ALL, "Precipitation", "Annual precipitation (mm/yr)")
plot_var(ALL, "Pdaymax", "Maximum daily precipitation (mm/day)")
plot_var(ALL, "PET", "Annual PET (mm/yr)")
plot_var(ALL, "PPET", "Moisture index")
plot_var(ALL, "CumulativePPET", "Cumulative moisture index")

# Structure
plot_var(ALL, "BasalArea", "Stand basal area (m2/ha)", aggregateProvinces = FALSE, provinces = "Barcelona", scenarios = "BAU")
plot_var(ALL, "TreeDensity", "Tree density (ind/ha)", aggregateProvinces = FALSE, provinces = "Barcelona", scenarios = "BAU")
plot_var(ALL, "QMD", "Quadratic mean diameter (cm)", aggregateProvinces = FALSE)

# LAI
plot_var(ALL, "LAI_max", "Woody LAI (m2/m2)", aggregateProvinces = FALSE, provinces = "Barcelona", scenarios = "BAU")
plot_var(ALL, "Shrub_lai", "Shrub LAI (m2/m2)", aggregateProvinces = FALSE)
plot_var(ALL, "Tree_lai", "Tree LAI (m2/m2)", aggregateProvinces = FALSE)
plot_var(ALL, "Total_lai", "Total LAI (m2/m2)", aggregateProvinces = FALSE)

# Function
plot_var(ALL, "GrossPrimaryProduction", "Gross Primary Production (gC/m2/yr)")
plot_var(ALL, "NetPrimaryProduction", "Net Primary Production (gC/m2/yr)")
plot_var(ALL, "WaterUseEfficiency", "Water Use Efficiency (gC/mm)")
plot_var(ALL, "CarbonUseEfficiency", "Carbon Use Efficiency (gC synthesis/gC)")
plot_var(ALL, "StemPLC", "Maximum annual Stem PLC ([0-1])", aggregateProvinces = FALSE, provinces = "Barcelona", scenarios = "BAU")
plot_var(ALL, "PlantStress", "Maximum annual plant drought stress ([0-1])", aggregateProvinces = FALSE, provinces = "Barcelona", scenarios = "BAU")

# Timber stock
plot_var(ALL, "AllVolume", "Tree volume stock (m3/ha)")
plot_var(ALL, "AllVolumeExt", "Tree volume stock (m3)", stand_agg_fun = "sum")
plot_var(ALL, "VolumeStructure", "Structural wood volume stock (m3/ha)")
plot_var(ALL, "VolumeFirewood", "Fire wood volume stock (m3/ha)")

# Biomass stock
plot_var(ALL, "TreeBiomass", "Tree biomass (Mg CO2/ha)")
plot_var(ALL, "ShrubBiomass", "Shrub biomass (Mg CO2/ha)")

# Dead biomass
plot_var(ALL, "DeadBiomass", "Dead biomass (Mg CO2/ha)")
plot_var(ALL, "CumulativeDeadBiomass", "Cumulative dead biomass (Mg CO2/ha)", aggregateProvinces = FALSE)
plot_var(ALL, "CumulativeTreeDeadBiomass", "Cumulative tree dead biomass (Mg CO2/ha)", aggregateProvinces = FALSE)
plot_var(ALL, "CumulativeShrubDeadBiomass", "Cumulative shrub dead biomass (Mg CO2/ha)", aggregateProvinces = FALSE)



# Wood extraction
plot_var(ALL, "CutAll", "Annual tree volume extraction (m3/ha/yr)")
plot_var(ALL, "CutAllExt", "Annual tree volume extraction (m3/yr)", stand_agg_fun = "sum", aggregateProvinces = FALSE)
plot_var(ALL, "CumulativeCutAll", "Cumulative tree volume extraction (m3/ha)", aggregateProvinces = FALSE)
plot_var(ALL, "CumulativeCutAllExt", "Cumulative tree volume extraction (m3)", stand_agg_fun = "sum")
plot_var(ALL, "CumulativeCutFirewood", "Cumulative firewood volume extraction (m3/ha)")
plot_var(ALL, "CumulativeCutStructure", "Cumulative structure volume extraction (m3/ha)")

# Water supply/regulation
plot_var(ALL, "BlueWater", "Blue water (mm)")
plot_var(ALL, "CumulativeBlueWater", "Cumulative Blue water (mm)", aggregateProvinces = FALSE)
plot_var(ALL, "RunoffCoefficient", "Runoff Coef. [0-1]", aggregateProvinces = FALSE)
plot_var(ALL, "CumulativeRunoffCoefficient", "Cumulative Runoff Coef. [0-1]", aggregateProvinces = FALSE)
plot_var(ALL, "RegulationCoefficient", "Regulation Coef. [0-1]")
plot_var(ALL, "CumulativeRegulation", "Cumulative Regulation Coef. [0-1]")
plot_var(ALL, "Cm_mean", "Cm (mm)", aggregateProvinces = FALSE, provinces = "Barcelona", scenarios = "BAU")

# Fire hazard
plot_var(ALL, "Shrub_fuel", "Shrub fine fuel", aggregateProvinces = FALSE)
plot_var(ALL, "Tree_fuel", "Tree fine fuel", aggregateProvinces = FALSE)
plot_var(ALL, "SFP", "Surface fire potential [0-9]", aggregateProvinces = FALSE)
plot_var(ALL, "CFP", "Crown fire potential [0-9]", aggregateProvinces = FALSE)

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
