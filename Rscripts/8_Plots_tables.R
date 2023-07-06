library(ggplot2)
library(tidyverse)


load_result_table <- function() {
  
  cli::cli_progress_step("Coordinates and area represented")
  nfiplot <- dplyr::bind_rows(readRDS(paste0("Rdata/nfiplot.rds")))
  initial <- dplyr::bind_rows(readRDS(paste0("Rdata/test_initial.rds")))
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
                  CutAllExt = CutAll*represented_area) |>
    dplyr::mutate(Climate = toupper(Climate))
  return(ALL)
}
plot_var <- function(x, var, ylab, stand_agg_fun = "mean", common = 2000:2020) {
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
    geom_line(aes(x = Year, y=y, col=Management), size = 1.1)+
    geom_line(data = x_agg[x_agg$Year %in% common,], 
              aes(x = Year, y=y), col = "black", size = 1.1)+
    geom_vline(xintercept = 2020, linetype="dashed")+
    facet_wrap(vars(Climate), nrow = 1)+xlab("")+ylab(ylab)+theme_bw()
}

ALL <- load_result_table()

# Climate
plot_var(ALL, "Precipitation", "Annual precipitation (mm/yr)")
plot_var(ALL, "Pdaymax", "Maximum daily precipitation (mm/day)")
plot_var(ALL, "PET", "Annual PET (mm/yr)")
plot_var(ALL, "PPET", "Moisture index")
plot_var(ALL, "CumulativePPET", "Cumulative moisture index")

# Structure
plot_var(ALL, "BasalArea", "Stand basal area (m2/ha)")
plot_var(ALL, "TreeDensity", "Tree density (ind/ha)")
plot_var(ALL, "QMD", "Quadratic mean diameter (cm)")

# LAI
plot_var(ALL, "LAI_max", "Woody LAI (m2/m2)")
plot_var(ALL, "Shrub_lai", "Shrub LAI (m2/m2)")
plot_var(ALL, "Tree_lai", "Tree LAI (m2/m2)")
plot_var(ALL, "Total_lai", "Total LAI (m2/m2)")


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
plot_var(ALL, "CumulativeDeadBiomass", "Cumulative dead biomass (Mg CO2/ha)")
plot_var(ALL, "CumulativeTreeDeadBiomass", "Cumulative tree dead biomass (Mg CO2/ha)")
plot_var(ALL, "CumulativeShrubDeadBiomass", "Cumulative shrub dead biomass (Mg CO2/ha)")

# Productivity
plot_var(ALL, "GrossPrimaryProduction", "Gross Primary Production (gC/m2/yr)")
plot_var(ALL, "NetPrimaryProduction", "Net Primary Production (gC/m2/yr)")


# Wood extraction
plot_var(ALL, "CutAll", "Annual tree volume extraction (m3/ha/yr)")
plot_var(ALL, "CumulativeCutAll", "Cumulative tree volume extraction (m3/ha)")
plot_var(ALL, "CumulativeCutFirewood", "Cumulative firewood volume extraction (m3/ha)")
plot_var(ALL, "CumulativeCutStructure", "Cumulative structure volume extraction (m3/ha)")

# Water supply/regulation
plot_var(ALL, "BlueWater", "Blue water (mm)")
plot_var(ALL, "CumulativeBlueWater", "Cumulative Blue water (mm)")
plot_var(ALL, "RunoffCoefficient", "Runoff Coef. [0-1]")
plot_var(ALL, "CumulativeRunoffCoefficient", "Cumulative Runoff Coef. [0-1]")
plot_var(ALL, "RegulationCoefficient", "Regulation Coef. [0-1]")
plot_var(ALL, "CumulativeRegulation", "Cumulative Regulation Coef. [0-1]")

# Fire hazard
plot_var(ALL, "Shrub_fuel", "Shrub fine fuel")
plot_var(ALL, "Tree_fuel", "Tree fine fuel")
plot_var(ALL, "SFP", "Surface fire potential [0-9]")
plot_var(ALL, "CFP", "Crown fire potential [0-9]")

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
