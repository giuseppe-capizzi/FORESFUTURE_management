library(ggplot2)
library(tidyverse)


cli::cli_progress_step("Coordinates and area represented")
nfiplot <- dplyr::bind_rows(readRDS(paste0("Rdata/nfiplot.rds")))
initial <- dplyr::bind_rows(readRDS(paste0("Rdata/test_initial.rds")))
n <- nrow(nfiplot)
n_test<-nrow(initial)
initial$represented_area <- initial$represented_area*(n/n_test)

BAU_rcp45 <- readRDS("Rdata/annual_indicators/BAU_mpiesm_rca4_rcp45.rds")
BAU_rcp85 <- readRDS("Rdata/annual_indicators/BAU_mpiesm_rca4_rcp85.rds")
AMF_rcp45 <- readRDS("Rdata/annual_indicators/AMF_mpiesm_rca4_rcp45.rds")
ALL <- bind_rows(BAU_rcp45, BAU_rcp85,
                 AMF_rcp45) |>
  dplyr::left_join(sf::st_drop_geometry(initial)[,c("id", "represented_area"), drop = FALSE], by = "id") |>
  dplyr::mutate(AllVolumeExt = AllVolume*represented_area,
                VolumeStructureExt = VolumeStructure*represented_area,
                CutStructureExt = CutStructure*represented_area,
                BlueWater = Runoff+DeepDrainage)

ALL |>
  dplyr::filter(Province == "Barcelona") |>
  dplyr::group_by(Climate, Management, Year) |>
  dplyr::summarise(AllVolume = mean(AllVolume, na.rm=TRUE), 
                   AllVolumeExt = sum(AllVolumeExt, na.rm=TRUE)/1000, 
                   VolumeStructure = mean(VolumeStructure, na.rm=TRUE), 
                   VolumeStructureExt = sum(VolumeStructureExt, na.rm=TRUE)/1000, 
                   VolumeFirewood = mean(VolumeFirewood, na.rm=TRUE),
                   CutStructure = mean(CutStructure, na.rm=TRUE), 
                   CutStructureExt = sum(CutStructureExt, na.rm=TRUE)/1000, 
                   CutFirewood = mean(CutFirewood, na.rm=TRUE),
                   TreeBiomass = mean(TreeBiomass, na.rm=TRUE),
                   TreeDeadBiomass = mean(TreeDeadBiomass, na.rm=TRUE),
                   CumulativeTreeDeadBiomass = mean(CumulativeTreeDeadBiomass, na.rm=TRUE),
                   ShrubBiomass = mean(ShrubBiomass, na.rm=TRUE),
                   ShrubDeadBiomass = mean(ShrubDeadBiomass, na.rm=TRUE),
                   CumulativeShrubDeadBiomass = mean(CumulativeShrubDeadBiomass, na.rm=TRUE),
                   TreeDensity = mean(TreeDensity, na.rm=TRUE),
                   BasalArea = mean(BasalArea, na.rm=TRUE),
                   BlueWater = mean(BlueWater, na.rm=TRUE),
                   RunoffCoefficient = mean(BlueWater/Precipitation, na.rm=TRUE),
                   LAI_max = mean(LAI_max, na.rm=TRUE),
                   # Shrub_lai = mean(Shrub_lai, na.rm=TRUE),
                   # Tree_fuel = mean(Tree_fuel, na.rm=TRUE),
                   # Shrub_fuel = mean(Shrub_fuel, na.rm=TRUE),
                   # SFP = mean(SFP, na.rm=TRUE),
                   # CFP = mean(CFP, na.rm=TRUE),
                   .groups = "drop") -> a


ggplot(a)+
  geom_line(aes(x = Year, y=BasalArea, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=TreeDensity, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=AllVolume, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=AllVolumeExt, col=Management, linetype = Climate))


ggplot(a)+
  geom_line(aes(x = Year, y=VolumeStructure, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=VolumeStructureExt, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=CutStructureExt, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=CutStructure, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=VolumeFirewood, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=CutFirewood, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=TreeBiomass, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=CumulativeTreeDeadBiomass, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=ShrubBiomass, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=ShrubDeadBiomass, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=CumulativeShrubDeadBiomass, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=LAI_max, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=Shrub_lai, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=BlueWater, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=RunoffCoefficient, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=Tree_fuel, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=Shrub_fuel, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=SFP, col=Management, linetype = Climate))

ggplot(a)+
  geom_line(aes(x = Year, y=CFP, col=Management, linetype = Climate))
