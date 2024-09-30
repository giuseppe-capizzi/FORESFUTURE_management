# script to lunch from Nuria's pc CTFC (only projections)
library(meteoland)

# load nfiplot
nfiplot <- readRDS("Rdata/nfiplot.rds")

# vector of years 1
proj_years <- c("2006_2010",
                "2011_2020", 
                "2021_2030", 
                "2031_2040", 
                "2041_2050")

# vector of years 2
proj_years <- c("2051_2060", 
                "2061_2070", 
                "2071_2080", 
                "2081_2090", 
                "2091_2100")


# set rcps empy 0 KB
# 1. done
rcp <- "45"
proj_years <- c("2021_2030")
# 2. done
rcp <- "45"
proj_years <- c("2041_2050")
# 3. done
rcp <- "45"
proj_years <- c("2071_2080")
# 4.done
rcp <- "85"
proj_years <- c("2051_2060")
# 5. done
rcp <- "85"
proj_years <- c("2006_2010")
# 6.
rcp <- "45"
proj_years <- c("2031_2040")
for(y in proj_years){
  cat(paste0("\nClimate interpolation RCP", rcp, " - ",  y, "\n"))
  interpolator <- read_interpolator(paste0(dirname(getwd()), 
                    "/FORMESdata/Sources/Climate/Interpolators/catalunya_projections/mpiesm_rca4_rcp", rcp , "_daily_interpolator_", y, ".nc"))
  nfi_data <- interpolate_data(nfiplot, interpolator = interpolator)
  saveRDS(nfi_data, paste0(dirname(getwd()), 
    "/FORMESdata/Rdata/Interpolated_climate_projections/nfi_",rcp,"_",y,".rds")) # save nfi_data
  rm(nfi_data) # remove nfi_data
  rm(interpolator) # remove interpolator
}
  