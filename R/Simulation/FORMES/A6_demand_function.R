# Demand function 
# library(FORMES)
# library(tidyverse)
# 
# 
# 
# # load a scenario for an example 
# bar_s1_2 <- readRDS("C:/Users/giuseppe.capizzi/OneDrive - ctfc.cat/OCCC_ScnForestals/Rdata/Scenarios/BAU_2001-2020/8.rds")
# # we are interested in live volume and extracted volume
# bar_s1_2$volume$variationSpp # volum_viust+1 - volum_viust
# bar_s1_2$volume$extractedSpp # volum_extrett_t+1

# Function to generate next demand 
demand_generation <- function(demand_default, step, perc, dir){
  # browser()
  # set new data.frame object for demand (retrieved from general demand)
  # select only one step 
  new_demand <- demand_default
  # initialize proportions list
  proportions <- vector(mode = "list", length = 4)
  names(proportions) <- c("Barcelona", "Girona", "Lleida", "Tarragona") # c(8,17,25,43)
  province_vec <- c("Barcelona" = 8, "Girona" =17, "Lleida"=25, "Tarragona"=43)
  
  for(pr in names(province_vec)){
    
    # retrieve order species from original demand (to reorder later)
    order_species <- new_demand[[pr]][new_demand[[pr]]$Step == 1,]$Species
    
    grp_dem <- new_demand[[pr]]
    grp_dem <- grp_dem %>% group_by(Species) %>% summarise(AnnualDemand = sum(AnnualDemand))
    # calculate proportions 
    # grp_dem <- grp_dem %>% mutate(prop = grp_dem$AnnualDemand/sum(grp_dem$AnnualDemand)) %>%
    #   arrange(order_species) 
    proportions[[pr]] <- grp_dem %>% mutate(prop = grp_dem$AnnualDemand/sum(grp_dem$AnnualDemand), AnnualDemand=NULL) %>%
      arrange(order_species)
    
    ## new dataframe
    # select only 1 step
    new_demand[[pr]] = new_demand[[pr]][new_demand[[pr]]$Step == 1,]
    # set new step
    new_demand[[pr]]$Step = step
    # set AnnualDemand to 0
    new_demand[[pr]]$AnnualDemand = 0
  }
  
  # calculate new demand per province
  for(p in prov){
    cat(paste0("Calculate demand for province ", p), "\n")
    # read scenario and extract data.frame volume
    numYears <- readRDS(paste0("Rdata/Scenarios/", dir, "/",p, ".rds"))$numYears
    assign(paste0("volume_", p), readRDS(paste0("Rdata/Scenarios/", dir, "/",p, ".rds"))$volume)
    vol = get(paste0("volume_", p))
    sel_col <- ncol(vol$variationSpp)
    # check the species 
    if(!all(names(vol$variationSpp[,sel_col]) == names(vol$extractedSpp[,sel_col]))){
      stop("Species does not match between Increment and Extracted volume in Scenario")
    }
    # select last column of dataframe (last variation and last extraction)
    
    tot_demand <- perc*(sum(vol$variationSpp[,sel_col] + vol$extractedSpp[,sel_col])/100)
    tot_annual_demand <- tot_demand/numYears
    pr = names(province_vec[province_vec==p])
    new_demand[[pr]] <- left_join(new_demand[[pr]], proportions[[pr]], by = "Species") %>% mutate(AnnualDemand = tot_annual_demand*prop, prop=NULL)
  }
  return(new_demand)
}

# params to retrieve provinces 
# params = defaultParamsFORMES()
# prov <- params$provs
# ## Load demand
# demand_2001_2020 <- readRDS("Rdata/demand_2001_2020.rds")
# 
# 
# prova <- demand_generation(demand_default = demand_2001_2020, dir = "BAU_2001-2020", step = 3, perc = 30)
# check results
# plot(prova$Barcelona$AnnualDemand/sum(prova$Barcelona$AnnualDemand))
# points(demand_2001_2020$Barcelona[demand_2001_2020$Barcelona$Step==1, "AnnualDemand"]/sum(demand_2001_2020$Barcelona[demand_2001_2020$Barcelona$Step==1, "AnnualDemand"]), col="red")










