library(IFNallometry)
library(medfuels)

provinces <- c(8,17,25,43)
provinceStrings <- c("Barcelona", "Girona", "Lleida", "Tarragona")


cli::cli_li(paste0("Defining tree biomass function"))
tree_biomass_scenario<-function(x, SpParams, as.CO2 = TRUE){
  if(inherits(x, "forest")) x <- x$treeData
  ntree <- nrow(x)
  if(ntree>0) {
    IFN_codes <- species_characterParameter(x$Species, SpParams, "IFNcodes")
    IFN_codes_split <- strsplit(IFN_codes, "/")
    IFN_codes <- sapply(IFN_codes_split, function(x) return(x[[1]]))
    y <- data.frame(ID = rep("XX", ntree), 
                    Species = IFN_codes,
                    DBH = x$DBH,
                    H = x$Height/100,
                    N = x$N
    )
    biom <- IFNallometry::IFNbiomass(y, as.CO2 = as.CO2, verbose = FALSE)
    return(bind_cols(x,biom[,-c(1,2)]))
  }
  return(numeric(0))
}

cli::cli_li(paste0("Defining shrub biomass function"))
shrub_biomass_scenario<-function(x, SpParams, as.CO2 = TRUE){
  if(inherits(x, "forest")) x <- x$shrubData
  nshrub <- nrow(x)
  if(nshrub>0) {
    y <- data.frame(plot = x$id,
                    species = x$Species,
                    H = x$Height,
                    C = x$Cover
    )
    map <- c("Adenocarpus spp."="Adenocarpus telonensis",
             "Anagyris spp." = "Anagyris foetida",
             "Artemisia spp." = "Artemisia campestris",
             "Asparagus spp." = "Asparagus acutifolius",
             "Astragalus spp." = "Astragalus sempervirens",
             "Bupleurum spp." = "Bupleurum fruticescens",
             "Calicotome spp." = "Calicotome spinosa",
             "Cistus salvifolius" = "Cistus salviifolius",
             "Corema spp." = "Corema album",
             "Coronilla spp." = "Coronilla emerus",
             "Cotoneaster spp." = "Cotoneaster integerrimus",
             "Daphne spp." ="Daphne laureola",
             "Dorycnium spp." = "Dorycnium pentaphyllum",
             "Echium spp." = "Echium vulgare",
             "Ephedra spp." = "Ephedra fragilis",
             "Erinacea spp." = "Erinacea anthyllis",
             "Euphorbia spp." = "Euphorbia characias",
             "Genista spp." = "Genista scorpius",
             "Genistella spp." = "Genistella tridentata",
             "Halimium spp." = "Halimium umbellatum",
             "Helianthemum spp." = "Helianthemum apenninum",
             "Helichrysum spp." = "Helichrysum stoechas",
             "Ilex spp." = "Ilex aquifolium",
             "Lavandula spp." = "Lavandula stoechas",
             "Lithodora spp." = "Lithospermum fruticosum",
             "Lavandula spp." = "Lavandula stoechas",
             "Lonicera spp." = "Lonicera implexa",
             "Ononis spp." = "Ononis spinosa",
             "Osyris spp." = "Osyris alba",
             "Phlomis spp." = "Phlomis lychnitis",
             "Retama spp." = "Retama sphaerocarpa",
             "Rhamnus spp." = "Rhamnus alaternus",
             "Rhododendron spp." = "Rhododendron ferrugineum",
             "Ribes spp." = "Ribes uva-crispa",
             "Rosa spp." = "Rosa canina",
             "Rubus spp." = "Rubus ulmifolius",
             "Ruscus spp." = "Ruscus aculeatus",
             "Salix spp." = "Salix purpurea",
             "Salsola spp." = "Salsola genistoides",
             "Salvia rosmarinus" = "Rosmarinus officinalis",
             "Sambucus spp." = "Sambucus nigra",
             "Santolina spp." = "Santolina chamaecyparissus",
             "Sideritis spp." = "Sideritis hirsuta",
             "Spartium spp." = "Spartium junceum",
             "Spiraea spp." = "Spiraea crenata",
             "Teline spp." = "Teline spp.",
             "Teucrium spp." = "Teucrium polium",
             "Thymelaea spp." = "Thymelaea hirsuta",
             "Thymus spp." = "Thymus vulgaris",
             "Ulex spp." = "Ulex parviflorus",
             "Vaccinium spp." = "Vaccinium myrtillus",
             "Viburnum spp." = "Viburnum tinus")
    for(i in 1:length(map)) {
      y$species[y$species==names(map)[i]] <- as.character(map[i])
    }
    vines <- c("Ampelodesmos mauritanica","Clematis spp.", "Hedera helix", "Smilax aspera")
    vine_sel <- (y$species %in% vines)
    y <- y[!vine_sel, , drop = FALSE]
    x <- x[!vine_sel, , drop=FALSE]
    biom_above <- medfuels::shrubspeciesfuel(y, type = "total")
    area <- medfuels::individualshrubarea(y)
    N <- (y$C*100)/area # density of individuals per ha
    biom_above_individual <- biom_above/N
    biom_below_individual <- 0.732*((biom_above_individual*1000)^0.9427)/1000 # From kg above to kg below
    biom_below <- biom_below_individual*N
    biom_above <- biom_above * 10 # From kg/m2 to Mg/ha
    biom_below <- biom_below * 10 # From kg/m2 to Mg/ha
    if(as.CO2) {
      biom_above <- biom_above*0.5*(44/12) # from Mg dry weight to Mg CO2
      biom_below <- biom_below*0.5*(44/12) # from Mg dry weight to Mg CO2
    }
    res <- cbind(x, biom_above, biom_below)
    res <- res |>
      dplyr::rename(Aerial = biom_above, Roots = biom_below)
    return(res)
  }
  return(numeric(0))
}

cli::cli_li(paste0("Defining volume function"))
volume_scenario<-function(x, SpParams, province){
  if(inherits(x, "forest")) x <- x$treeData
  ntree <- nrow(x)
  if(ntree>0) {
    IFN_codes <- species_characterParameter(x$Species, SpParams, "IFNcodes")
    IFN_codes_split <- strsplit(IFN_codes, "/")
    IFN_codes <- sapply(IFN_codes_split, function(x) return(x[[1]]))
    y <- data.frame(ID = rep("XX", ntree), 
                    Province = rep(province, ntree),
                    Species = IFN_codes,
                    DBH = x$DBH,
                    H = x$Height/100,
                    N = x$N
    )
    vol <- IFNallometry::IFNvolume(y)
    vcc <- pmax(0,vol$VCC)
    vcc[x$DBH < 7.5] <- 0
    return(vcc) #m3/ha
  }
  return(numeric(0))
}

