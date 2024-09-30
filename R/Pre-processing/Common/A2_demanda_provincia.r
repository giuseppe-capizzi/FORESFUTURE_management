rm(list=ls()); gc()
library(tidyverse)

## A. Aprofitaments forestals 2001 - 2009 (falten 2001 i 2005) 
aprofit = xlsx::read.xlsx("Tables/AprofitamentsForestals_2001-2021/AprofitamentsForestals_EspècieProvíncia_2001-2009.xlsx", startRow=2, sheetIndex = 1)

## Sumar demandes de llenya en m3 a les espècies principals
aprofit$P.sylvestris = aprofit$P.sylvestris + aprofit$Llenya_Conif_m3
aprofit$Q.ilex = aprofit$Q.ilex + aprofit$Llenya_Alzi.rou_m3
aprofit$Altres.planifòlis = aprofit$Altres.planifòlis + aprofit$Llenya_Planifo_m3

## Canviar el format de les dades i calcular el volum total d'aprofitaments
aprofit_prov_spp_01_09 = aprofit %>% filter(Provincia != "Catalunya") %>% 
  select(-Conif, -Alzi.rou, -Planifo, -Llenya_Alzi.rou_m3, -Llenya_Conif_m3, -Llenya_Planifo_m3) %>%
  pivot_longer(cols = A.alba:Altres.planifòlis, names_to = "Especie", values_to = "Volum")
#%>% group_by(Year, Provincia, Species) %>%   summarise(vol_total=sum(value, na.rm=T))


## B. Aprofitaments forestals 2010 - 2020
aprofit = #xlsx::read.xlsx("Tables/AprofitamentsForestals_2001-2021/aprof_2010_2020.xlsx", sheetIndex = 1)
  read.csv("Tables/AprofitamentsForestals_2001-2021/Aprofitaments_2010-2020.csv", sep=";")
head(aprofit)  

## Comarca - Provincia
comarca_provi = data.frame(Comarca=unique(aprofit$Comarca))
comarca_provi$Provincia = NA
comarca_provi$Provincia[comarca_provi$Comarca %in% c("Alt Penedès", "Anoia", "Bages",
                                                     "Baix Llobregat", "Barcelonès", "Berguedà", "Cerdanya", "Garraf", "Maresme", "Osona",
                                                     "Vallès Occidental", "Vallès Oriental", "Moianès")] = "Barcelona"
comarca_provi$Provincia[comarca_provi$Comarca %in% c("Alt Empordà", "Baix Empordà", 
                                                     "Garrotxa", "Gironès", "Selva", "Ripollès", "Pla de l' Estany")] = "Girona"
comarca_provi$Provincia[comarca_provi$Comarca %in% c("Alta Ribagorça", "Solsonès",
                                                     "Garrigues", "Noguera", "Segarra", "Segrià", "Urgell", "Alt Urgell", "Pallars Jussà",
                                                     "Pallars Sobirà", "Val d'Aran", "Pla d'Urgell")] = "Lleida"
comarca_provi$Provincia[comarca_provi$Comarca %in% c("Alt Camp", "Baix Camp", "Baix Ebre",
                                                     "Baix Penedès", "Conca de Barber", "Montsià", "Priorat", "Ribera d'Ebre", 
                                                     "Tarragonès", "Terra Alta")] = "Tarragona"
  # comarca_provi[is.na(comarca_provi$Provincia),]

## Add Province name, remove "Llenya of Arbu+Mat", group by Year, Province, Sp & Product
aprofit_prov_spp_10_20 = aprofit %>% left_join(comarca_provi, by = "Comarca") %>% 
  filter(Sp != "Arbu+Mat") %>% group_by(Any, Provincia, Sp, Producte, Unitat) %>% summarise(Valor=sum(Valor))

## For verification purposes, sum at the Catalan level
aprofit_spp_10_20 = aprofit_prov_spp_10_20 %>% group_by(Any, Especie) %>% summarize(Volum=sum(Volum))

## Transform wood t to m3 and add to the target species
aprofit_prov_spp_10_20$Valor[aprofit_prov_spp_10_20$Sp=="Conif"] = aprofit_prov_spp_10_20$Valor[aprofit_prov_spp_10_20$Sp=="Conif"] * 1000/500 
aprofit_prov_spp_10_20$Valor[aprofit_prov_spp_10_20$Sp=="Planifo"] = aprofit_prov_spp_10_20$Valor[aprofit_prov_spp_10_20$Sp=="Planifo"] * 1000/650
for(y in unique(aprofit_prov_spp_10_20$Any)){
  for(p in unique(aprofit_prov_spp_10_20$Provincia)){
    aprofit_prov_spp_10_20$Valor[aprofit_prov_spp_10_20$Any==y & aprofit_prov_spp_10_20$Provincia==p & aprofit_prov_spp_10_20$Sp=="P.sylvestris"] = 
      aprofit_prov_spp_10_20$Valor[aprofit_prov_spp_10_20$Any==y & aprofit_prov_spp_10_20$Provincia==p & aprofit_prov_spp_10_20$Sp=="P.sylvestris"] + 
      aprofit_prov_spp_10_20$Valor[aprofit_prov_spp_10_20$Any==y & aprofit_prov_spp_10_20$Provincia==p & aprofit_prov_spp_10_20$Sp=="Conif"]
    aprofit_prov_spp_10_20$Valor[aprofit_prov_spp_10_20$Any==y & aprofit_prov_spp_10_20$Provincia==p & aprofit_prov_spp_10_20$Sp=="Q.ilex"] = 
      aprofit_prov_spp_10_20$Valor[aprofit_prov_spp_10_20$Any==y & aprofit_prov_spp_10_20$Provincia==p & aprofit_prov_spp_10_20$Sp=="Q.ilex"] + 
      aprofit_prov_spp_10_20$Valor[aprofit_prov_spp_10_20$Any==y & aprofit_prov_spp_10_20$Provincia==p & aprofit_prov_spp_10_20$Sp=="Alzi+rou"]
    aprofit_prov_spp_10_20$Valor[aprofit_prov_spp_10_20$Any==y & aprofit_prov_spp_10_20$Provincia==p & aprofit_prov_spp_10_20$Sp=="Altres planifolis"] = 
      aprofit_prov_spp_10_20$Valor[aprofit_prov_spp_10_20$Any==y & aprofit_prov_spp_10_20$Provincia==p & aprofit_prov_spp_10_20$Sp=="Altres planifolis"] + 
      aprofit_prov_spp_10_20$Valor[aprofit_prov_spp_10_20$Any==y & aprofit_prov_spp_10_20$Provincia==p & aprofit_prov_spp_10_20$Sp=="Planifo"]
  }
}

## Remove 'Llenyes'
aprofit_prov_spp_10_20 = aprofit_prov_spp_10_20 %>% filter(Producte == "Fusta")

## Match names of species
aprofit_prov_spp_01_09$Especie[aprofit_prov_spp_01_09$Especie == "Altres.coníferes" ] = "Altres coniferes"
aprofit_prov_spp_01_09$Especie[aprofit_prov_spp_01_09$Especie == "Altres.planifòlis" ] = "Altres planifolis"
aprofit_prov_spp_01_09$Especie[aprofit_prov_spp_01_09$Especie == "Eucaliptus.sp." ] = "Eucaliptus sp"
aprofit_prov_spp_01_09$Especie[aprofit_prov_spp_01_09$Especie == "Fraxinus.sp" ] = "Fraxinus sp"
aprofit_prov_spp_01_09$Especie[aprofit_prov_spp_01_09$Especie == "Ulmus.sp" ] = "Ulmus sp"
aprofit_prov_spp_10_20$Sp[aprofit_prov_spp_10_20$Sp == "Eucaliptus sp." ] = "Eucaliptus sp"

## C. Bind both dataframes
head(aprofit_prov_spp_01_09)
head(aprofit_prov_spp_10_20 )
aprofit_prov_spp_10_20 = aprofit_prov_spp_10_20[,c(1,2,3,6)]
names(aprofit_prov_spp_10_20) = names(aprofit_prov_spp_01_09)
aprofit_prov_spp = rbind(aprofit_prov_spp_01_09, aprofit_prov_spp_10_20)

## Add decade and compute mean annual volume
aprofit_prov_spp$Decade = NA
aprofit_prov_spp$Decade[aprofit_prov_spp$Any<=2010] = "2001-2010"
aprofit_prov_spp$Decade[aprofit_prov_spp$Any>2010] = "2011-2020"
annual_aprofit_prov_spp = aprofit_prov_spp %>% group_by(Decade, Provincia, Especie) %>% 
  summarise(Volum = mean(Volum, na.rm=T))
sort(unique(annual_aprofit_prov_spp$Especie))

## Final table of demand per decade and species
write.table(annual_aprofit_prov_spp, "Tables/AprofitamentsForestals_2001-2021/aprofit_decade_prov_spp.txt",
            quote=F, row.names=F, sep="\t")


## D. Verify with data at Catalonia level
aprofit_cat_conif = read.table("Tables/AprofitamentsForestals_2001-2021/OFC_aprofitaments_coniferes_1988_2021.txt", header = T)
aprofit_cat_planif = read.table("Tables/AprofitamentsForestals_2001-2021/OFC_aprofitaments_planifolis_1988_2021.txt", header = T)
aprofit_cat = aprofit_cat_conif %>% left_join(aprofit_cat_planif, by=c("Any", "Titularitat")) %>% 
  pivot_longer(cols = Pi_negre:altres_planif, names_to = "Especie", values_to = "Volum") %>% 
  filter(Any<=2020) %>% mutate(Decada=ifelse(Any<=2000, "1988-2000", ifelse(Any<=2010, "2001-2010", "2011-2020"))) %>% 
  group_by(Decada, Any, Especie) %>% summarise(Volum=sum(Volum))

## Mean annual volume per species
aprofit_cat_decada = aprofit_cat %>% group_by(Decada, Especie) %>% summarise(Volum=mean(Volum))  # OK! As in the ESCENARIS_GESTIO_feb23.pdf

## Verification, species by species
sp = names(table(aprofit_spp_10_20$Especie))
names(table(aprofit_cat$Especie))
spp = c("Avets", "Vern", "altres_conif", "altres_planif", NA, "Castanyer", "Eucaliptus", "Faig", "Freixe", "Pollancres",
        "Pi_blanc", "Plàtan", "Douglas", "Pinassa", "Pinaster", "Pi_pinyer", "Pi_insigne", "Pi_roig", "Pi_negre", "Roures",
        "Alzina_surera", "Roures", "Alzina_surera", "Robínia", NA)
i = 2
for(i in 1:length(sp)){
  a = aprofit_spp_10_20 %>% filter(Especie==sp[i])  
  b = aprofit_cat %>% filter(Any>=2010 & Especie==spp[i])
  cat(paste0(sp[i], ":  ", round(sum(a$Volum)), " - ", round(sum(b$Volum)), "; DIF = ", round(sum(a$Volum)-sum(b$Volum)), "\n"))
}


