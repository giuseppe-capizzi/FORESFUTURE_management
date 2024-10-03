library(ggplot2)
library(tidyverse)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#                                                                                                    #
#                        Data frame creation directly from FORMES output                          -----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# example 
bau <- readRDS("C:/Users/giuseppe.capizzi/OneDrive - ctfc.cat/OCCC_ScnForestals/Rdata/Scenarios/BAU/BAU_45/BAU_2021-2030_45/8.rds")
bau_common <- readRDS("C:/Users/giuseppe.capizzi/OneDrive - ctfc.cat/OCCC_ScnForestals/Rdata/Scenarios_old_results/BAU/BAU_2001-2020/8.rds")

# voglio standing, standgrowth and dead per ha
bau$volume$summary

# area
bau$totalArea
"\BAU_45\BAU_2021-2030_45"

## VOLUME ####
data_creation <- function(mgms = c("BAU", "RSB", "ACG", "AMF", "ASEA", "NOG"),
                          rcp = c("45", "85"),
                          prov = c(8,17,25,43),
                          years = c("2021-2030","2031-2040","2041-2050","2051-2060",
                                    "2061-2070","2071-2080","2081-2090","2091-2100")){
  # browser()
  cli::cli_h3("Binding IFNscenario results from")
  
  column_names <- c("Climate","Management","Years","Province","Standing_ha","Growth", "Growth_ha",
                    "Dead_ha", "Extraction", "Extraction_ha")
  data <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  colnames(data) <- column_names
  # add BAU (first 2 decades)
  for(m in mgms){
    if (m != "NOG"){
      for(r in rcp){
        for(p in prov){
          dir_bau <- paste0("Rdata/Scenarios/BAU/BAU_2001-2020/",p,".rds" )
          scn <- readRDS(dir_bau)
          if(scn$steps!=2){stop("Wrong step number")}
          if(scn$numYears != 10){stop("wrong numYears")}
          for(s in 1:scn$steps){
            data1y <- data.frame(matrix(ncol = length(column_names), nrow = 1))
            colnames(data1y) <- column_names
            
            data1y$Climate <- paste0("rcp",r)
            data1y$Management <- m
            data1y$Years <- ifelse(s==1, "2001-2010", "2011-2020")
            if(p == 8){data1y$Province = "Barcelona"}
            if(p == 17){data1y$Province = "Girona"}
            if(p == 25){data1y$Province = "Lleida"}
            if(p == 43){data1y$Province = "Tarragona"}
            
            data1y$Standing_ha <- scn$volume$summary[as.character(s), "standing"]
            data1y$Growth_ha <- scn$volume$summary[as.character(s), "standgrowth"]
            data1y$Dead_ha <- scn$volume$summary[as.character(s), "dead"]
            data1y$Extraction_ha <- scn$volume$summary[as.character(s), "extracted"]
            data1y$Extraction <- colSums(scn$volume$extractedSpp)[as.character(s)]
            data1y$Growth <- colSums(scn$volume$standgrowthSpp)[as.character(s)]
            
            data <- rbind(data, data1y)
          }
        }
      }
    }
    
  }
  
  for(m in mgms){ # for each management
    cli::cli_progress_step(paste0("Management ", m))
    if(m == "NOG"){
      for(r in rcp){
        for(p in prov){
          dir <- paste0("Rdata/Scenarios/NOG_2001-2100_",r,"/",p,".rds" )
          scn <- readRDS(dir)
          if(scn$steps!=10){stop("wrong number of steps")}
          if(scn$numYears != 10){stop("wrong numYears")}
          for(s in 1:scn$steps){
            data1y <- data.frame(matrix(ncol = length(column_names), nrow = 1))
            colnames(data1y) <- column_names
            
            data1y$Climate <- paste0("rcp",r)
            data1y$Management <- m
            data1y$Years <- ifelse(s==1,"2001-2010", ifelse(s==2, "2011-2020",years[s-2]))
            if(p == 8){data1y$Province = "Barcelona"}
            if(p == 17){data1y$Province = "Girona"}
            if(p == 25){data1y$Province = "Lleida"}
            if(p == 43){data1y$Province = "Tarragona"}
            
            data1y$Standing_ha <- scn$volume$summary[as.character(s), "standing"]
            data1y$Growth_ha <- scn$volume$summary[as.character(s), "standgrowth"]
            data1y$Dead_ha <- scn$volume$summary[as.character(s), "dead"]
            data1y$Extraction_ha <- scn$volume$summary[as.character(s), "extracted"]
            data1y$Extraction <- colSums(scn$volume$extractedSpp)[as.character(s)]
            data1y$Growth <- colSums(scn$volume$standgrowthSpp)[as.character(s)]
            
            data <- rbind(data, data1y)
          }
        }
      }
    } else {
      for(r in rcp){ # for each climatic scenario
        for(y in years){ # for each step
          for(p in prov){
            dir <- paste0("Rdata/Scenarios/",m,"/",m,"_",r,"/",m,"_", y,"_",r,"/",p,".rds" )
            scn <- readRDS(dir)
            if(scn$steps>1){stop("more than 1 step")}
            if(scn$numYears != 10){stop("wrong numYears")}
            
            data1y <- data.frame(matrix(ncol = length(column_names), nrow = 1))
            colnames(data1y) <- column_names
            
            data1y$Climate <- paste0("rcp",r)
            data1y$Management <- m
            data1y$Years <- y 
            if(p == 8){data1y$Province = "Barcelona"}
            if(p == 17){data1y$Province = "Girona"}
            if(p == 25){data1y$Province = "Lleida"}
            if(p == 43){data1y$Province = "Tarragona"}
            
            data1y$Standing_ha <- scn$volume$summary["1", "standing"]
            data1y$Growth_ha <- scn$volume$summary["1", "standgrowth"]
            data1y$Dead_ha <- scn$volume$summary["1", "dead"]
            data1y$Extraction_ha <- scn$volume$summary["1", "extracted"]
            data1y$Extraction <- colSums(scn$volume$extractedSpp)["1"]
            data1y$Growth <- colSums(scn$volume$standgrowthSpp)["1"]
            
            data <- rbind(data, data1y)
          }
        }
      }
    }
    
    cli::cli_progress_done()
  }
  data$Extraction_Rate <- 100*data$Extraction/data$Growth
  return(data)
}

data_prova <- data_creation()

##### plotting variables #####

# colors 
mgm_colors <- setNames(c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33"),
                       c("ACG", "AMF", "ASEA","BAU", "NOG","RSB"))

var = "Dead_ha"
data_prova$y <- data_prova[[var]]
g<-ggplot(data_prova, aes(x = Years, y=y/10 , fill=Management))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_vline(xintercept = 2.5, linetype="dashed")+
  annotate("rect", xmin = 0.5, xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = 0.3)+
  # scale_fill_brewer("Management", type = "qual", palette = "Set1")+
  scale_fill_manual(values = mgm_colors) +
  facet_grid(rows = vars(Climate), cols = vars(Province))+
  theme_bw()+
  ylab(paste0("Dead", " (m3/ha/year)"))+
  # ylab(expression(paste("Growth", " ",10^{6}," (m3/decade)"))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) # +
  # geom_hline(yintercept = 30, linetype="dotted")+
  # geom_hline(yintercept = 40, linetype="dotted")+
  # geom_hline(yintercept = 70, linetype="dotted")+
  # geom_hline(yintercept = 100, linetype="dotted")
g 
ggsave(paste0("Plots_Giuseppe/Province/",var,".png"), g, width = 16, height = 8)



###### group for catalunya ####

data_prova_Cat <- data_prova |>
  dplyr::group_by(Climate, Management, Years) |>
  dplyr::summarise(Standing_ha = mean(Standing_ha),
                   Growth = sum(Growth),
                   Growth_ha = mean(Growth_ha),
                   Dead_ha = mean(Dead_ha),
                   Extraction = sum(Extraction),
                   Extraction_ha = mean(Extraction_ha),
                   Extraction_Rate = mean(Extraction_Rate), .groups = "drop")

# var = "Standing_ha"
data_prova_Cat$y <- data_prova_Cat[[var]]
g<-ggplot(data_prova_Cat, aes(x = Years, y=y/10, fill=Management))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_vline(xintercept = 2.5, linetype="dashed")+
  annotate("rect", xmin = 0.5, xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = 0.3)+
  # scale_fill_brewer("Management", type = "qual", palette = "Set1")+
  scale_fill_manual(values = mgm_colors) +
  facet_wrap(vars(Climate), nrow = 1)+theme_bw()+
  # ylab(var)+
  ylab(paste0("Dead", " (m3/ha/year)"))+
  # ylab(expression(paste("Growth", " ",10^{6}," (m3/decade)"))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) #+
  # geom_hline(yintercept = 30, linetype="dotted")+
  # geom_hline(yintercept = 40, linetype="dotted")+
  # geom_hline(yintercept = 70, linetype="dotted")+
  # geom_hline(yintercept = 100, linetype="dotted")
g

ggsave(paste0("Plots_Giuseppe/Catalunya/",var,"_Cat.png"), g, width = 16, height = 8)


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#                                                                                                    #
#                        DEMAND Data frame creation directly from FORMES output                           ----
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#



## example demand 
demand_bau <- readRDS("C:/Users/giuseppe.capizzi/OneDrive - ctfc.cat/OCCC_ScnForestals/Rdata/Demand/BAU/BAU_45/demand_2021-2030.rds")

View(demand_bau)

# comparison between 1 saved and one demand_ret()

# saved BAU 45 2021-2030
ex_demand <- readRDS("C:/Users/giuseppe.capizzi/OneDrive - ctfc.cat/OCCC_ScnForestals/Rdata/Demand/BAU/BAU_45/demand_2021-2030.rds")
# demand_ret (barcelona)
# load fun
source("Rscripts/B2c_demand_cal.R")
demand_2001_2020 <- readRDS("C:/Users/giuseppe.capizzi/OneDrive - ctfc.cat/OCCC_ScnForestals/Rdata/demand_2001_2020.rds")
scn <- readRDS("C:/Users/giuseppe.capizzi/OneDrive - ctfc.cat/OCCC_ScnForestals/Rdata/Scenarios/BAU/BAU_45/BAU_2021-2030_45/8.rds")
bau_dem_ret <- demand_ret(prov = 8, scn = scn)

# comparison
str(ex_demand$Barcelona)
str(bau_dem_ret)


demand_data_creation <- function(mgms = c("BAU", "RSB", "ACG", "AMF", "ASEA"),
                          rcp = c("45", "85"),
                          provinces = c("Barcelona", "Girona", "Lleida", "Tarragona"),
                          years = c("2021-2030","2031-2040","2041-2050","2051-2060",
                                    "2061-2070","2071-2080","2081-2090","2091-2100")){
  # browser()
  cli::cli_h3("Binding demand dataframe")
  
  column_names <- c("Climate","Management","Years","Province","AnnualDemand", "Species", "Name")
  data <- data.frame(matrix(ncol = length(column_names), nrow = 0))
  colnames(data) <- column_names
  # add BAU (first 2 decades)
  dem <- readRDS("Rdata/demand_2001_2020.rds")
  for(m in mgms){ # for each management
    for(r in rcp){ # for each climatic scenario
      for(p in provinces){
        for(s in 1:length(unique(dem[[p]]$Step))){
          dem_s <- dem[[p]][dem[[p]]$Step == s,]
          data1y <- data.frame(matrix(ncol = 0, nrow = nrow(dem_s)))
      
          data1y$Climate <- paste0("rcp",r)
          data1y$Management <- m
          data1y$Years <- ifelse(s == 1, "2001-2010", "2011-2020")
          data1y$Province = p
          data1y<- cbind(data1y,dem_s[,c("AnnualDemand","Species","Name")])
      
          data <- rbind(data, data1y)
        }
      }
    }
  }
  
  
  for(m in mgms){ # for each management
    cli::cli_progress_step(paste0("Management ", m))
    for(r in rcp){ # for each climatic scenario
      for(y in years){ # for each step
        dir <- paste0("Rdata/Demand/",m,"/",m,"_",r,"/demand_",y,".rds" )
        dem <- readRDS(dir)
        for(p in provinces){
          if(unique(dem[[p]]$Step)!=1){stop("wrong step number")}
          
          data1y <- data.frame(matrix(ncol = 0, nrow = nrow(dem[[p]])))
          
          data1y$Climate <- paste0("rcp",r)
          data1y$Management <- m
          data1y$Years <- y 
          data1y$Province = p
          data1y<- cbind(data1y,dem[[p]][,c("AnnualDemand","Species","Name")])
          
          data <- rbind(data, data1y)
        }
      }
    }
    cli::cli_progress_done()
  }
  
  return(data)
}

demand_data <- demand_data_creation(mgms = c("BAU", "RSB", "AMF", "ASEA"))

# ploting demand #####

var = "AnnualDemand"
demand_data$y <- demand_data[[var]]
g<-ggplot(demand_data, aes(x = Years, y=y, fill=Management))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_vline(xintercept = 2.5, linetype="dashed")+
  annotate("rect", xmin = 0.5, xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = 0.3)+
  # scale_fill_brewer("Management", type = "qual", palette = "Set1")+
  scale_fill_manual(values = mgm_colors) +
  facet_wrap(~Name, scales = "free") +
  theme_bw()+
  ylab(var)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
g
ggsave(paste0("Plots_Giuseppe/Province/",var,".png"), g, width = 16, height = 8)

###### group for catalunya ####

demand_Cat <- demand_data |>
  dplyr::group_by(Climate, Management, Years, Name) |>
  dplyr::summarise(AnnualDemand = sum(AnnualDemand), 
                   .groups = "drop") 

demand_Cat$y <- demand_Cat[[var]]
g<-ggplot(demand_Cat, aes(x = Years, y=y/1000, fill=Management))+
  geom_bar(stat="identity", position=position_dodge())+
  geom_vline(xintercept = 2.5, linetype="dashed")+
  annotate("rect", xmin = 0.5, xmax = 2.5, ymin = -Inf, ymax = Inf, alpha = 0.3)+
  # scale_fill_brewer("Management", type = "qual", palette = "Set1")+
  scale_fill_manual(values = mgm_colors) +
  facet_wrap(~Name, scales = "free") +
  theme_bw()+
  # ylab(paste0(var," (m3/y)"))+
  ylab(expression(paste("Annual Demand", " ",10^{3}," (m3/year)"))) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 6))
g



ggsave(paste0("Plots_Giuseppe/Catalunya/",var,"_Cat.png"), g, width = 16, height = 8)





















