

rm(list=ls(all=TRUE))
library(readxl)
library(haven)
library(geodata)
library(sf)
library(tmap) #tmaptools::palette_explorer()
library("viridis")

library(tidyr)
library(dplyr)
year = 2024
path <- 'D:/Data/'

fip <- read_excel('D:/Data/cadreHarmonise2020-2024.xlsx')

fip <- fip %>% 
  filter(adm0_name %in% c("Burkina Faso", "Niger", "Mali"))


fip$adm2_name <- toupper(fip$adm2_name)
fip$adm1_name <- toupper(fip$adm1_name)
fip$FIP <- fip$FIP * 100
fip$FIP<- round(fip$FIP)

colnames(fip)[colnames(fip) == "adm1_name"] <- "NAME_1"
colnames(fip)[colnames(fip) == "adm2_name"] <- "NAME_2"
colnames(fip)[colnames(fip) == "adm0_name"] <- "Country"
fip <- fip[,c("Country","NAME_2",'Year', "FIP" )]




i <- fip$NAME_2
fip$NAME_2[i=="BAROUELI"] <- "BARAOUELI"
fip$NAME_2[i=="GUIDAN-ROUMDJI"] <-  "GUIDAN ROUMDJI"
fip$NAME_2[i== "KOMONJDJARI"] <-  "KOMANDJARI"  
fip$NAME_2[i=="KOURITENGA"] <- "KOURITTENGA"    
fip$NAME_2[i=="MAINE-SOROA"] <- "MAINE SOROA" 
fip$NAME_2[i== "VILLE DE DOSSO"] <- "DOSSO" 
fip$NAME_2[i=="TIBIRI (DOUTCHI)"] <- "TIBIRI" 

fip_Year <- fip %>% 
  filter(Year %in% c(paste0(year)))
fip_Year <- fip_Year[fip_Year$FIP > 19, ]
fip_Year<- fip_Year[order(fip_Year$FIP,decreasing = T),]
library(openxlsx)
write.xlsx(fip_Year, file = "D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Products/FIP/Final_last/table.xlsx")


YearC=year-1
conflict <- sf::st_read(paste0("D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Data/Conflicts/clim_conflict_ips_overlays_",YearC,".geojson"))
conflict = conflict[,c("conflict_clust_label","clim_cluster_short_label","intersect_conf_clim","NAME_0","NAME_1","NAME_2")]
colnames(conflict)[colnames(conflict) == "conflict_clust_label"] <- "label"

conf_no_limited <- function(conf){
  county_conf <- conf[conf$label != "Limited conflict"  ,]
  #county_conf <- st_intersection(county_conf, merged)
  i <- county_conf$intersect_conf_clim
  county_conf$intersect_conf_clim[i=="High conflict-[Low levels of drought stress/High precipitation]"] <-
    "High conflict + Low drought stress" 
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate-Low levels of drought stress/High-Moderate precipitation]"] <-
    "High conflict + Moderate-Low drought stress"
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate-High levels of drought stress/Moderate-Low precipitation]" ] <-
    "High conflict + Moderate-High drought stress"
  county_conf$intersect_conf_clim[i=="High conflict-[High levels of drought stress/Low precipitation]"] <-
    "High conflict + High drought stress"
  
  
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Low levels of drought stress/High precipitation]"] <-
    "Moderate conflict + Low drought stress" 
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate-Low levels of drought stress/High-Moderate precipitation]"] <-
    "Moderate conflict + Moderate-Low drought stress"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate-High levels of drought stress/Moderate-Low precipitation]"] <-
    "Moderate conflict + Moderate-High drought stress"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High levels of drought stress/Low precipitation]"] <-
    "Moderate conflict + High drought stress"
  
  i <- county_conf$intersect_conf_clim
  
  county_conf$clust[i=="Moderate conflict + Low drought stress"] <- 8
  county_conf$clust[i=="Moderate conflict + Moderate-Low drought stress"] <- 7
  county_conf$clust[i=="Moderate conflict + Moderate-High drought stress"] <- 6
  county_conf$clust[i=="Moderate conflict + High drought stress"] <- 5
  
  county_conf$clust[i=="High conflict + Low drought stress"] <- 4
  county_conf$clust[i=="High conflict + Moderate-Low drought stress"] <- 3
  county_conf$clust[i=="High conflict + Moderate-High drought stress"] <- 2
  county_conf$clust[i=="High conflict + High drought stress"] <- 1
  county_conf$clust <- as.factor(county_conf$clust)
  return(county_conf)
}

conf=conflict__no_limited <- conf_no_limited(conflict)
merged_shapefile <- st_make_valid(merged_shapefile)

conflict <- st_join(conflict, merged_shapefile, join = st_within)
conflict$n=1
N_conflicts <- conflict %>%
  group_by(NAME_2.y, label) %>%
  summarise(N = round(sum(n, na.rm = TRUE),2))

N_conflicts <- N_conflicts[N_conflicts$label != "Limited conflict"  ,]
N_conflicts <- na.omit(N_conflicts)
colnames(N_conflicts)[1]='NAME_2'
N_conflicts$NAME_2 = toupper(N_conflicts$NAME_2)
dd = merge(N_conflicts,fip_Year,by =c("NAME_2"))
