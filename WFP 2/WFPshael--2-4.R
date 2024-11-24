

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



#get  shapefile
Mali_adm2 <- sf::st_read("D:/Data/gadm/HDX/MLI/mli_admbnda_adm2_1m_gov_20211220.shp")
Mali_adm2 <- Mali_adm2[c('ADM2_FR')]
colnames(Mali_adm2)[colnames(Mali_adm2) == "ADM2_FR"] <- "NAME_2"
Niger_adm2 <- sf::st_read("D:/Data/gadm/HDX/NER/NER_admbnda_adm2_IGNN_20230720_em.shp")
Niger_adm2 <- Niger_adm2[c('ADM2_FR')]
colnames(Niger_adm2)[colnames(Niger_adm2) == "ADM2_FR"] <- "NAME_2"
BF_adm2 <- sf::st_read("D:/Data/gadm/HDX/BFA/bf_admin2_hno_2022.shp")
BF_adm2 <- BF_adm2[c('adm2_name1')]
colnames(BF_adm2)[colnames(BF_adm2) == "adm2_name1"] <- "NAME_2"

merged_shapefile <- rbind(BF_adm2, Mali_adm2, Niger_adm2)

Mali_adm0 <- sf::st_as_sf(geodata::gadm(country = 'MALI',level = 0, path = path))
Niger_adm0 <- sf::st_as_sf(geodata::gadm(country = 'NIGER',level = 0, path = path))
BF_adm0 <- sf::st_as_sf(geodata::gadm(country = 'BURKINA FASO',level = 0, path = path))

Boundaries <- rbind(BF_adm0, Mali_adm0, Niger_adm0)

#plot(merged_shapefile['NAME_1'])


#merged_shapefile <- merged_shapefile[c('NAME_2')]



#rename admin 2 to remove accents
merged_shapefile$NAME_2 <- stringi::stri_trans_general(str = merged_shapefile$NAME_2, id = "Latin-ASCII")


fip$FIP <- fip$FIP * 100
fip$FIP<- round(fip$FIP)

merged_shapefile$NAME_2 <- toupper(merged_shapefile$NAME_2)


adm2 = merged_shapefile
#Check naming fidelity
nameCheck <- function(admin, df, level){
  if(level==1){
    a <- sort(unique(admin$NAME_1))
    b <- sort(unique(df$NAME_1))
    tt=b %in% a
    print(b[!tt])
  }
  if(level==2){
    a <- sort(unique(admin$NAME_2))
    b <- sort(unique(df$NAME_2))
    tt=b %in% a
    print(b[!tt])
  }
  
}

colnames(fip)[colnames(fip) == "adm1_name"] <- "NAME_1"
colnames(fip)[colnames(fip) == "adm2_name"] <- "NAME_2"
colnames(fip)[colnames(fip) == "adm0_name"] <- "Country"

fip_mean <- fip[,c("NAME_2",'Year', "FIP" )]

nameCheck(merged_shapefile, fip, 2)



i <- fip_mean$NAME_2
fip_mean$NAME_2[i=="BAROUELI"] <- "BARAOUELI"
fip_mean$NAME_2[i=="GUIDAN-ROUMDJI"] <-  "GUIDAN ROUMDJI"
fip_mean$NAME_2[i== "KOMONJDJARI"] <-  "KOMANDJARI"  
fip_mean$NAME_2[i=="KOURITENGA"] <- "KOURITTENGA"    
fip_mean$NAME_2[i=="MAINE-SOROA"] <- "MAINE SOROA" 
fip_mean$NAME_2[i== "VILLE DE DOSSO"] <- "DOSSO" 
fip_mean$NAME_2[i=="TIBIRI (DOUTCHI)"] <- "TIBIRI" 
#fip_mean$NAME_2[i=="MAINE-SOROA"] <- "MAINE SOROA" 

nameCheck(merged_shapefile, fip_mean, 2)
nameCheck(fip_mean, merged_shapefile, 2)

dF <- fip_mean %>%
  group_by(NAME_2, Year) %>%
  summarise(FIP = round(mean(FIP, na.rm = TRUE),2))

# "ANDERAMBOUKANE" "INEKAR" et "TIDERMENE" contiendront les valeurs de Taoudenit
i <- merged_shapefile$NAME_2
merged_shapefile$NAME_2[i=="ANDERAMBOUKANE"] <- "TAOUDENIT" 
merged_shapefile$NAME_2[i=="TIDERMENE"] <- "TAOUDENIT" 
merged_shapefile$NAME_2[i=="INEKAR"] <- "TAOUDENIT" 

fip_merg0 <- merge(merged_shapefile, dF, by=c("NAME_2"))



fip_merg <- fip_merg0 %>% 
  filter(Year %in% c(paste0(year)))

YearC=year-1
conflict <- sf::st_read(paste0("D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Data/Conflicts/clim_conflict_ips_overlays_",YearC,".geojson"))
colnames(conflict)[colnames(conflict) == "conflict_clust_label"] <- "label"



#conflict=conflict_2019 <- sf::st_read("D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Data/Conflicts/clim_conflict_ips_overlays_2019.geojson")
#conflict=conflict_2020 <- sf::st_read("D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Data/Conflicts/clim_conflict_ips_overlays_2020.geojson")
#conflict=conflict_2021 <- sf::st_read("D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Data/Conflicts/clim_conflict_ips_overlays_2021.geojson")
#conflict=conflict_2022 <- sf::st_read("D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Data/Conflicts/clim_conflict_ips_overlays_2022.geojson")
#names(conflict_2017)


unique(conflict$intersect_conf_clim)






#conf2=conflict_all <- all_cluster(conflict)

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

labs <- unique(conf$intersect_conf_clim[order(conf$clust)]) 
#unique(conflict__no_limited$intersect_conf_clim)
no_limited_label <- c("High conflict + High drought stress", "High conflict + Moderate-High drought stress", "High conflict + Moderate-Low drought stress",
                      "High conflict + Low drought stress", "Moderate conflict + High drought stress", "Moderate conflict + Moderate-High drought stress",
                      "Moderate conflict + Moderate-Low drought stress", "Moderate conflict + Low drought stress" )



unique(conflict__no_limited$clust)



#plotting CONFLICT FIP
#=================================================================



tmap_options(check.and.fix = TRUE)
tmap_mode('plot')

# Remplacement des valeurs manquantes avec -1
fip_merg$FIP <- ifelse(is.na(fip_merg$FIP), -1, fip_merg$FIP)  

# Création de la carte
map <- tm_shape(fip_merg) +
  tm_fill(col = "FIP", 
          title = "Food Insecure Population (%)", 
          style = "fixed",  
          breaks = c(-1, 0, 20, 30, 40, 50, 60, 70, 80, 90, 100),  
          palette = c("grey", "#FFFFFF", mako(8, direction = -1)),  # Blanc (white) bien défini pour 0-20%
          legend.show = TRUE, 
          labels = c("Missing", "≤ 20%", "20 - 30%", 
                     "30 - 40%", "40 - 50%", "50 - 60%", 
                     "60 - 70%", "70 - 80%", "80 - 90%", "90 - 100%")) + 
  tm_shape(conflict__no_limited) +
  tm_fill(col = "clust", 
          palette = "-YlOrRd", 
          title = "Conflict-Climate Intersection", 
          alpha = 0.7,
          legend.show = TRUE, 
          labels = no_limited_label) +
  tm_shape(adm2) +
  tm_borders(col = "grey", lwd = 1.2) +
  tm_text("NAME_2", 
          size = 0.4, 
          remove.overlap = TRUE, 
          col = 'black', 
          fontface = 2) +
  tm_shape(Mali_adm0) +
  tm_borders(col = "#AA3377", lwd = 1.5) +
  tm_shape(Niger_adm0) +
  tm_borders(col = "#4477AA", lwd = 1.5) +
  tm_shape(BF_adm0) +
  tm_borders( col = "black", lwd = 1.5,alpha=0.7) +
  tm_add_legend(type = "fill", col = NA, border.col = NA, lwd = 8, title = " ") +
  tm_add_legend(type = "line",  col = "black", lwd = 1.5, title = "Burkina Faso border") +
  tm_add_legend(type = "line",  col = "#AA3377", lwd = 1.5, title = "Mali border") +
  tm_add_legend(type = "line",  col = "#4477AA", lwd = 1.5, title = "Niger border") +
  tm_compass(type = "8star", size = 3, position = c(.92, .13)) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 4, position = c(.929, 0.08)) +
  tm_layout(legend.outside = FALSE, 
            legend.text.size = 0.6,
            legend.text.color = "black",
            legend.title.size = 1,
            legend.title.color = "black",
            legend.title.fontface = 2,
            legend.frame = FALSE,
            asp = 1.3,
            legend.position = c("left", "top"), 
            legend.width = 0.5,
            inner.margins = c(0, 0.05, 0, 0.05)) 

# Sauvegarde de la carte
fpath <- 'D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Products/FIP/Final_last4/'
tmap_save(map, dpi = 300, width = 11, height = 8, units = "in", 
          filename = paste0(fpath, paste0("FIP_", year, ".png")))
