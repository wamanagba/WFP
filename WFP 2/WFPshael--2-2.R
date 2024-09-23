

rm(list=ls(all=TRUE))
library(readxl)
library(haven)
library(geodata)
library(sf)
library(tmap) #tmaptools::palette_explorer()
library("viridis")

library(tidyr)
library(dplyr)
year = 2020
path <- 'D:/Data/'

fip <- read_excel('D:/Data/cadreHarmonise2020-2024.xlsx')

fip <- fip %>% 
  filter(adm0_name %in% c("Burkina Faso", "Niger", "Mali"))


fip$adm2_name <- toupper(fip$adm2_name)
fip$adm1_name <- toupper(fip$adm1_name)



#get  shapefile
Mali_adm1 <- sf::st_as_sf(geodata::gadm(country = 'MALI',level = 1, path = path))
Niger_adm1 <- sf::st_as_sf(geodata::gadm(country = 'NIGER',level = 1, path = path))
BF_adm1 <- sf::st_as_sf(geodata::gadm(country = 'BURKINA FASO',level = 1, path = path))
merged_shapefile <- rbind(BF_adm1, Mali_adm1, Niger_adm1)

Mali_adm0 <- sf::st_as_sf(geodata::gadm(country = 'MALI',level = 0, path = path))
Niger_adm0 <- sf::st_as_sf(geodata::gadm(country = 'NIGER',level = 0, path = path))
BF_adm0 <- sf::st_as_sf(geodata::gadm(country = 'BURKINA FASO',level = 0, path = path))

Boundaries <- rbind(BF_adm0, Mali_adm0, Niger_adm0)

#plot(merged_shapefile['NAME_1'])


merged_shapefile <- merged_shapefile[c('NAME_1')]



#rename admin 2 to remove accents
merged_shapefile$NAME_1 <- stringi::stri_trans_general(str = merged_shapefile$NAME_1, id = "Latin-ASCII")


fip$FIP <- fip$FIP * 100
fip$FIP<- round(fip$FIP,2)

#merged_shapefile$NAME_2 <- toupper(merged_shapefile$NAME_2)
merged_shapefile$NAME_1 <- toupper(merged_shapefile$NAME_1)

adm2 = merged_shapefile
#Check naming fidelity
nameCheck <- function(admin, df, level){
  if(level==1){
    a <- sort(unique(admin$NAME_1))
    b <- sort(unique(df$NAME_1))
    tt=b %in% a
    print(b[!tt])
  }
  if(level==3){
    a <- sort(unique(admin$NAME_3))
    b <- sort(unique(df$NAME_3))
    tt=b %in% a
    print(b[!tt])
  }
  
}

colnames(fip)[colnames(fip) == "adm1_name"] <- "NAME_1"
colnames(fip)[colnames(fip) == "adm2_name"] <- "NAME_2"
colnames(fip)[colnames(fip) == "adm0_name"] <- "Country"

fip_mean <- fip[,c("NAME_1",'Year', "FIP" )]

nameCheck(merged_shapefile, fip, 1)



i <- fip_mean$NAME_1
fip_mean$NAME_1[i=="HAUTS-BASSINS"] <- "HAUT-BASSINS"
fip_mean$NAME_1[i=="PLATEAU CENTRAL"] <- "PLATEAU-CENTRAL"
fip_mean$NAME_1[i=="TILLABERI"] <- "TILLABERY"
fip_mean$NAME_1[i=="TOMBOUCTOU"] <- "TIMBUKTU"


nameCheck(merged_shapefile, fip_mean, 1)

dF <- fip_mean %>%
  group_by(NAME_1, Year) %>%
  summarise(FIP = round(mean(FIP, na.rm = TRUE),2))


fip_merg0 <- merge(merged_shapefile, dF, by=c("NAME_1"))


fip_merg <- fip_merg0 %>% 
  filter(Year %in% c(paste0(year)))

YearC=year-1
conflict <- sf::st_read(paste0("D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Data/Conflicts/clim_conflict_ips_overlays_",YearC,".geojson"))




#conflict=conflict_2019 <- sf::st_read("D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Data/Conflicts/clim_conflict_ips_overlays_2019.geojson")
#conflict=conflict_2020 <- sf::st_read("D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Data/Conflicts/clim_conflict_ips_overlays_2020.geojson")
#conflict=conflict_2021 <- sf::st_read("D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Data/Conflicts/clim_conflict_ips_overlays_2021.geojson")
#conflict=conflict_2022 <- sf::st_read("D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Data/Conflicts/clim_conflict_ips_overlays_2022.geojson")
#names(conflict_2017)


#unique(conflict$intersect_conf_clim)





all_cluster <- function(conf){
  #county_conf <- st_intersection(conf, merged_shapefile)
  county_conf=conf
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
  
  county_conf$intersect_conf_clim[i=="Limited conflict-[Low levels of drought stress/High precipitation]"] <-
    "Limited conflict + Low drought stress" 
  county_conf$intersect_conf_clim[i=="Limited conflict-[Moderate-Low levels of drought stress/High-Moderate precipitation]" ] <-
    "Limited conflict + Moderate-Low drought stress"
  county_conf$intersect_conf_clim[i=="Limited conflict-[Moderate-High levels of drought stress/Moderate-Low precipitation]"] <-
    "Limited conflict + Moderate-High drought stress"
  county_conf$intersect_conf_clim[i=="Limited conflict-[High levels of drought stress/Low precipitation]"] <-
    "Limited conflict + High drought stress"
  
  i <- county_conf$intersect_conf_clim
  county_conf$clust[i=="Limited conflict + Low drought stress"] <- 12
  county_conf$clust[i=="Limited conflict + Moderate-Low drought stress"] <- 11
  county_conf$clust[i=="Limited conflict + Moderate-High drought stress"] <- 10
  county_conf$clust[i=="Limited conflict + High drought stress"] <- 9
  
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


limited_label <- c("High conflict + High drought stress", "High conflict + Moderate-High drought stress", "High conflict + Moderate-Low drought stress",
                      "High conflict + Low drought stress", "Moderate conflict + High drought stress", "Moderate conflict + Moderate-High drought stress",
                      "Moderate conflict + Moderate-Low drought stress", "Moderate conflict + Low drought stress","Limited conflict + High drought stress",
                   "Limited conflict + Moderate-High drought stress","Limited conflict + Moderate-Low drought stress","Limited conflict + Low drought stress")


unique(conflict__no_limited$clust)

#unique(conf2$clust)

#plotting CONFLICT FIP
#=================================================================


map <- tm_shape(fip_merg) +
  tm_fill(col = "FIP", 
          title = "Food Insecure Population (%)", 
          style = "cont", 
          breaks = seq(0, 50, by = 5),  # Set breaks from 0 to 30 with steps of 5
          palette = mako(10, direction = -1), 
          legend.show = TRUE) +
  tm_shape(conflict__no_limited) +
  tm_fill(col = "clust", 
          palette = "-YlOrRd", 
          title = "Conflict-Climate Intersection", 
          alpha = 0.7,
          legend.show = TRUE, 
          labels = no_limited_label) +
  tm_shape(adm2) +
  tm_borders(col = "grey", lwd = 0.9) +
  tm_text("NAME_1", 
          size = 0.5, 
          remove.overlap = TRUE, 
          col = 'black', 
          fontface = 2) +
  tm_shape(Mali_adm0) +
  tm_borders(col = "red", lwd = 1, lty = "dotdash") +
  tm_shape(Niger_adm0) +
  tm_borders(lty = "twodash", col = "blue", lwd = 1) +
  tm_shape(BF_adm0) +
  tm_borders(lty = "dashed", col = "black", lwd = 1) +
  tm_add_legend(type = "fill", col = NA, border.col = NA, lwd = 8, title = " ") +
  tm_add_legend(type = "line", lty = "twodash", col = "black", lwd = 1.5, title = "Burkina Faso border") +
  tm_add_legend(type = "line", lty = "dotdash", col = "red", lwd = 1.5, title = "Mali border") +
  tm_add_legend(type = "line", lty = "dashed", col = "blue", lwd = 1.5, title = "Niger border") +
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

map



fpath <- 'D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Products/FIP/FIP_final/'
tmap_save(map, dpi= 300,  width=11, height =8, units="in",
          filename=paste0(fpath, paste0("FIP_",year,".png")))




### FSC data proccessing #####
##############################

path <- 'D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Data/FCS/'
df1 <- read_sav(paste0(path,'ALL_2018_2023_Data_Fusion_PDM_Resilience.sav'))

temp <- df1 %>% 
  filter(ADMIN0Name %in% c("BURKINA FASO", "MALI", "NIGER"))
temp$ADMIN1Name <- toupper(temp$ADMIN1Name)
temp$ADMIN2Name <- toupper(temp$ADMIN2Name)

# remplacer les valeurs manquante dans la colone ADMIN2Name
#Burkina Faso
for (i in 1:length(temp$ADMIN2Name)) {
  if(temp$ADMIN2Name[i]=="SENO"){temp$ADMIN1Name[i]="SAHEL"}
  if(temp$ADMIN2Name[i]=="ZONDOMA" || temp$ADMIN2Name[i]=="YATENGA" || temp$ADMIN2Name[i]=="PASSORE"){temp$ADMIN1Name[i]="NORD"}
  if(temp$ADMIN2Name[i]=="SANMATENGA" || temp$ADMIN2Name[i]=="NAMENTENGA"){temp$ADMIN1Name[i]="CENTRE-NORD"}
  if(temp$ADMIN2Name[i]=="GNAGNA" || temp$ADMIN2Name[i]=="GNAGNA"){temp$ADMIN1Name[i]="EST"}
}

#Niger
for (i in 1:length(temp$ADMIN2Name)) {
  if(temp$ADMIN2Name[i]=="VILLE DE M"|| temp$ADMIN2Name[i]== "MAYAHI" || temp$ADMIN2Name[i]=="DAKORO" || temp$ADMIN2Name[i]=="MADAROUNFA" || temp$ADMIN2Name[i]=="TESSAOUA" || temp$ADMIN2Name[i]=="GUIDAN ROU" || temp$ADMIN2Name[i]=="GAZAOUA"){temp$ADMIN1Name[i]="MARADI"}
  if(temp$ADMIN2Name[i]=="BELBEDJI" || temp$ADMIN2Name[i]=="MAGARIA" || temp$ADMIN2Name[i]=="MIRRIAH" || temp$ADMIN2Name[i]=="KANTCHE"){temp$ADMIN1Name[i]="ZINDER"}
  if(temp$ADMIN2Name[i]=="ABALAK" || temp$ADMIN2Name[i]=="BOUZA"){temp$ADMIN1Name[i]="TAHOUA"}
}

#Mali
i <- temp$ADMIN1Name 
temp$ADMIN1Name[i=="TOMBOUTOU" | i=="TIMBUKTU" ] <- "TIMBUKTU"
temp$ADMIN1Name[i=="TOMBOUCTOU"] <- "TIMBUKTU"
temp$ADMIN1Name[i=="MENAKA"] <- "GAO"
temp$ADMIN1Name[i=="BADIANGARA"] <- "BANDIAGARA"
temp$ADMIN1Name[i=="BARAOUELI"] <- "BAROUELI"
temp$ADMIN1Name[i=="GOURMA_RHAROUS"] <- "GOURMA-RHAROUS"
temp$ADMIN1Name[i=="NIAFOUNKE"] <- "NIAFUNKE"
temp$ADMIN1Name[i=="TIN ESSAKO"] <- "TIN-ESSAKO" 
temp$ADMIN1Name[i=="TENENKOUN"] <- "TENENKOU" 

#Burkina Faso
temp$ADMIN1Name[i=="CENTRE_NORD" | i=="CENTRNORD" ] <- "CENTRE-NORD"
temp$ADMIN1Name[i=="PLATEAU CENTRAL"] <- "PLATEAU-CENTRAL"
temp$ADMIN1Name[i=="PLATEAU CENTRAL"] <- "PLATEAU-CENTRAL"
temp$ADMIN1Name[i=="HAUTS-BASSINS"] <- "HAUTS-BASSINS"

#Niger
temp$ADMIN1Name[i=="TILLABERI"] <- "TILLABERY"

fcs_mean <- aggregate(FCS~YEAR+ADMIN1Name, data=temp,mean, na.rm=T)
names(fcs_mean)[2] <- "NAME_1"
#Year=2023

fcs_mean$YEAR= as.numeric(fcs_mean$YEAR)
fcs_mean <- fcs_mean[fcs_mean$YEAR <= year, ]
fcs_mean <- fcs_mean[fcs_mean$NAME_1 != '', ]
fcs_mean <- aggregate(FCS~NAME_1, data=fcs_mean,mean, na.rm=T)

adm2 = merged_shapefile



merged <- merge(adm2, fcs_mean, by="NAME_1", all=TRUE)




#plotting FCS
#=================================================================
tmap_mode("plot")
map <- tm_shape(merged)+
  tm_fill(col="FCS", title="FCS",style = "cont", palette = viridis(100,direction	=-1),legend.show = T)+
  tm_shape(conflict__no_limited) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Conflict-Climate Intersection",
          legend.show = T, labels=no_limited_label)+
  tm_shape(adm2)+
  tm_borders(col="black",lwd=0.5)+
  tm_text("NAME_1", size = 0.7, remove.overlap = TRUE, col ='white')+
  tm_shape(Mali_adm0) +
  tm_borders(col = "red", lwd = 2, lty = "dotdash") +  # Changed to a thicker, solid line for prominence
  tm_shape(Niger_adm0) +
  tm_borders(lty = "twodash", col = "blue", lwd = 2) +
  tm_shape(BF_adm0) +
  tm_borders(lty = "dashed", col = "black", lwd = 2) +
  tm_add_legend(type = "fill", col = NA, border.col = NA,lwd = 8, title = " ") + # Add title for borders legend
  tm_add_legend(type = "line", lty="twodash",col = "black", lwd = 1.5, title = "Burkina Faso border") +
  tm_add_legend(type = "line", lty = "dotdash", col = "red", lwd = 1.5, title = "Mali border") +  # Updated legend
  tm_add_legend(type = "line", lty = "dashed", col = "blue", lwd = 1.5, title = "Niger border") +
  tm_compass(type = "8star", size=4,position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 1.5, 
               position = c("right", "bottom"))+
  tm_layout(legend.outside=F, 
            legend.text.size = 1,
            legend.text.color = "black",
            legend.title.size= 1,
            legend.title.color = "black",
            legend.title.fontface = 2,
            legend.frame=F,
            asp=1.3,
            legend.position = c("left", "top"), 
            legend.width = 0.5,
            #bg.color = 'grey85',
            inner.margins = c(0,0.05,0,0.05)
  )

map
fpath="D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Products/FCS/"
tmap_save(map, dpi= 300,  width=11, height =8, units="in",
          filename=paste0(fpath,year,"_FCS.png"))






############ Hospots


tmap_mode("plot")
map <- tm_shape(conf) +
  tm_fill(col= "clust", palette= viridis(11,direction = -1, option = "H"),style = "cont", title="Conflict-climate intersection",alpha=0.9,
          legend.show = T, labels=labs
  ) +
  tm_shape(adm2) + # Appliquer les bordures et textes aux frontières administratives
  tm_borders(col = "black", lwd = .5) + # Utiliser des lignes noires pour les bordures
  tm_text("NAME_1", size = 0.5, col='black', remove.overlap = TRUE) +  # Appliquer les textes pour les noms des régions
  tm_shape(Mali_adm0) +
  tm_borders(col = "red", lwd = 2, lty = "dotdash") +  # Changed to a thicker, solid line for prominence
  tm_shape(Niger_adm0) +
  tm_borders(lty = "twodash", col = "blue", lwd = 2) +
  tm_shape(BF_adm0) +
  tm_borders(lty = "dashed", col = "black", lwd = 2) +
  tm_add_legend(type = "fill", col = NA, border.col = NA,lwd = 8, title = " ") + # Add title for borders legend
  tm_add_legend(type = "line", lty="twodash",col = "black", lwd = 1.5, title = "Burkina Faso border") +
  tm_add_legend(type = "line", lty = "dotdash", col = "red", lwd = 1.5, title = "Mali border") +  # Updated legend
  tm_add_legend(type = "line", lty = "dashed", col = "blue", lwd = 1.5, title = "Niger border") +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 1, 
               position = c("right", "bottom"))+
  tm_mouse_coordinates()+
  tm_layout(legend.outside=F, 
            legend.text.size = .5,
            legend.text.color = "black",
            legend.title.size= 1,
            legend.title.color = "black",
            legend.title.fontface = 2,
            legend.frame=F,
            asp=1.3,
            legend.position = c("left", "top"), 
            legend.width = 0.5,
            #bg.color = 'grey85',
            inner.margins = c(0.1,0.1,0.3,0.1)
            # Réglage des marges pour mieux ajuster le contenu
  )
map
fpath="D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Products/Hospot/"
tmap_save(map, dpi= 300,  width=11, height =8, units="in", filename=paste0(fpath,YearC,"_Hospot.png"))








