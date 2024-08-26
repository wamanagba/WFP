

rm(list=ls(all=TRUE))
library(terra)
path <- '/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Mali/'
conf <- sf::st_read("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/CSO/MLI/clim_conflict_ips_overlays (ACCLED-2017-2022).geojson")
#get mali shapefile
bdy <- sf::st_as_sf(geodata::gadm(country = 'MALI',level = 1, path = tempdir() ))
bdy <- bdy[c('NAME_1')]

#rename admin 2 to remove accents
bdy$NAME_1 <- stringi::stri_trans_general(str = bdy$NAME_1, id = "Latin-ASCII")
#bdy$NAME_1 <- toupper(bdy$NAME_1)
bdy_MLI=bdy
#=====Create New Legend labels=========================
reLabel <- function(conf){
  county_conf <- conf
  i <- county_conf$intersect_conf_clim
  county_conf$intersect_conf_clim[i=="High conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "High conflict + Low climate impact" 
  county_conf$intersect_conf_clim[i=="High conflict-[High-Moderate levels of precipitation/Moderate-Low levels of drought stress]"] <-
    "High conflict + Moderate climate impact" 
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "High conflict + Moderate climate impact"
  county_conf$intersect_conf_clim[i=="High conflict-[Low levels of precipitation/High levels of drought stress]"] <-
    "High conflict + High climate impact" 
  
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "Moderate conflict + Low climate impact" 
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High-Moderate levels of precipitation/Moderate-Low levels of drought stress]"] <-
    "Moderate conflict + Moderate climate impact"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "Moderate conflict + Moderate climate impact"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Low levels of precipitation/High levels of drought stress]"] <-
    "Moderate conflict + High climate impact"
  
  county_conf$intersect_conf_clim[i=="Limited conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "Limited conflict + Low climate impact"
  county_conf$intersect_conf_clim[i=="Limited conflict-[High-Moderate levels of precipitation/Moderate-Low levels of drought stress]"] <-
    "Limited conflict + Moderate climate impact"
  county_conf$intersect_conf_clim[i=="Limited conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "Limited conflict + Moderate climate impact"
  
  i <- county_conf$intersect_conf_clim
  county_conf$clust[i=="Limited conflict + Low climate impact"] <- 9
  county_conf$clust[i=="Limited conflict + Moderate climate impact"] <- 8
  county_conf$clust[i=="Limited conflict + High climate impact"] <- 7
  
  county_conf$clust[i=="Moderate conflict + Low climate impact"] <- 6
  county_conf$clust[i=="Moderate conflict + Moderate climate impact"] <- 5
  county_conf$clust[i=="Moderate conflict + High climate impact"] <- 4
  
  county_conf$clust[i=="High conflict + Low climate impact"] <- 3
  county_conf$clust[i=="High conflict + Moderate climate impact"] <- 2
  county_conf$clust[i=="High conflict + High climate impact"] <- 1
  county_conf$clust <- as.factor(county_conf$clust)
  return(county_conf)
}

conf_MLI <-reLabel(conf)
conf_MLI <- conf_MLI[c("clust","NAME_1", "NAME_2", "FATALITIES", "climvar_medn_prec", "livelihoods",
                       "median_female_edu", "female_population", "median_male_edu", 
                       "climvar_NDWS_median","intersect_conf_clim")]
#labs <- unique(conf$intersect_conf_clim[order(conf$clust)]) 
#================================================Plot Map====================







path <- '/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Burkina_Faso/'
conf <- sf::st_read("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Burkina_Faso/BFA_clim_conflict_ips_overlays.geojson")
#get mali shapefile
bdy <- sf::st_as_sf(geodata::gadm(country = 'BURKINA FASO',level = 1, path = tempdir() ))
bdy <- bdy[c('NAME_1')]

#rename admin 2 to remove accents
bdy$NAME_1 <- stringi::stri_trans_general(str = bdy$NAME_1, id = "Latin-ASCII")
#bdy$NAME_1 <- toupper(bdy$NAME_1)
bdy_BF=bdy
#=====Create New Legend labels=========================
reLabel <- function(conf){
  county_conf <- conf
  i <- county_conf$intersect_conf_clim
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate levels of precipitation/Moderate levels of waterlogging]"] <-
    "Moderate conflict + Moderate climate impact" 
  
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate levels of precipitation/Moderate levels of waterlogging]"] <-
    "High conflict + Moderate climate impact"
  
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High levels of precipitation/High levels of waterlogging]"] <-
    "Moderate conflict + Low climate impact" 
  
  county_conf$intersect_conf_clim[i=="High conflict-[High levels of precipitation/High levels of waterlogging]" ] <-
    "High conflict + Low climate impact"
  
  county_conf$intersect_conf_clim[i=="Limited conflict-[High levels of precipitation/High levels of waterlogging]"] <-
    "Limited conflict + Low climate impact"
  
  county_conf$intersect_conf_clim[i== "Limited conflict-[Moderate levels of precipitation/Moderate levels of waterlogging]"] <-
    "Limited conflict + Moderate climate impact"
  
  county_conf$intersect_conf_clim[i=="High conflict-[Low levels of precipitation/High levels of heat stress]"] <-
    "High conflict + High climate impact" 
  
  county_conf$intersect_conf_clim[i=="Limited conflict-[Low levels of precipitation/High levels of heat stress]"] <-
    "Limited conflict + High climate impact"
  
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Low levels of precipitation/High levels of heat stress]"   ] <-
    "Moderate conflict + High climate impact"
  
  i <- county_conf$intersect_conf_clim
  county_conf$clust[i=="Limited conflict + Low climate impact"] <- 9
  county_conf$clust[i=="Limited conflict + Moderate climate impact"] <- 8
  county_conf$clust[i=="Limited conflict + High climate impact"] <- 7
  
  
  county_conf$clust[i=="Moderate conflict + Low climate impact"] <- 6
  county_conf$clust[i=="Moderate conflict + Moderate climate impact"] <- 5
  county_conf$clust[i=="Moderate conflict + High climate impact"] <- 4
  
  county_conf$clust[i=="High conflict + Low climate impact"] <- 3
  county_conf$clust[i=="High conflict + Moderate climate impact"] <- 2
  county_conf$clust[i=="High conflict + High climate impact"] <- 1
  
  county_conf$clust <- as.factor(county_conf$clust)
  return(county_conf)
}



conf_BF <-reLabel(conf)
conf_BF <- conf_BF[c("clust","NAME_1", "NAME_2", "FATALITIES", "climvar_medn_prec", "livelihoods",
                       "median_female_edu", "female_population", "median_male_edu", 
                       "climvar_NDWS_median","intersect_conf_clim")]
#labs <- unique(conf$intersect_conf_clim[order(conf$clust)]) 
#================================================Plot Map====================








path <- '/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Niger/'
conf <- sf::st_read("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Niger/NER_clim_conflict_ips_overlays.geojson")
#get mali shapefile
bdy <- sf::st_as_sf(geodata::gadm(country = 'NIGER',level = 1, path = tempdir() ))
bdy <- bdy[c('NAME_1')]

#rename admin 2 to remove accents
bdy$NAME_1 <- stringi::stri_trans_general(str = bdy$NAME_1, id = "Latin-ASCII")
#bdy$NAME_1 <- toupper(bdy$NAME_1)
bdy_NER=bdy
#=====Create New Legend labels=========================
reLabel <- function(conf){
  county_conf <- conf
  i <- county_conf$intersect_conf_clim
  
  #conf= conflict
  #unique(conf$intersect_conf_clim)
  #county_conf <- st_intersection(conf, merged)
  i <- county_conf$intersect_conf_clim
  county_conf$intersect_conf_clim[i=="High conflict-[High levels of drought stress/Low precipitation]"] <-
    "High conflict + High climate impact"
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate-High levels of drought stress/Moderate-Low precipitation]"] <-
    "High conflict + Moderate climate impact"
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate-Low levels of drought stress/High-Moderate precipitation]"] <-
    "High conflict + Moderate climate impact"
  county_conf$intersect_conf_clim[i=="High conflict-[Low levels of drought stress/High precipitation]" ] <-
    "High conflict + Low climate impact"
  
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High levels of drought stress/Low precipitation]"] <-
    "Moderate conflict + High climate impact" 
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate-High levels of drought stress/Moderate-Low precipitation]"] <-
    "Moderate conflict + Moderate climate impact"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate-Low levels of drought stress/High-Moderate precipitation]"] <-
    "Moderate conflict + Moderate climate impact"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Low levels of drought stress/High precipitation]" ] <-
    "Moderate conflict + Low climate impact"
  
  county_conf$intersect_conf_clim[i=="Limited conflict-[High levels of drought stress/Low precipitation]"] <-
    "Limited conflict + High climate impact" 
  county_conf$intersect_conf_clim[i=="Limited conflict-[Moderate-High levels of drought stress/Moderate-Low precipitation]" ] <-
    "Limited conflict + Moderate climate impact"
  county_conf$intersect_conf_clim[i=="Limited conflict-[Moderate-Low levels of drought stress/High-Moderate precipitation]"] <-
    "Limited conflict + Moderate climate impact"
  county_conf$intersect_conf_clim[i=="Limited conflict-[Low levels of drought stress/High precipitation]"] <-
    "Limited conflict + Low climate impact"
  
  i <- county_conf$intersect_conf_clim
  county_conf$clust[i=="Limited conflict + Low climate impact"] <- 9
  county_conf$clust[i=="Limited conflict + Moderate climate impact"] <- 8
  county_conf$clust[i=="Limited conflict + High climate impact"] <- 7
  
  county_conf$clust[i=="Moderate conflict + Low climate impact"] <- 6
  county_conf$clust[i=="Moderate conflict + Moderate climate impact"] <- 5
  county_conf$clust[i=="Moderate conflict + High climate impact"] <- 4
  
  county_conf$clust[i=="High conflict + Low climate impact"] <- 3
  county_conf$clust[i=="High conflict + Moderate climate impact"] <- 2
  county_conf$clust[i=="High conflict + High climate impact"] <- 1
  county_conf$clust <- as.factor(county_conf$clust)
  return(county_conf)
}



conf_NGR <-reLabel(conf)
conf_NGR <- conf_NGR[c("clust","NAME_1", "NAME_2", "FATALITIES", "climvar_medn_prec", "livelihoods",
                           "median_female_edu", "female_population", "median_male_edu", 
                           "climvar_NDWS_median","intersect_conf_clim")]

#================================================Plot Map====================

conf = rbind(conf_BF,conf_MLI)
conf = rbind(conf,conf_NGR)

summary(conf)

labs <- unique(conf$intersect_conf_clim[order(conf$clust)]) 
labs= c("High conflict + High climate impact","High conflict + Moderate climate impact","High conflict + Low climate impact",
"Moderate conflict + High climate impact","Moderate conflict + Moderate climate impact","Moderate conflict + Low climate impact", 
"Limited conflict + High climate impact", "Limited conflict + Moderate climate impact","Limited conflict + Low climate impact")

bdy = rbind(bdy_BF,bdy_MLI)
bdy = rbind(bdy,bdy_NER)

library(tmap)
library(mapview)
tmap_mode("plot")
map <- tm_shape(conf) +
  #tm_fill(col= "clust", palette= c("red4", "red2", "orange2", "yellow", "grey85"), title="Conflict-climate intersection",
  #legend.show = T, labels=labs, popup.vars="NAME_3") + #"-YlOrRd" palette = viridis(100,direction	=-1)
  tm_fill(col= "clust", palette= viridis(9,direction	=-1, option = "H", alpha=0.7),style = "cont", title="Conflict-climate intersection",
          legend.show = T, labels=labs, popup.vars=c("NAME_1" ,"NAME_2", "FATALITIES", "climvar_medn_prec",  "livelihoods","median_female_edu", 
                                                     "female_population", "median_male_edu", "climvar_NDWS_median")
  ) + #"-YlOrRd" palette = viridis(100,direction	=-1)
  #tm_text("label", col='white', size = 1.1)+
  tm_shape(bdy)+
  tm_text("NAME_1", size = 1.0, col='black', remove.overlap = TRUE)+ 
  tm_borders(col = "black")+
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 75, 150), text.size = 1, 
               position = c("right", "bottom"))+
  tm_mouse_coordinates()+
  tm_layout(legend.outside=F, 
            legend.text.size = 0.9,
            legend.text.color = "black",
            legend.title.size= 1.2,
            legend.title.fontface = 2,
            legend.frame=F,
            asp=1.3,
            legend.just = c("left", "top"), 
            legend.position  = c("left", "top"),
            legend.width = 0.75,
            inner.margins = c(0.02, 0.02, 0.05, 0.02)
  )
map


BF_adm0=st_read("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/Migrations_Works/Data/gadm41_BFA_shp/gadm41_BFA_0.shp")
Niger_adm0= st_read("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/Migrations_Works/Data/gadm41_NER_shp/gadm41_NER_0.shp")
Mali_adm0 <- st_read("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/Migrations_Works/Data/gadm41_MLI_shp/gadm41_MLI_0.shp")
Boundaries = rbind(BF_adm0,Niger_adm0)
Boundaries = rbind(Boundaries,Mali_adm0)
plot(Boundaries)

tmap_mode("plot")
map <- tm_shape(conf) +
  tm_fill(col= "clust", palette= viridis(11,direction = -1, option = "H"),style = "cont", title="Conflict-climate intersection",alpha=0.9,
          legend.show = T, labels=labs, popup.vars=c("NAME_1" ,"NAME_3", "FATALITIES", "climvar_medn_prec", "livelihoods","median_female_edu", 
                                                     "female_population", "median_male_edu", "climvar_NDWS_median")
  ) +
  tm_shape(bdy) + # Appliquer les bordures et textes aux frontières administratives
  tm_borders(col = "black", lwd = .5) + # Utiliser des lignes noires pour les bordures
  tm_text("NAME_1", size = 1.0, col='black', remove.overlap = TRUE) +  # Appliquer les textes pour les noms des régions
  tm_shape(Boundaries) + # Ajouter les limites de "Boundaries" sans texte
  tm_borders(lty = "dashed", col = "black", lwd = 3) + # Utiliser des lignes en pointillés bleues
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 75, 150), text.size = 1, 
               position = c("right", "bottom"))+
  tm_mouse_coordinates()+
  tm_layout(
    legend.outside=F, 
    legend.text.size = 0.9,
    legend.text.color = "black",
    legend.title.size= 1.2,
    legend.title.fontface = 2,
    legend.frame=F,
    asp=1.3,
    legend.just = c("left", "top"), 
    legend.position = c("left", "top"),
    legend.width = 0.75,
    inner.margins = c(0,0.03,0,0.08) # Réglage des marges pour mieux ajuster le contenu
  )
print(map)

fpath <- '/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Burkina_Faso/'
tmap_save(map, dpi= 500,  width=15, height =10, units="in", filename=paste0(fpath, "Hospot2.2.png"))




map <- tm_shape(conf) +
  tm_fill(col= "clust", palette= viridis(11, direction = -1, option = "H", alpha = 0.7), title="Conflict-climate intersection",
          legend.show = T, labels=labs, popup.vars=c("NAME_2" ,"NAME_3", "FATALITIES", "climvar_medn_prec", "livelihoods","median_female_edu", 
                                                     "female_population", "median_male_edu", "climvar_NDWS_median")
  ) +
  tm_shape(bdy) + # Appliquer les bordures et textes aux frontières administratives
  tm_borders(col = "black", lwd = 1) + # Utiliser des lignes noires pour les bordures
  tm_text("NAME_1", size = 1.0, col='black', remove.overlap = TRUE) +  # Appliquer les textes pour les noms des régions
  tm_shape(Boundaries) + # Ajouter les limites de "Boundaries" sans texte
  tm_borders(lty = "dashed", col = "black", lwd = 3) + # Utiliser des lignes en pointillés bleues
  tm_add_legend(type = "line", col = "black", lwd = 1, title = "National boundary") +
  tm_add_legend(type = "line", lty = "dashed", col = "black", lwd = 1.5, title = "regional boundary") +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 75, 150), text.size = 1, 
               position = c("right", "bottom"))+
  tm_mouse_coordinates()+
  tm_layout(
    legend.outside = F, 
    legend.text.size = 0.9,
    legend.text.color = "black",
    legend.title.size= 1.2,
    legend.title.fontface = 2,
    legend.frame = F,
    asp = 1.3,
    legend.just = c("left", "top"), 
    legend.position = c("left", "top"),
    legend.width = 0.75,
    inner.margins = c(0, 0.03, 0, 0.08)
  )

print(map)
tmap_save(map, dpi= 500,  width=15, height =10, units="in", filename=paste0(fpath, "Hospot.png"))



map <- tm_shape(conf) +
  tm_fill(col= "clust", palette= viridis(11, direction = -1, option = "H", alpha = 0.7), title="Conflict-climate intersection",
          legend.show = T, labels=labs, popup.vars=c("NAME_2" ,"NAME_3", "FATALITIES", "climvar_medn_prec", "livelihoods","median_female_edu", 
                                                     "female_population", "median_male_edu", "climvar_NDWS_median")
  ) +
  tm_shape(bdy) +
  tm_borders(col = "grey", lwd = 1) +
  tm_text("NAME_1", size = 1.0, col='black', remove.overlap = TRUE) +
  tm_shape(BF_adm0) +
  tm_borders(lty = "dashed", col = "black", lwd = 2) +
  tm_shape(Mali_adm0) +
  tm_borders(lty = "dashed", col = "red", lwd = 2) +
  tm_shape(Niger_adm0) +
  tm_borders(lty = "dashed", col = "blue", lwd = 2) +
  tm_add_legend(type = "fill", col = NA, border.col = NA,lwd = 8, title = " ") + # Add title for borders legend
  tm_add_legend(type = "line", lty = "dashed", col = "black", lwd = 1.5, title = "Burkina Faso border") +
  tm_add_legend(type = "line", lty = "dashed", col = "red", lwd = 1.5, title = "Mali border") +
  tm_add_legend(type = "line", lty = "dashed", col = "blue", lwd = 1.5, title = "Niger border") +
  tm_compass(type = "8star", position = c("right", "top")) +
  tm_scale_bar(breaks = c(0, 75, 150), text.size = 1, 
               position = c("right", "bottom"))+
  tm_mouse_coordinates()+
  tm_layout(
    legend.outside = F, 
    legend.text.size = 0.9,
    legend.text.color = "black",
    legend.title.size= 1.2,
    legend.title.fontface = 2,
    legend.frame = F,
    asp = 1.3,
    legend.just = c("left", "top"), 
    legend.position = c("left", "top"),
    legend.width = 0.75,
    inner.margins = c(0, 0.03, 0, 0.08)
  )

print(map)
tmap_save(map, dpi= 500,  width=15, height =10, units="in", filename=paste0(fpath, "Hospot2.png"))
