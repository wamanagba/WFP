




rm(list=ls(all=TRUE))
library(readxl)
library(haven)
library(geodata)
library(sf)
library(tmap) #tmaptools::palette_explorer()
library("viridis")
library(readxl)
library(tidyr)
library(dplyr)

path <- 'D:/Data/'

fip <- read_excel('D:/Data/cadre_harmonise_caf_ipc_Mar24-copy2.xlsx')

fip <- fip %>% 
  filter(adm0_name %in% c("Burkina Faso", "Niger", "Mali"))
fip <- fip %>% 
  filter(exercise_label %in% c("Sep-Dec"))

fip <- fip %>% 
  filter(chtype %in% c("current"))
unique(fip$exercise_year)

fip$adm2_name <- toupper(fip$adm2_name)
fip$adm1_name <- toupper(fip$adm1_name)



#get mali shapefile
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
colnames(fip)[colnames(fip) == "reference_year"] <- "Year"

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


fip_merg <- merge(merged_shapefile, dF, by=c("NAME_1"))

fip_merg <- fip_merg %>% 
  filter(Year %in% c("2017","2018","2019","2020","2021","2022"))

#plot(fip_merg['FIP'])

#read conflict data
# Load geospatial data for each year
conflict_2017 <- sf::st_read("D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Data/Conflicts/clim_conflict_ips_overlays_2017.geojson")
conflict_2017$year =2017

conflict_2018 <- sf::st_read("D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Data/Conflicts/clim_conflict_ips_overlays_2018.geojson")
conflict_2018$year =2018

conflict_2019 <- sf::st_read("D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Data/Conflicts/clim_conflict_ips_overlays_2019.geojson")
conflict_2019$year =2019
conflict_2020 <- sf::st_read("D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Data/Conflicts/clim_conflict_ips_overlays_2020.geojson")
conflict_2020$year =2020
conflict_2021 <- sf::st_read("D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Data/Conflicts/clim_conflict_ips_overlays_2021.geojson")
conflict_2021$year =2021
conflict_2022 <- sf::st_read("D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Data/Conflicts/clim_conflict_ips_overlays_2022.geojson")
conflict_2022$year =2022

# Merge all layers into a single object
conflict <- bind_rows(conflict_2017, conflict_2018, conflict_2019, conflict_2020, conflict_2021, conflict_2022)
#check the results
names(conflict_2017)
names(conflict)

unique(conflict$intersect_conf_clim)





all_cluster <- function(conf){
  county_conf <- st_intersection(conf, merged_shapefile)
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

conflict_all <- all_cluster(conflict)

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

conflict__no_limited <- conf_no_limited(conflict)
#unique(conflict__no_limited$intersect_conf_clim)
no_limited_label <- c("High conflict + High drought stress", "High conflict + Moderate-High drought stress", "High conflict + Moderate-Low drought stress",
                      "High conflict + Low drought stress", "Moderate conflict + High drought stress", "Moderate conflict + Moderate-High drought stress",
                      "Moderate conflict + Moderate-Low drought stress", "Moderate conflict + Low drought stress" )
unique(conflict__no_limited$clust)


#plotting CONFLICT FIP
#=================================================================


map <- tm_shape(fip_merg)+
  tm_fill(col="FIP", title="Food Insecure Population (%)", style = "cont", palette = mako(10, direction = -1), legend.show = T)+
  tm_facets("Year")+
  tm_shape(conflict__no_limited) +tm_facets("year")+
  tm_fill(col= "clust", palette="-YlOrRd", title="Confli,ct-Climate Intersection", alpha=0.7,
          legend.show = T, labels=no_limited_label)+tm_facets("year")+
  tm_shape(adm2)+
  tm_borders(col="grey", lwd=1)+
  tm_text("NAME_1", size = 0.5, remove.overlap = TRUE, col ='black', fontface = 2)+
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
  tm_compass(type = "8star", size=3, position = c(.92, .13)) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 4, position = c(.929, 0.08))+
  tm_layout(legend.outside = T, 
            legend.only = F,
            legend.show = F,
            legend.text.size = 1.5,
            legend.text.color = "black",
            legend.title.size = 1,
            legend.title.color = "black",
            legend.title.fontface = 2,
            legend.frame = F,
            asp = 1.3,
            legend.position = c("left", "top"), 
            legend.width = 0.5,
            inner.margins = c(0, 0.05, 0, 0.05)
  )


map




map <- tm_shape(fip_merg) +
  tm_fill(col = "FIP", title = "Food Insecure Population (%)", style = "cont", 
          palette = mako(10, direction = -1), legend.show = TRUE) +
  tm_facets("Year") +  # Facet by year to create separate maps for each year
  tm_shape(conflict__no_limited) +
  tm_fill(col = "clust", palette = "-YlOrRd", title = "Conflict-Climate Intersection", 
          alpha = 0.7, legend.show = TRUE, labels = no_limited_label) +
  tm_facets("year") +  # Ensure conflict data is also faceted by year
  tm_shape(adm2) +
  tm_borders(col = "grey", lwd = 1) +
  tm_text("NAME_1", size = 0.5, remove.overlap = TRUE, col = 'black', fontface = 2) +
  tm_shape(Mali_adm0) +
  tm_borders(col = "red", lwd = 2, lty = "dotdash") +
  tm_shape(Niger_adm0) +
  tm_borders(lty = "twodash", col = "blue", lwd = 2) +
  tm_shape(BF_adm0) +
  tm_borders(lty = "dashed", col = "black", lwd = 2) +
  tm_add_legend(type = "fill", col = NA, border.col = NA, lwd = 8, title = " ") + 
  tm_add_legend(type = "line", lty = "twodash", col = "black", lwd = 1.5, title = "Burkina Faso border") +
  tm_add_legend(type = "line", lty = "dotdash", col = "red", lwd = 1.5, title = "Mali border") +
  tm_add_legend(type = "line", lty = "dashed", col = "blue", lwd = 1.5, title = "Niger border") +
  tm_compass(type = "8star", size = 3, position = c(.92, .13)) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 4, position = c(.929, 0.08)) +
  tm_layout(
    legend.outside = TRUE, 
    legend.only = FALSE,
    legend.show = TRUE,
    legend.text.size = 1.5,
    legend.text.color = "black",
    legend.title.size = 1,
    legend.title.color = "black",
    legend.title.fontface = 2,
    legend.frame = FALSE,
    asp = 1.3,
    legend.position = c("left", "top"), 
    legend.width = 0.5,
    inner.margins = c(0, 0.05, 0, 0.05)
  )

map



fpath <- 'D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Products/'
tmap_save(map, dpi= 300,  width=30, height =25, units="in",
          filename=paste0(fpath, "FIP2.png"))

