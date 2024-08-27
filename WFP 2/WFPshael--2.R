


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

#plot(merged_shapefile['NAME_1'])


merged_shapefile <- merged_shapefile[c('NAME_1')]



#rename admin 2 to remove accents
merged_shapefile$NAME_1 <- stringi::stri_trans_general(str = merged_shapefile$NAME_1, id = "Latin-ASCII")


fip$FIP <- fip$FIP * 100
fip$FIP<- round(fip$FIP,2)

#merged_shapefile$NAME_2 <- toupper(merged_shapefile$NAME_2)
merged_shapefile$NAME_1 <- toupper(merged_shapefile$NAME_1)

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
  filter(Year %in% c("2017"))

plot(fip_merg['FIP'])

#read conflict data
conflict <- sf::st_read("D:/OneDrive - CGIAR/1-Scripts/WFP/WFP 2/Data/Conflicts/clim_conflict_ips_overlays_2017.geojson")
names(conflict)
plot(conflict['FATALITIES'])
unique(conflict$intersect_conf_clim)
reLabel <- function(conf){
  conf=conflict
  temp <- conf[conf$conflict_clust_label=="High conflict"  ,]
  temp
  temp <- st_intersection(temp, merged)
  unique(temp$intersect_conf_clim)
  i <- temp$intersect_conf_clim
  temp$intersect_conf_clim[i=="High conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "High conflict + Low drought stress"
  temp$intersect_conf_clim[i=="High conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "High conflict + Moderate-High drought stress"
  i <- temp$intersect_conf_clim
  
  temp$clust[i=="High conflict + Low drought stress"] <- 2
  temp$clust[i=="High conflict + Moderate-High drought stress"] <- 1
  temp$clust <- as.factor(temp$clust)
  return(temp)
}
conflict_mali <- reLabel(conflict)
plot(conflict_mali['intersect_conf_clim'])
unique(conflict_mali$intersect_conf_clim)
unique(conflict$conflict_clust_label)
all_cluster <- function(conf){
  county_conf <- st_intersection(conf, merged)
  i <- county_conf$intersect_conf_clim
  county_conf$intersect_conf_clim[i=="High conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "High conflict + Low drought stress" 
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "High conflict + Moderate-High drought stress"
  
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "Moderate conflict + Low drought stress" 
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High-Moderate levels of precipitation/Moderate-Low levels of drought stress]"] <-
    "Moderate conflict + Moderate-Low drought stress"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "Moderate conflict + Moderate-High drought stress"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Low levels of precipitation/High levels of drought stress]"] <-
    "Moderate conflict + High drought stress"
  
  county_conf$intersect_conf_clim[i=="Limited conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "Limited conflict + Low drought stress" 
  county_conf$intersect_conf_clim[i=="Limited conflict-[High-Moderate levels of precipitation/Moderate-Low levels of drought stress]"] <-
    "Limited conflict + Moderate-Low drought stress"
  county_conf$intersect_conf_clim[i=="Limited conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "Limited conflict + Moderate-High drought stress"
  county_conf$intersect_conf_clim[i=="Limited conflict-[Low levels of precipitation/High levels of drought stress]"] <-
    "Limited conflict + High drought stress"
  i <- county_conf$intersect_conf_clim
  county_conf$clust[i=="Limited conflict + Low drought stress"] <- 10
  county_conf$clust[i=="Limited conflict + Moderate-Low drought stress"] <- 9
  county_conf$clust[i=="Limited conflict + Moderate-High drought stress"] <- 8
  county_conf$clust[i=="Limited conflict + High drought stress"] <- 7
  
  county_conf$clust[i=="Moderate conflict + Low drought stress"] <- 6
  county_conf$clust[i=="Moderate conflict + Moderate-Low drought stress"] <- 5
  county_conf$clust[i=="Moderate conflict + Moderate-High drought stress"] <- 4
  county_conf$clust[i=="Moderate conflict + High drought stress"] <- 3
  
  county_conf$clust[i=="High conflict + Low drought stress"] <- 2
  county_conf$clust[i=="High conflict + Moderate-High drought stress"] <- 1
  county_conf$clust <- as.factor(county_conf$clust)
  return(county_conf)
}

conflict_all <- all_cluster(conflict)

county_conf <- conflict[conflict$conflict_clust_label==c("High conflict","Moderate conflict")  ,]
county_conf <- st_intersection(county_conf, merged)

unique(county_conf$conflict_clust_label)
conf_no_limited <- function(conf){
  county_conf <- conf[conf$conflict_clust_label != "Limited conflict"  ,]
  #county_conf <- st_intersection(county_conf, merged)
  i <- county_conf$intersect_conf_clim
  county_conf$intersect_conf_clim[i=="High conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "High conflict + Low drought stress" 
  county_conf$intersect_conf_clim[i=="High conflict-[High-Moderate levels of precipitation/Moderate-Low levels of drought stress]"] <-
    "High conflict + Moderate-Low drought stress" 
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "High conflict + Moderate-High drought stress"
  county_conf$intersect_conf_clim[i=="High conflict-[Low levels of precipitation/High levels of drought stress]"] <-
    "High conflict + High drought stress" 
  
  
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High levels of precipitation/Low levels of drought stress]"] <-
    "Moderate conflict + Low drought stress" 
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High-Moderate levels of precipitation/Moderate-Low levels of drought stress]"] <-
    "Moderate conflict + Moderate-Low drought stress"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
    "Moderate conflict + Moderate-High drought stress"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Low levels of precipitation/High levels of drought stress]"] <-
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
unique(conflict__no_limited$intersect_conf_clim)
no_limited_label <- c("High conflict + High drought stress", "High conflict + Moderate-High drought stress", "High conflict + Moderate-Low drought stress",
                      "High conflict + Low drought stress", "Moderate conflict + High drought stress", "Moderate conflict + Moderate-High drought stress",
                      "Moderate conflict + Moderate-Low drought stress", "Moderate conflict + Low drought stress" )
unique(conflict_all$clust)
plot(conflict_all['intersect_conf_clim'])
label <- c("High conflict + Moderate-High drought","High conflict + Low drought", 
           "Moderate conflict + High drought", "Moderate conflict + Moderate-High drought",
           "Moderate conflict + Moderate-Low drought", "Moderate conflict + Low drought",
           "Limited conflict + High drought", "Limited conflict + Moderate-High drought ",
           "Limited conflict + Moderate-Low drought", "Limited conflict + Low drought")
high_conf_label <- c("High conflict + Moderate-High drought stress","High conflict + Low drought stress")

#plotting CONFLICT FIP
#=================================================================
tmap_mode("plot")
map <- tm_shape(fip_merg)+
  tm_fill(col="FIP", title="Food Insecure Population (%)",style = "cont", palette = mako(10,direction	=-1),legend.show = T)+
  tm_shape(conflict__no_limited) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Conflict-Climate Intersection",
          legend.show = T, labels=no_limited_label)+
  tm_shape(merged_shapefile)+
  tm_borders(col="black",lwd=0.01)+
  tm_text("NAME_2", size = 0.6, remove.overlap = TRUE, col ='black')+
  tm_compass(type = "8star", size=4,position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 1.5, 
               position = c("right", "bottom"))+
  tm_layout(legend.outside=F, 
            legend.text.size = 1.1,
            legend.text.color = "black",
            legend.title.size= 1.2,
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
fpath <- '/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Mali/Results/'
tmap_save(map, dpi= 300,  width=11.7, height =8.3, units="in",
          filename=paste0(fpath, "FIP_mali.png"))


#plotting FCS
#=================================================================
tmap_mode("plot")
map <- tm_shape(merged)+
  tm_fill(col="FCS", title="FCS",style = "cont", palette = viridis(100,direction	=-1),legend.show = T)+
  tm_shape(conflict__no_limited) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Conflict-Climate Intersection",
          legend.show = T, labels=no_limited_label)+
  tm_shape(merged_shapefile)+
  tm_borders(col="black",lwd=0.01)+
  tm_text("NAME_1", size = 0.7, remove.overlap = TRUE, col ='white')+
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
tmap_save(map, dpi= 300,  width=11.7, height =8.3, units="in",
          filename=paste0(fpath, "FCS.png"))

#plotting ICAMAG
#=================================================================
ICSMAG <- read_excel(paste0(path,"mli_ica+Nut_CollectingTable_20230720.xlsx"))
ICSMAG <- ICSMAG[c("adm2_name","adm1_name","ICAMAG","ICAMCG")]
ICSMAG <- ICSMAG %>% drop_na()
names(ICSMAG)[names(ICSMAG) == "adm2_name"] <- "NAME_2"
ICSMAG$NAME_2 <- toupper(ICSMAG$NAME_2)
nameCheck(merged_shapefile, ICSMAG, level=2)

#merge icsmag and mali shapefile
merged_icsmag <- merge(merged_shapefile, ICSMAG, by="NAME_2")
unique(merged_icsmag$ICAMAG)
plot(merged_icsmag['ICAMAG'])

# #get icsmag conflict data
# high_conflict <- function(conf){
#   temp <- conf[conf$conflict_clust_label=="High conflict"  ,]
#   temp <- st_intersection(temp, merged_icsmag)
#   i <- temp$intersect_conf_clim
#   temp$intersect_conf_clim[i=="High conflict-[High levels of precipitation/Low levels of drought stress]"] <-
#     "High conflict + Low drought stress"
#   temp$intersect_conf_clim[i=="High conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
#     "High conflict + Moderate-High drought stress"
#   i <- temp$intersect_conf_clim
#   
#   temp$clust[i=="High conflict + Low drought stress"] <- 2
#   temp$clust[i=="High conflict + Moderate-High drought stress"] <- 1
#   temp$clust <- as.factor(temp$clust)
#   return(temp)
# }
# high_conf <- high_conflict(conflict)
# plot(high_conf['FATALITIES'])
# all_conf <- function(conf){
#   county_conf <- conf[conf$conflict_clust_label != "Limited conflict"  ,]
#   county_conf <- st_intersection(county_conf, merged_icsmag)
#   i <- county_conf$intersect_conf_clim
#   county_conf$intersect_conf_clim[i=="High conflict-[High levels of precipitation/Low levels of drought stress]"] <-
#     "High conflict + Low drought stress" 
#   county_conf$intersect_conf_clim[i=="High conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
#     "High conflict + Moderate-High drought stress"
#   
#   county_conf$intersect_conf_clim[i=="Moderate conflict-[High levels of precipitation/Low levels of drought stress]"] <-
#     "Moderate conflict + Low drought stress" 
#   county_conf$intersect_conf_clim[i=="Moderate conflict-[High-Moderate levels of precipitation/Moderate-Low levels of drought stress]"] <-
#     "Moderate conflict + Moderate-Low drought stress"
#   county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate-Low levels of precipitation/Moderate-High levels of drought stress]"] <-
#     "Moderate conflict + Moderate-High drought stress"
#   county_conf$intersect_conf_clim[i=="Moderate conflict-[Low levels of precipitation/High levels of drought stress]"] <-
#     "Moderate conflict + High drought stress"
#   
#   i <- county_conf$intersect_conf_clim
#   
#   county_conf$clust[i=="Moderate conflict + Low drought stress"] <- 6
#   county_conf$clust[i=="Moderate conflict + Moderate-Low drought stress"] <- 5
#   county_conf$clust[i=="Moderate conflict + Moderate-High drought stress"] <- 4
#   county_conf$clust[i=="Moderate conflict + High drought stress"] <- 3
#   
#   county_conf$clust[i=="High conflict + Low drought stress"] <- 2
#   county_conf$clust[i=="High conflict + Moderate-High drought stress"] <- 1
#   county_conf$clust <- as.factor(county_conf$clust)
#   return(county_conf)
# }
# 
# conflict_icsmag <- all_conf(conflict)
# plot(conflict_icsmag)
# unique(conflict_icsmag$clust)

#maps for ICAMAG
icsmag_map <- tm_shape(merged_icsmag)+
  tm_fill(col="ICAMAG", title="ICAMAG",style = "cat", palette = viridis(8,direction	=1, option = "G", alpha=0.5),legend.show = T)+
  #tm_fill(col="ICAMAG", title="ICAMAG",style = "cat", palette = tmaptools::get_brewer_pal("Set3", n = 8),legend.show = T)+
  #tm_fill(col="ICAMAG", title="ICAMAG",style = "cat", palette = '-Greens',alpha=0.5, legend.show = T)+
  tm_shape(conflict__no_limited) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Conflict-Climate Intersection",
          legend.show = T, labels=no_limited_label)+
  tm_shape(merged_shapefile)+
  tm_borders(col="black",lwd=0.01)+
  tm_text("NAME_2", size = 0.7, remove.overlap = TRUE, col ='gray70')+
  tm_compass(type = "8star", size=4,position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 1.5, 
               position = c("right", "bottom"))+
  tm_layout(legend.outside=F, 
            legend.text.size = 1,
            legend.text.color = "black",
            legend.title.size= 1.1,
            legend.title.color = "black",
            legend.title.fontface = 2,
            legend.frame=F,
            asp = 1.4,
            legend.position = c("left", "top"), 
            legend.width = 0.6,
            #bg.color = 'grey85',
            inner.margins = c(0,0.05,0,0.05)
  )

icsmag_map
tmap_save(icsmag_map,  dpi= 300,  height=8.3, width=11.7, units="in",
          filename=paste0(fpath, "ICAMAG.png"))



