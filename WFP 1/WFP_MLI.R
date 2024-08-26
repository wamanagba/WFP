

rm(list=ls(all=TRUE))
library(haven)
library(geodata)
library(sf)
library(tmap) #tmaptools::palette_explorer()
library("viridis")
library(readxl)
library(tidyr)

path <- '/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Mali/'
#Percentage of food insecure population 2023
fip <- read.csv(paste0(path,'Cadre Harmonise_RBD_VAM_Region Estimation_November2023CAR IPCSept 2023_20231208.csv'),check.names = F, header = T)

fip$NAME_2 <- toupper(fip$NAME_2)
fip$NAME_3 <- toupper(fip$NAME_3)
df1 <- read_sav(paste0(path,'ALL_2018_2023_Data_Fusion_PDM_Resilience.sav'))
head(df1, n=3)
temp <- df1[df1$ADMIN0Name=='MALI', ]
head(temp, n=3)
temp <- aggregate(FCS~YEAR+ADMIN2Name, data=temp,mean, na.rm=T)
write.csv(temp,paste0(path,'FCS_DataFusion.csv'))

#get mali shapefile
Mali_adm2 <- sf::st_as_sf(geodata::gadm(country = 'MALI',level = 2, path = path))
#Mali_adm2 <- sf::st_read("D:/OneDrive - CGIAR/SA_Team/Data/Admin/MLI/MLI.shp")
Mali_adm2 <- Mali_adm2[c('NAME_1','NAME_2')]
Mali_adm2
plot(Mali_adm2['NAME_2'])

#rename admin 2 to remove accents
Mali_adm2$NAME_2 <- stringi::stri_trans_general(str = Mali_adm2$NAME_2, id = "Latin-ASCII")
#Mali_adm2$NAME_3 <- stringi::stri_trans_general(str = Mali_adm2$NAME_3, id = "Latin-ASCII")
#calculate mean for 2018-2023
df <-  read.csv(paste0(path,'FCS_DataFusion.csv'),header=T, sep=',')
names(df)[3] <- "NAME_2"
df$NAME_2 <- toupper(df$NAME_2)
i <- df$NAME_2
df$NAME_2[i=="TOMBOUTOU" | i=="TIMBUKTU" ] <- "TOMBOUCTOU"
df$NAME_2[i=="BADIANGARA"] <- "BANDIAGARA"
df$NAME_2[i=="BARAOUELI"] <- "BAROUELI"
df$NAME_2[i=="GOURMA_RHAROUS"] <- "GOURMA-RHAROUS"
df$NAME_2[i=="NIAFOUNKE"] <- "NIAFUNKE"
df$NAME_2[i=="TIN ESSAKO"] <- "TIN-ESSAKO" 
df$NAME_2[i=="TENENKOUN"] <- "TENENKOU" 


fcs_mean <- aggregate(FCS~NAME_2, data=df, mean, na.rm=T)
fip$FIP <- fip$FIP * 100
#names(fcs_mean)[names(fcs_mean) == "ADMIN2Name"] <- "NAME_2"

#merge df and shapefile
Mali_adm2$NAME_2 <- toupper(Mali_adm2$NAME_2)
#Mali_adm2$NAME_3 <- toupper(Mali_adm2$NAME_3)
#Check naming fidelity
nameCheck <- function(admin, df, level){
  if(level==2){
    a <- sort(unique(admin$NAME_2))
    b <- sort(unique(df$NAME_2))
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
nameCheck(Mali_adm2, fcs_mean, 2)

merged <- merge(Mali_adm2, fcs_mean, by="NAME_2", all=TRUE)
head(merged, n=3)


fip_mean <- fip[,c("NAME_3", "FIP", "Geocode" )]
names(fip_mean)[1] <- "NAME_2" #Doing this in order to ensure a better match after discussion with Carolina


#fip_mean <- aggregate(FIP~NAME_2, data=fip, mean, na.rm=T)

#i <- fip_mean$NAME_3
#fip_mean$NAME_3[i=="TIN-ESSAKO"] <- "TINESSAKO"
#fip_mean$NAME_3[i=="KORO"] <- "KORO-CENTRAL"
#fip_mean$NAME_3[i=="BANAMBA"] <- "BANAMBA-CENTRAL"
nameCheck(Mali_adm2, fip_mean, 2)
fip_merg <- merge(Mali_adm2[,'NAME_2'], fip_mean[,c('FIP', 'NAME_2')], by="NAME_2")


#read conflict data
conflict <- sf::st_read("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/CSO/MLI/clim_conflict_ips_overlays (ACCLED-2017-2022).geojson")
names(conflict)
plot(conflict['FATALITIES'])
unique(conflict$intersect_conf_clim)
reLabel <- function(conf){
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
  tm_shape(Mali_adm2)+
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
  tm_shape(Mali_adm2)+
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
nameCheck(Mali_adm2, ICSMAG, level=2)

#merge icsmag and mali shapefile
merged_icsmag <- merge(Mali_adm2, ICSMAG, by="NAME_2")
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
  tm_shape(Mali_adm2)+
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



