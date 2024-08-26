


rm(list=ls(all=TRUE))
library(haven)
library(geodata)
library(sf)
library(tmap) #tmaptools::palette_explorer()
library("viridis")
library(readxl)
library(tidyr)

path <- '/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Niger/'
#Percentage of food insecure population 2023
fip <- read.csv(paste0(path,'cadreHarmonise_Niger.csv'),check.names = F, header = T,sep = ";")
names(fip)[2] <- "NAME_1"; names(fip)[3] <- "NAME_2"
fip$NAME_2 <- toupper(fip$NAME_2)
fip$NAME_2 <- toupper(fip$NAME_2)
df1 <- read_sav(paste0('/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Burkina_Faso/ALL_2018_2023_Data_Fusion_PDM_Resilience.sav'))
head(df1, n=3)
temp <- df1[df1$ADMIN0Name=='NIGER', ]
temp$ADMIN1Name <- toupper(temp$ADMIN1Name)
head(temp, n=3)
temp <- aggregate(FCS~YEAR+ADMIN1Name, data=temp,mean, na.rm=T)
write.csv(temp,paste0(path,'FCS_DataFusion.csv'))

#get Niger shapefile
Niger_adm2 <- sf::st_as_sf(geodata::gadm(country = 'NIGER',level = 2, path = path))
#Niger_adm2 <- sf::st_read("D:/OneDrive - CGIAR/SA_Team/Data/Admin/MLI/MLI.shp")
Niger_adm2 <- Niger_adm2[c('NAME_1','NAME_2')]
Niger_adm2
plot(Niger_adm2['NAME_1'])

#rename admin 2 to remove accents
Niger_adm2$NAME_1 <- stringi::stri_trans_general(str = Niger_adm2$NAME_1, id = "Latin-ASCII")
#Niger_adm2$NAME_3 <- stringi::stri_trans_general(str = Niger_adm2$NAME_3, id = "Latin-ASCII")
#calculate mean for 2018-2023
df <-  read.csv(paste0(path,'FCS_DataFusion.csv'),header=T, sep=',')
names(df)[3] <- "NAME_1"
df$NAME_1 <- toupper(df$NAME_1)
 


fcs_mean <- aggregate(FCS~NAME_1, data=df, mean, na.rm=T)
fip$FIP <- as.numeric(fip$FIP) * 100
#names(fcs_mean)[names(fcs_mean) == "ADMIN2Name"] <- "NAME_2"

#merge df and shapefile
Niger_adm2$NAME_1 <- toupper(Niger_adm2$NAME_1)
#Niger_adm2$NAME_3 <- toupper(Niger_adm2$NAME_3)
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
nameCheck(Niger_adm2, fcs_mean, 2)

merged <- merge(Niger_adm2, fcs_mean, by="NAME_1", all=TRUE)
head(merged, n=3)


fip_mean <- fip[,c("NAME_1", "FIP", "Geocode" )]
names(fip_mean)[1] <- "NAME_1" #Doing this in order to ensure a better match after discussion with Carolina


#fip_mean <- aggregate(FIP~NAME_2, data=fip, mean, na.rm=T)

i <- fip_mean$NAME_1
fip_mean$NAME_1[i=="TILLABERI"] <- "TILLABERY"
#fip_mean$NAME_3[i=="KORO"] <- "KORO-CENTRAL"
#fip_mean$NAME_3[i=="BANAMBA"] <- "BANAMBA-CENTRAL"
nameCheck(Niger_adm2, fip_mean, 2)
fip_merg <- merge(Niger_adm2[,'NAME_1'], fip_mean[,c('FIP', 'NAME_1')], by="NAME_1")


#read conflict data
conflict <- sf::st_read("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Niger/NER_clim_conflict_ips_overlays.geojson")
names(conflict)
plot(conflict['FATALITIES'])
unique(conflict$intersect_conf_clim)
reLabel <- function(conf){
  #conf= conflict
  temp <- conf[conf$conflict_clust_label=="High conflict",]
  unique(temp$intersect_conf_clim)
  temp <- st_intersection(temp, merged)
  unique(temp$intersect_conf_clim)
  i <- temp$intersect_conf_clim
  temp$intersect_conf_clim[i=="High conflict-[High levels of drought stress/Low precipitation]"] <-
    "High conflict + High drought stress"
  temp$intersect_conf_clim[i=="High conflict-[Moderate-High levels of drought stress/Moderate-Low precipitation]"] <-
    "High conflict + Moderate drought stress"
  temp$intersect_conf_clim[i=="High conflict-[Moderate-Low levels of drought stress/High-Moderate precipitation]"] <-
    "High conflict + Moderate drought stress"
  temp$intersect_conf_clim[i=="High conflict-[Low levels of drought stress/High precipitation]" ] <-
    "High conflict + Low drought stress"
  
  i <- temp$intersect_conf_clim
  
  temp$clust[i=="High conflict + Low drought stress"] <- 3
  temp$clust[i=="High conflict + Moderate drought stress"] <- 2
  temp$clust[i=="High conflict + High drought stress"] <- 1
  temp$clust <- as.factor(temp$clust)
  return(temp)
}
conflict_Niger <- reLabel(conflict)
plot(conflict_Niger['intersect_conf_clim'])
unique(conflict_Niger$intersect_conf_clim)
unique(conflict$conflict_clust_label)

all_cluster <- function(conf){
  #conf= conflict
  unique(conf$intersect_conf_clim)
  county_conf <- st_intersection(conf, merged)
  i <- county_conf$intersect_conf_clim
  county_conf$intersect_conf_clim[i=="High conflict-[High levels of drought stress/Low precipitation]"] <-
    "High conflict + High drought stress"
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate-High levels of drought stress/Moderate-Low precipitation]"] <-
    "High conflict + Moderate drought stress"
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate-Low levels of drought stress/High-Moderate precipitation]"] <-
    "High conflict + Moderate drought stress"
  county_conf$intersect_conf_clim[i=="High conflict-[Low levels of drought stress/High precipitation]" ] <-
    "High conflict + Low drought stress"
  
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High levels of drought stress/Low precipitation]"] <-
    "Moderate conflict + High drought stress" 
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate-High levels of drought stress/Moderate-Low precipitation]"] <-
    "Moderate conflict + Moderate drought stress"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate-Low levels of drought stress/High-Moderate precipitation]"] <-
    "Moderate conflict + Moderate drought stress"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Low levels of drought stress/High precipitation]" ] <-
    "Moderate conflict + Low drought stress"
  
  county_conf$intersect_conf_clim[i=="Limited conflict-[High levels of drought stress/Low precipitation]"] <-
    "Limited conflict + High drought stress" 
  county_conf$intersect_conf_clim[i=="Limited conflict-[Moderate-High levels of drought stress/Moderate-Low precipitation]" ] <-
    "Limited conflict + Moderate drought stress"
  county_conf$intersect_conf_clim[i=="Limited conflict-[Moderate-Low levels of drought stress/High-Moderate precipitation]"] <-
    "Limited conflict + Moderate drought stress"
  county_conf$intersect_conf_clim[i=="Limited conflict-[Low levels of drought stress/High precipitation]"] <-
    "Limited conflict + Low drought stress"
  
  i <- county_conf$intersect_conf_clim
  county_conf$clust[i=="Limited conflict + Low drought stress"] <- 9
  county_conf$clust[i=="Limited conflict + Moderate drought stress"] <- 8
  county_conf$clust[i=="Limited conflict + High drought stress"] <- 7
  
  county_conf$clust[i=="Moderate conflict + Low drought stress"] <- 6
  county_conf$clust[i=="Moderate conflict + Moderate drought stress"] <- 5
  county_conf$clust[i=="Moderate conflict + High drought stress"] <- 4
  
  county_conf$clust[i=="High conflict + Low drought stress"] <- 3
  county_conf$clust[i=="High conflict + Moderate drought stress"] <- 2
  county_conf$clust[i=="High conflict + High drought stress"] <- 1
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
  
  county_conf$intersect_conf_clim[i=="High conflict-[High levels of drought stress/Low precipitation]"] <-
    "High conflict + High drought stress"
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate-High levels of drought stress/Moderate-Low precipitation]"] <-
    "High conflict + Moderate drought stress"
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate-Low levels of drought stress/High-Moderate precipitation]"] <-
    "High conflict + Moderate drought stress"
  county_conf$intersect_conf_clim[i=="High conflict-[Low levels of drought stress/High precipitation]" ] <-
    "High conflict + Low drought stress"
  
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High levels of drought stress/Low precipitation]"] <-
    "Moderate conflict + High drought stress" 
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate-High levels of drought stress/Moderate-Low precipitation]"] <-
    "Moderate conflict + Moderate drought stress"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate-Low levels of drought stress/High-Moderate precipitation]"] <-
    "Moderate conflict + Moderate drought stress"
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Low levels of drought stress/High precipitation]" ] <-
    "Moderate conflict + Low drought stress"

  i <- county_conf$intersect_conf_clim
  county_conf$clust[i=="Moderate conflict + Low drought stress"] <- 6
  county_conf$clust[i=="Moderate conflict + Moderate drought stress"] <- 5
  county_conf$clust[i=="Moderate conflict + High drought stress"] <- 4
  county_conf$clust[i=="High conflict + Low drought stress"] <- 3
  county_conf$clust[i=="High conflict + Moderate drought stress"] <- 2
  county_conf$clust[i=="High conflict + High drought stress"] <- 1
  county_conf$clust <- as.factor(county_conf$clust)
  return(county_conf)
}

conflict__no_limited <- conf_no_limited(conflict)
unique(conflict__no_limited$intersect_conf_clim)
no_limited_label <- c("High conflict + High drought stress", "High conflict + Moderate drought stress",
                      "High conflict + Low drought stress", "Moderate conflict + High drought stress", "Moderate conflict + Moderate drought stress",
                      "Moderate conflict + Low drought stress" )
unique(conflict_all$clust)
plot(conflict_all['intersect_conf_clim'])
label <- c("High conflict + High drought stress",
            "High conflict + Moderate drought",
           "High conflict + Low drought", 
           "Moderate conflict + High drought",
           "Moderate conflict + Moderate drought",
           "Moderate conflict + Low drought",
           "Limited conflict + High drought", 
           "Limited conflict + Moderate drought ",
            "Limited conflict + Low drought")
high_conf_label <- c("High conflict + High drought stress","High conflict + Moderate-High drought stress","High conflict + Low drought stress")

#plotting CONFLICT FIP
#=================================================================
tmap_mode("plot")
map <- tm_shape(fip_merg)+
  tm_fill(col="FIP", title="Food Insecure Population (%)",style = "cont", palette = mako(10,direction	=-1),legend.show = T)+
  tm_shape(conflict__no_limited) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Conflict-Climate Intersection",
          legend.show = T, labels=no_limited_label)+
  tm_shape(Niger_adm2)+
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
fpath <- '/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Niger/Results/'
tmap_save(map, dpi= 300,  width=11.7, height =8.3, units="in",
          filename=paste0(fpath, "FIP.png"))


#plotting FCS
plot(merged)
#=================================================================
tmap_mode("plot")
map <- tm_shape(merged)+
  tm_fill(col="FCS", title="FCS",style = "cont", palette = viridis(100,direction	=-1),legend.show = T)+
  tm_shape(conflict__no_limited) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Conflict-Climate Intersection",
          legend.show = T, labels=no_limited_label)+
  tm_shape(Niger_adm2)+
  tm_borders(col="black",lwd=0.01)+
  tm_text("NAME_2", size = 0.7, remove.overlap = TRUE, col ='white')+
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
nameCheck(Niger_adm2, ICSMAG, level=2)

#merge icsmag and Niger shapefile
merged_icsmag <- merge(Niger_adm2, ICSMAG, by="NAME_2")
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
  tm_shape(Niger_adm2)+
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



