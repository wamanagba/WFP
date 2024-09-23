

rm(list=ls(all=TRUE))
library(haven)
library(geodata)
library(sf)
library(tmap) #tmaptools::palette_explorer()
library("viridis")
library(readxl)
library(tidyr)

path <- '/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Burkina_Faso/'
#Percentage of food insecure population 2023
fip <- read.csv(paste0(path,'caadreHarmonise.csv'),check.names = F, header = T,sep = ";")
fip <- fip[-(1:3), ]
colnames(fip)[1:3] <- c("NAME_0", "NAME_1", "NAME_2")
fip$NAME_1 <- toupper(fip$NAME_1)
fip$NAME_2 <- toupper(fip$NAME_2)

fip$FIP[fip$FIP == "0,00"] <- 0

df1 <- read_sav(paste0(path,'ALL_2018_2023_Data_Fusion_PDM_Resilience.sav'))
head(df1, n=3)
temp <- df1[df1$ADMIN0Name=='BURKINA FASO', ]
temp$ADMIN1Name <- toupper(temp$ADMIN1Name)
temp$ADMIN1Name[temp$ADMIN1Name=="CENTRE_NORD" | temp$ADMIN1Name=="CENTRNORD" ] <- "CENTRE-NORD"
temp$ADMIN1Name[temp$ADMIN1Name=="PLATEAU CENTRAL"] <- "PLATEAU-CENTRAL"

head(temp, n=3)
temp <- aggregate(FCS~YEAR+ADMIN1Name, data=temp,mean, na.rm=T)
write.csv(temp,paste0(path,'FCS_DataFusion.csv'))

#get BF shapefile
BF_adm2 <- sf::st_as_sf(geodata::gadm(country = 'BURKINA FASO',level = 2, path = path))
BF_adm2 <- st_read("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/Migrations_Works/Data/gadm41_BFA_shp/gadm41_BFA_1.shp")
#BF_adm2 <- sf::st_read("D:/OneDrive - CGIAR/SA_Team/Data/Admin/MLI/MLI.shp")
BF_adm2 <- BF_adm2[c('NAME_1')]
BF_adm2
plot(BF_adm2['NAME_1'])

#rename admin 2 to remove accents
BF_adm2$NAME_1 <- stringi::stri_trans_general(str = BF_adm2$NAME_1, id = "Latin-ASCII")
#BF_adm2$NAME_3 <- stringi::stri_trans_general(str = BF_adm2$NAME_3, id = "Latin-ASCII")
#calculate mean for 2018-2023
df <-  read.csv(paste0(path,'FCS_DataFusion.csv'),header=T, sep=',')
names(df)[3] <- "NAME_1"
df$NAME_1 <- toupper(df$NAME_1)
i <- df$NAME_1
df$NAME_1[i=="CENTRE_NORD" | i=="CENTRNORD" ] <- "CENTRE-NORD"
df$NAME_1[i=="PLATEAU CENTRAL"] <- "PLATEAU-CENTRAL"

fcs_mean <- aggregate(FCS~NAME_1, data=df, mean, na.rm=T)
fip$FIP <- as.numeric(fip$FIP) * 100
#names(fcs_mean)[names(fcs_mean) == "ADMIN2Name"] <- "NAME_2"

#merge df and shapefile
BF_adm2$NAME_1 <- toupper(BF_adm2$NAME_1)
#BF_adm2$NAME_3 <- toupper(BF_adm2$NAME_3)
#Check naming fidelity
nameCheck <- function(admin, df, level){
  if(level==2){
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

nameCheck(BF_adm2, fcs_mean, 2)

merged <- merge(BF_adm2, fcs_mean, by="NAME_1", all=TRUE)
merged_BF=merged
head(merged, n=3)


fip_mean <- fip[,c("NAME_1", "FIP", "Geocode" )]
names(fip_mean)[1] <- "NAME_1" #Doing this in order to ensure a better match after discussion with Carolina


#fip_mean <- aggregate(FIP~NAME_1, data=fip, mean, na.rm=T)

i <- fip_mean$NAME_1
fip_mean$NAME_1[i=="PLATEAU CENTRAL"] <- "PLATEAU-CENTRAL"
fip_mean$NAME_1[i=="HAUTS-BASSINS"] <- "HAUT-BASSINS"
#fip_mean$NAME_3[i=="BANAMBA"] <- "BANAMBA-CENTRAL"
nameCheck(BF_adm2, fip_mean, 2)
fip_merg <- merge(BF_adm2[,'NAME_1'], fip_mean[,c('FIP', 'NAME_1')], by="NAME_1")


#read conflict data
conflict <- sf::st_read("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Burkina_Faso/BFA_clim_conflict_ips_overlays.geojson")
names(conflict)
plot(conflict['FATALITIES'])
unique(conflict$intersect_conf_clim)



county_conf <- conflict[conflict$conflict_clust_label==c("High conflict","Moderate conflict")  ,]
county_conf <- st_intersection(county_conf, merged)
unique(county_conf$conflict_clust_label)

conf_no_limited <- function(conf){
  county_conf <- conf[conf$conflict_clust_label != "Limited conflict"  ,]
  #county_conf <- st_intersection(county_conf, merged)
  i <- county_conf$intersect_conf_clim
  
  county_conf$intersect_conf_clim[i=="High conflict-[Low levels of precipitation/High levels of heat stress]"] <-
    "High conflict + High climate impact" 
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate levels of precipitation/Moderate levels of waterlogging]"] <-
    "High conflict + Moderate climate impact"
  county_conf$intersect_conf_clim[i=="High conflict-[High levels of precipitation/High levels of waterlogging]" ] <-
    "High conflict + Low climate impact"
  
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate levels of precipitation/Moderate levels of waterlogging]"] <-
    "Moderate conflict + Moderate climate impact" 
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High levels of precipitation/High levels of waterlogging]"] <-
    "Moderate conflict + Low climate impact" 
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Low levels of precipitation/High levels of heat stress]"   ] <-
    "Moderate conflict + High climate impact"
  
  
  
  
  i <- county_conf$intersect_conf_clim
  county_conf$clust[i=="Moderate conflict + Low climate impact"] <- 6
  county_conf$clust[i=="Moderate conflict + Moderate climate impact"] <- 5
  county_conf$clust[i=="Moderate conflict + High climate impact"] <- 4
  
  
  county_conf$clust[i=="High conflict + Low climate impact"] <- 3
  county_conf$clust[i=="High conflict + Moderate climate impact"] <- 2
  county_conf$clust[i=="High conflict + High climate impact"] <- 1
  county_conf$clust <- as.factor(county_conf$clust)
  return(county_conf)
}

conflict__no_limited <- conf_no_limited(conflict)
unique(conflict__no_limited$intersect_conf_clim)

#plotting CONFLICT FIP
#=================================================================
tmap_mode("plot")

fip_merg_BF=fip_merg
conflict__no_limited_BF= conflict__no_limited
BF_adm2=BF_adm2




path <- '/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Niger/'
#Percentage of food insecure population 2023
fip <- read.csv(paste0(path,'cadreHarmonise_Niger_Proj.csv'),check.names = F, header = T,sep = ";")
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
#Niger_adm2 <- sf::st_as_sf(geodata::gadm(country = 'NIGER',level = 2, path = path))
Niger_adm2 <- st_read("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/Migrations_Works/Data/gadm41_NER_shp/gadm41_NER_1.shp")
Niger_adm2 <- Niger_adm2[c('NAME_1')]
Niger_adm2
plot(Niger_adm2['NAME_1'])

#rename admin 2 to remove accents
Niger_adm2$NAME_1 <- stringi::stri_trans_general(str = Niger_adm2$NAME_1, id = "Latin-ASCII")
#Niger_adm2$NAME_3 <- stringi::stri_trans_general(str = Niger_adm2$NAME_3, id = "Latin-ASCII")
#calculate mean for 2018-2023
df <-  read.csv(paste0(path,'FCS_DataFusion.csv'),header=T, sep=',')
names(df)[3] <- "NAME_1"
df$NAME_1 <- toupper(df$NAME_1)
df$NAME_1[df$NAME_1=="TILLABERI"] <- "TILLABERY"


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
merged_NER=merged
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







county_conf <- conflict[conflict$conflict_clust_label==c("High conflict","Moderate conflict")  ,]
county_conf <- st_intersection(county_conf, merged)

unique(county_conf$conflict_clust_label)
conf_no_limited <- function(conf){
  county_conf <- conf[conf$conflict_clust_label != "Limited conflict"  ,]
  #county_conf <- st_intersection(county_conf, merged)
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
  
  i <- county_conf$intersect_conf_clim
  county_conf$clust[i=="Moderate conflict + Low climate impact"] <- 6
  county_conf$clust[i=="Moderate conflict + Moderate climate impact"] <- 5
  county_conf$clust[i=="Moderate conflict + High climate impact"] <- 4
  county_conf$clust[i=="High conflict + Low climate impact"] <- 3
  county_conf$clust[i=="High conflict + Moderate climate impact"] <- 2
  county_conf$clust[i=="High conflict + High climate impact"] <- 1
  county_conf$clust <- as.factor(county_conf$clust)
  return(county_conf)
}

conflict__no_limited <- conf_no_limited(conflict)
unique(conflict__no_limited$intersect_conf_clim)



fip_merg_NER=fip_merg
conflict__no_limited_NER= conflict__no_limited
Niger_adm2= Niger_adm2




path <- '/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Mali/'
#Percentage of food insecure population 2023
fip <- read.csv(paste0(path,'Cadre Harmonise_RBD_VAM_Region Estimation_November2023CAR IPCSept 2023_20231208.csv'),check.names = F, header = T)

fip$NAME_1 <- toupper(fip$NAME_2)
#fip$NAME_2 <- toupper(fip$NAME_2)
df1 <- read_sav(paste0(path,'ALL_2018_2023_Data_Fusion_PDM_Resilience.sav'))
head(df1, n=3)
temp <- df1[df1$ADMIN0Name=='MALI', ]
head(temp, n=3)
temp <- aggregate(FCS~YEAR+ADMIN1Name, data=temp,mean, na.rm=T)
write.csv(temp,paste0(path,'FCS_DataFusion.csv'))

#get mali shapefile
Mali_adm2 <- sf::st_as_sf(geodata::gadm(country = 'MALI',level = 2, path = path))
Mali_adm2 <- st_read("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/Migrations_Works/Data/gadm41_MLI_shp/gadm41_MLI_1.shp")
Mali_adm2 <- Mali_adm2[c('NAME_1')]
Mali_adm2$NAME_1[Mali_adm2$NAME_1=="TOMBOUTOU" | Mali_adm2$NAME_1=="Timbuktu" ] <- "TOMBOUCTOU"
Mali_adm2
plot(Mali_adm2['NAME_1'])

#rename admin 2 to remove accents
Mali_adm2$NAME_1 <- stringi::stri_trans_general(str = Mali_adm2$NAME_1, id = "Latin-ASCII")
#Mali_adm2$NAME_3 <- stringi::stri_trans_general(str = Mali_adm2$NAME_3, id = "Latin-ASCII")
#calculate mean for 2018-2023
df <-  read.csv(paste0(path,'FCS_DataFusion.csv'),header=T, sep=',')
names(df)[3] <- "NAME_1"
df$NAME_1 <- toupper(df$NAME_1)
i <- df$NAME_1

df$NAME_1[i=="TOMBOUTOU" | i=="TIMBUKTU" ] <- "TOMBOUCTOU"
df$NAME_1[i=="MENAKA"] <- "GAO"
df$NAME_1[i=="BARAOUELI"] <- "BAROUELI"
df$NAME_1[i=="GOURMA_RHAROUS"] <- "GOURMA-RHAROUS"
df$NAME_1[i=="NIAFOUNKE"] <- "NIAFUNKE"
df$NAME_1[i=="TIN ESSAKO"] <- "TIN-ESSAKO" 
df$NAME_1[i=="TENENKOUN"] <- "TENENKOU" 


fcs_mean <- aggregate(FCS~NAME_1, data=df, mean, na.rm=T)
fip$FIP <- fip$FIP * 100
#names(fcs_mean)[names(fcs_mean) == "ADMIN2Name"] <- "NAME_1"

#merge df and shapefile
Mali_adm2$NAME_1 <- toupper(Mali_adm2$NAME_1)
#Mali_adm2$NAME_3 <- toupper(Mali_adm2$NAME_3)
#Check naming fidelity
nameCheck <- function(admin, df, level){
  if(level==2){
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
nameCheck(Mali_adm2, fcs_mean, 2)

merged <- merge(Mali_adm2, fcs_mean, by="NAME_1", all=TRUE)
merged_MLI=merged
head(merged, n=3)


fip_mean <- fip[,c("NAME_1", "FIP", "Geocode" )]
names(fip_mean)[1] <- "NAME_1" #Doing this in order to ensure a better match after discussion with Carolina


#fip_mean <- aggregate(FIP~NAME_1, data=fip, mean, na.rm=T)

#i <- fip_mean$NAME_3
#fip_mean$NAME_3[i=="TIN-ESSAKO"] <- "TINESSAKO"
#fip_mean$NAME_3[i=="KORO"] <- "KORO-CENTRAL"
#fip_mean$NAME_3[i=="BANAMBA"] <- "BANAMBA-CENTRAL"
nameCheck(Mali_adm2, fip_mean, 2)
fip_merg <- merge(Mali_adm2[,'NAME_1'], fip_mean[,c('FIP', 'NAME_1')], by="NAME_1")


#read conflict data
conflict <- sf::st_read("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/CSO/MLI/clim_conflict_ips_overlays (ACCLED-2017-2022).geojson")
names(conflict)
plot(conflict['FATALITIES'])
unique(conflict$intersect_conf_clim)

county_conf <- conflict[conflict$conflict_clust_label==c("High conflict","Moderate conflict")  ,]
county_conf <- st_intersection(county_conf, merged)

unique(county_conf$conflict_clust_label)
conf_no_limited <- function(conf){
  county_conf <- conf[conf$conflict_clust_label != "Limited conflict"  ,]
  #county_conf <- st_intersection(county_conf, merged)
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
  
  i <- county_conf$intersect_conf_clim
  county_conf$clust[i=="Moderate conflict + Low climate impact"] <- 6
  county_conf$clust[i=="Moderate conflict + Moderate climate impact"] <- 5
  county_conf$clust[i=="Moderate conflict + High climate impact"] <- 4
  
  county_conf$clust[i=="High conflict + Low climate impact"] <- 3
  county_conf$clust[i=="High conflict + Moderate climate impact"] <- 2
  county_conf$clust[i=="High conflict + High climate impact"] <- 1
  county_conf$clust <- as.factor(county_conf$clust)
  return(county_conf)
}

conflict__no_limited <- conf_no_limited(conflict)
unique(conflict__no_limited$intersect_conf_clim)








fip_merg_MLI=fip_merg
conflict__no_limited_MLI= conflict__no_limited

#=================================================================
#fip_merg_BF=fip_merg
#conflict__no_limited_BF= conflict__no_limited
#BF_adm2=BF_adm2

#fip_merg_NER=fip_merg
#conflict__no_limited_NER= conflict__no_limited
#Niger_adm2= Niger_adm2

Mali_adm2
#fip_merg_MLI=fip_merg
#conflict__no_limited_MLI= conflict__no_limited





fip_mergm <- rbind(fip_merg_BF, fip_merg_NER)
colnames(fip_merg_MLI)[1]= "NAME_1"
fip_merg2 = rbind(fip_mergm, fip_merg_MLI)

no_limited_label <- c("High conflict + High climate impact", "High conflict + Moderate climate impact",
                      "High conflict + Low climate impact", "Moderate conflict + High climate impact", "Moderate conflict + Moderate climate impact",
                      "Moderate conflict + Low climate impact" )

# Réduire le dataframe pour contenir uniquement la colonne 'clust'
conflict__no_limited_BF1 <- conflict__no_limited_BF["clust"]
conflict__no_limited_NER1 <- conflict__no_limited_NER["clust"]
conflict__no_limited_MLI1 <- conflict__no_limited_MLI["clust"]
conflict__no_limited1 <- rbind(conflict__no_limited_BF1, conflict__no_limited_NER1)
conflict__no_limited2 <- rbind(conflict__no_limited1, conflict__no_limited_MLI1)


adm2 = rbind(Niger_adm2,BF_adm2)
adm2 = rbind(adm2,Mali_adm2)
plot(adm2)


BFA_adm0=BF_adm0=st_read("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/Migrations_Works/Data/gadm41_BFA_shp/gadm41_BFA_0.shp")
Ner_adm0=Niger_adm0= st_read("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/Migrations_Works/Data/gadm41_NER_shp/gadm41_NER_0.shp")
Mali_adm0 <- st_read("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/Migrations_Works/Data/gadm41_MLI_shp/gadm41_MLI_0.shp")
Boundaries = rbind(BF_adm0,Niger_adm0)
Boundaries = rbind(Boundaries,Mali_adm0)
plot(Boundaries)


tmap_mode("plot")
map <- tm_shape(fip_merg2)+
  tm_fill(col="FIP", title="Food Insecure Population (%)",style = "cont", palette = mako(10,direction	=-1),legend.show = T)+
  tm_shape(conflict__no_limited2) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Conflict-Climate Intersection",alpha=0.7,
          legend.show = T, labels=no_limited_label)+
  tm_shape(adm2)+
  tm_borders(col="grey",lwd=1)+
  tm_text("NAME_1", size = 1.5, remove.overlap = TRUE, col ='black', fontface = 2)+
  tm_shape(Boundaries) +  # Ajout du shapefile Boundaries pour les frontières supplémentaires
  tm_borders(col="blue", lwd=3, lty="dashed") + # Frontières des Boundaries en gris
  #tm_text("COUNTRY", size = 4, remove.overlap = TRUE, col ='black', fontface = 2)+
  tm_compass(type = "8star", size=4,position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 1.5, 
               position = c("right", "bottom"))+
  tm_layout(legend.outside=F, 
            legend.text.size = 1.5,
            legend.text.color = "black",
            legend.title.size= 1.7,
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

fpath <- '/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Burkina_Faso/'
tmap_save(map, dpi= 500,  width=25, height =20, units="in",
          filename=paste0(fpath, "FIP001.png"))


map <- tm_shape(fip_merg2)+
  tm_fill(col="FIP", title="Food Insecure Population (%)", style = "cont", palette = mako(10, direction = -1), legend.show = T)+
  tm_shape(conflict__no_limited2) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Conflict-Climate Intersection", alpha=0.7,
          legend.show = T, labels=no_limited_label)+
  tm_shape(adm2)+
  tm_borders(col="black", lwd=.4)+
  tm_text("NAME_1", size = 1.5, remove.overlap = TRUE, col ='black', fontface = 2)+
  #tm_shape(Boundaries) +
  #tm_borders(col="blue", lwd=2, lty="dashed") +
  tm_shape(BF_adm0) +
  tm_borders(lty = "dashed", col = "black", lwd = 3) +
  tm_shape(Mali_adm0) +
  tm_borders(col = "red", lwd = 3, lty = "dotdash") +  # Changed to a thicker, solid line for prominence
  tm_shape(Niger_adm0) +
  tm_borders(lty = "twodash", col = "blue", lwd = 3) +
  tm_add_legend(type = "fill", col = NA, border.col = NA,lwd = 8, title = " ") + # Add title for borders legend
  tm_add_legend(type = "line", lty="twodash",col = "black", lwd = 1.5, title = "Burkina Faso border") +
  tm_add_legend(type = "line", lty = "dotdash", col = "red", lwd = 1.5, title = "Mali border") +  # Updated legend
  tm_add_legend(type = "line", lty = "dashed", col = "blue", lwd = 1.5, title = "Niger border") +
  tm_compass(type = "8star", size=4, position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 1.5, position = c("right", "bottom"))+
  tm_layout(legend.outside = F, 
            legend.text.size = 1.5,
            legend.text.color = "black",
            legend.title.size = 2,
            legend.title.color = "black",
            legend.title.fontface = 2,
            legend.frame = F,
            asp = 1.3,
            legend.position = c("left", "top"), 
            legend.width = 0.5,
            inner.margins = c(0, 0.05, 0, 0.05)
  )

map



fpath <- '/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Burkina_Faso/'
tmap_save(map, dpi= 500,  width=25, height =20, units="in",
          filename=paste0(fpath, "FIP001.png"))

conflict__no_limited3 <- conflict__no_limited2[conflict__no_limited2$clust != 6 ,]
conflict__no_limited3 <- conflict__no_limited2[conflict__no_limited2$clust != 3 ,]

no_limited_label3 <- c("High conflict + High climate impact", "High conflict + Moderate climate impact",
                       "Moderate conflict + High climate impact", "Moderate conflict + Moderate climate impact")



merged = rbind(merged_BF,merged_MLI)
merged = rbind(merged,merged_NER)
merged <- subset(merged, NAME_1 != "")
unique(merged$NAME_1)
#plotting FCS
#=================================================================





#######################################################################################

library(sf)
library(writexl)

#### ICA Areas extraction
path = "/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/CIAT/Data_CIAT/"
Niger_adm0 <- st_read("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/CIAT/Data_CIAT/CollectingTable_shapefile/ner_ica_collectingTableAlbert_20220707.shp")
Niger_adm0$geometry =Niger_adm0$geometry
Niger_df <- as.data.frame(Niger_adm0)
write_xlsx(Niger_df, "/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/CIAT/Data_CIAT/Niger.xlsx")

BF_adm0 <- st_read("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/CIAT/Data_CIAT/Shapefile/collectingtable/bfa_wfp_ica_food_insec_people_20180710.shp")
BF_df <- as.data.frame(BF_adm0)
write_xlsx(BF_df, "/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/CIAT/Data_CIAT/BF.xlsx")

#===========================================================
### Country Shape file

#get mali shapefile
Mali_adm2 <- sf::st_as_sf(geodata::gadm(country = 'MALI',level = 2, path = path))
#Mali_adm2 <- sf::st_read("D:/OneDrive - CGIAR/SA_Team/Data/Admin/MLI/MLI.shp")
Mali_adm2 <- Mali_adm2[c('NAME_1','NAME_2')]
plot(Mali_adm2['NAME_2'])
#rename admin 2 to remove accents
Mali_adm2$NAME_2 <- stringi::stri_trans_general(str = Mali_adm2$NAME_2, id = "Latin-ASCII")
Mali_adm2$NAME_1 <- stringi::stri_trans_general(str = Mali_adm2$NAME_1, id = "Latin-ASCII")

#get Niger shapefile
Niger_adm2 <- sf::st_as_sf(geodata::gadm(country = 'NIGER',level = 2, path = path))
Niger_adm2 <- Niger_adm2[c('NAME_1','NAME_2')]
plot(Niger_adm2['NAME_2'])
#rename admin 2 to remove accents
Niger_adm2$NAME_2 <- stringi::stri_trans_general(str = Niger_adm2$NAME_2, id = "Latin-ASCII")
Niger_adm2$NAME_1 <- stringi::stri_trans_general(str = Niger_adm2$NAME_1, id = "Latin-ASCII")
#get BF shapefile
Burkina_adm2 <- sf::st_as_sf(geodata::gadm(country = 'BURKINA FASO',level = 1, path = path))
Burkina_adm2 <- Burkina_adm2[c('NAME_1')]
names(Burkina_adm2)[names(Burkina_adm2) == "NAME_1"] <- "NAME_2"
plot(Burkina_adm2['NAME_2'])
#rename admin 2 to remove accents
Burkina_adm2$NAME_2 <- stringi::stri_trans_general(str = Burkina_adm2$NAME_2, id = "Latin-ASCII")


#ICA process
#=================================================================
ICSMAG_MLI <- read_excel(paste0('/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Mali/',"mli_ica+Nut_CollectingTable_20230720.xlsx"))
ICSMAG_MLI <- ICSMAG_MLI[c("adm2_name","adm1_name","ICAMAG")]
ICSMAG_MLI <- ICSMAG_MLI %>% drop_na()
names(ICSMAG_MLI)[names(ICSMAG_MLI) == "adm2_name"] <- "NAME_2"
names(ICSMAG_MLI)[names(ICSMAG_MLI) == "adm1_name"] <- "NAME_1"
ICSMAG_MLI$NAME_2 <- toupper(ICSMAG_MLI$NAME_2)
Mali_adm2$NAME_1 <- toupper(Mali_adm2$NAME_1)
Mali_adm2$NAME_2 <- toupper(Mali_adm2$NAME_2)
Mali_adm2$NAME_1[Mali_adm2$NAME_1=="TIMBUKTU" ] <- "TOMBOUCTOU"
nameCheck(Mali_adm2, ICSMAG_MLI, level=2)

#merge ICSMAG_MLI and mali shapefile
#merged_ICSMAG_MLI <- merge(Mali_adm2, ICSMAG_MLI, by = "NAME_2", all = TRUE)
merged_ICSMAG_MLI <- merge(Mali_adm2, ICSMAG_MLI, by="NAME_2")
merged_ICSMAG_MLI <- merged_ICSMAG_MLI[c("NAME_2","ICAMAG")]
unique(merged_ICSMAG_MLI$ICAMAG)
plot(merged_ICSMAG_MLI['ICAMAG'])


#ICSMAG_NGR <- read_excel(paste0("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/CIAT/Data_CIAT/Niger.xlsx"))
#ICSMAG_NGR <- ICSMAG_NGR[c("adm2_name","adm1_name","ICA_Areas")]
ICSMAG_NGR <- Niger_adm0[c("adm2_name","ICA_Areas")]
names(ICSMAG_NGR)[names(ICSMAG_NGR) == "ICA_Areas"] <- "ICAMAG"
ICSMAG_NGR <- ICSMAG_NGR %>% drop_na()
names(ICSMAG_NGR)[names(ICSMAG_NGR) == "adm2_name"] <- "NAME_2"
#names(ICSMAG_NGR)[names(ICSMAG_NGR) == "adm1_name"] <- "NAME_1"

#Niger_adm2$NAME_1[Niger_adm2$NAME_1=="Tillaberi" ] <- "Tillabery"
#Niger_adm2$NAME_2[Niger_adm2$NAME_2=="Tillaberi" ] <- "Tillabery"
#ICSMAG_NGR$NAME_2[ICSMAG_NGR$NAME_2=="Tillaberi" ] <- "Tillabery"
#ICSMAG_NGR$NAME_1[ICSMAG_NGR$NAME_1=="Tillaberi" ] <- "Tillabery"

ICSMAG_NGR$NAME_2 <- toupper(ICSMAG_NGR$NAME_2)
#Niger_adm2$NAME_2 <- toupper(Niger_adm2$NAME_2)


#nameCheck(Niger_adm2, ICSMAG_NGR, level=2)

#merge ICSMAG_NGR and mali shapefile
#merged_ICSMAG_NGR <- merge(Niger_adm2, ICSMAG_NGR, by="NAME_2")
merged_ICSMAG_NGR <- ICSMAG_NGR
unique(merged_ICSMAG_NGR$ICAMAG)
plot(merged_ICSMAG_NGR['ICAMAG'])

### BF


ICSMAG_BF <- BF_adm0[c("adm1_name","ICA_Area")]
names(ICSMAG_BF)[names(ICSMAG_BF) == "ICA_Area"] <- "ICAMAG"
ICSMAG_BF <- ICSMAG_BF %>% drop_na()
names(ICSMAG_BF)[names(ICSMAG_BF) == "adm1_name"] <- "NAME_2"
ICSMAG_BF$NAME_2 <- toupper(ICSMAG_BF$NAME_2)
merged_ICSMAG_BF <- ICSMAG_BF
unique(merged_ICSMAG_BF$ICAMAG)
plot(merged_ICSMAG_BF['ICAMAG'])



#######################################################################################
# Transformer le CRS de merged_ICSMAG_NGR pour correspondre à celui de merged_ICSMAG_BF
merged_ICSMAG_NGR <- st_transform(merged_ICSMAG_NGR, st_crs(merged_ICSMAG_BF))

merged_icsmag = rbind(merged_ICSMAG_BF,merged_ICSMAG_NGR)
merged_icsmag = rbind(merged_icsmag,merged_ICSMAG_MLI)
plot(merged_icsmag)
#plotting ICAMAG



#maps for ICAMAG
library(RColorBrewer)
icsmag_map <- tm_shape(merged_icsmag)+
  tm_fill(col = "ICAMAG", 
          title = "ICAMAG Level1 for Burkina Faso and Level 2 for Mali and Niger",
          style = "cat", 
          palette = brewer.pal(9, "Greens"), # Utiliser la palette "Greens" avec 9 nuances
          legend.show = TRUE)+
#tm_fill(col="ICAMAG", title="ICAMAG",style = "cat", palette = tmaptools::get_brewer_pal("Set3", n = 8),legend.show = T)+
  #tm_fill(col="ICAMAG", title="ICAMAG",style = "cat", palette = '-Greens',alpha=0.5, legend.show = T)+
  tm_shape(conflict__no_limited2) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Conflict-Climate Intersection",alpha=0.7,
          legend.show = T, labels=no_limited_label)+
  tm_shape(adm2)+
  tm_borders(col="black",lwd=01)+
  tm_text("NAME_1", size = 1, remove.overlap = TRUE, col ='gray9')+
  tm_shape(BFA_adm0) +
  tm_borders(lty = "dashed", col = "black", lwd = 3) +
  tm_shape(Mali_adm0) +
  tm_borders(col = "red", lwd = 3, lty = "dotdash") +  # Changed to a thicker, solid line for prominence
  tm_shape(Ner_adm0) +
  tm_borders(lty = "twodash", col = "blue", lwd = 3) +
  tm_add_legend(type = "fill", col = NA, border.col = NA,lwd = 8, title = " ") + # Add title for borders legend
  tm_add_legend(type = "line", lty="twodash",col = "black", lwd = 1.5, title = "Burkina Faso border") +
  tm_add_legend(type = "line", lty = "dotdash", col = "red", lwd = 1.5, title = "Mali border") +  # Updated legend
  tm_add_legend(type = "line", lty = "dashed", col = "blue", lwd = 1.5, title = "Niger border") +
  tm_compass(type = "8star", size=4,position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 1.5, 
               position = c("right", "bottom"))+
  tm_layout(legend.outside=F, 
            legend.text.size = 1.5,
            legend.text.color = "black",
            legend.title.size= 2,
            legend.title.color = "black",
            legend.title.fontface = 2,
            legend.frame=F,
            asp = 1.4,
            legend.position = c("left", "top"), 
            legend.width = 1,
            #bg.color = 'grey85',
            inner.margins = c(0,0.05,0,0.05)
  )

icsmag_map
tmap_save(icsmag_map,  dpi= 300,  height=20, width=25, units="in",
          filename=paste0(fpath, "ICAMAG.png"))



library(RColorBrewer)
icsmag_map <- tm_shape(merged_icsmag) +
  tm_fill(col = "ICAMAG", 
          title = "ICAMAG Level1 for Burkina Faso and Level 2 for Mali and Niger",
          style = "cat", 
          palette = brewer.pal(9, "Greens"), # Utiliser la palette "Greens" avec 9 nuances
          legend.show = TRUE) +
  tm_shape(conflict__no_limited2) +
  tm_fill(col = "clust", palette = "-YlOrRd", title = "Conflict-Climate Intersection", alpha = 0.7,
          legend.show = TRUE, labels = no_limited_label) +
  tm_shape(adm2) +
  tm_borders(col = "black", lwd = 1) +
  tm_text("NAME_1", size = 0.9, remove.overlap = TRUE, col = 'gray70') +
  tm_shape(BFA_adm0) +
  tm_borders(lty = "dashed", col = "black", lwd = 3) +
  tm_shape(Mali_adm0) +
  tm_borders(col = "red", lwd = 3, lty = "dotdash") +
  tm_shape(Ner_adm0) +
  tm_borders(lty = "twodash", col = "blue", lwd = 3) +
  tm_add_legend(type = "line", lty = "blank", col = "transparent", title = "Borders") + # Ajoute un titre pour les frontières
  tm_add_legend(type = "line", lty = "dashed", col = "black", lwd = 1.5, title = "Burkina Faso border") +
  tm_add_legend(type = "line", lty = "dotdash", col = "red", lwd = 1.5, title = "Mali border") +
  tm_add_legend(type = "line", lty = "twodash", col = "blue", lwd = 1.5, title = "Niger border") +
  tm_compass(type = "8star", size = 4, position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 50, 100), text.size = 1.5, 
               position = c("right", "bottom")) +
  tm_layout(legend.outside = FALSE, 
            legend.text.size = 1,
            legend.text.color = "black",
            legend.title.size = 1.1,
            legend.title.color = "black",
            legend.title.fontface = 2,
            legend.frame = TRUE,
            asp = 1.4,
            legend.position = c("left", "top"), 
            legend.width = 0.6,
            inner.margins = c(0, 0.05, 0, 0.05)
  )

# Afficher la carte
print(icsmag_map)


