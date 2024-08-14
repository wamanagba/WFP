
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
head(merged, n=3)


fip_mean <- fip[,c("NAME_1", "FIP", "Geocode" )]
names(fip_mean)[1] <- "NAME_1" #Doing this in order to ensure a better match after discussion with Carolina


#fip_mean <- aggregate(FIP~NAME_1, data=fip, mean, na.rm=T)

i <- fip_mean$NAME_1
fip_mean$NAME_1[i=="PLATEAU CENTRAL"] <- "PLATEAU-CENTRAL"
fip_mean$NAME_1[i=="HAUTS-BASSINS"] <- "HAUTS-BASSINS"
#fip_mean$NAME_3[i=="BANAMBA"] <- "BANAMBA-CENTRAL"
nameCheck(BF_adm2, fip_mean, 2)
fip_merg <- merge(BF_adm2[,'NAME_1'], fip_mean[,c('FIP', 'NAME_1')], by="NAME_1")


#read conflict data
conflict <- sf::st_read("/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Burkina_Faso/BFA_clim_conflict_ips_overlays.geojson")
names(conflict)
plot(conflict['FATALITIES'])
unique(conflict$intersect_conf_clim)
reLabel <- function(conf){
  #conf=conflict
  temp <- conf[conf$conflict_clust_label=="High conflict",]
  temp
  temp <- st_intersection(temp, merged)
  unique(temp$intersect_conf_clim)
  i <- temp$intersect_conf_clim
  temp$intersect_conf_clim[i== "High conflict-[Moderate levels of precipitation/Moderate levels of waterlogging]"] <-
    "High conflict + Moderate climate condition"
  temp$intersect_conf_clim[i=="High conflict-[Low levels of precipitation/High levels of heat stress]"] <-
    "High conflict + High climate condition"
  temp$intersect_conf_clim[i=="High conflict-[High levels of precipitation/High levels of waterlogging]"] <-
    "High conflict + High climate condition"
  i <- temp$intersect_conf_clim
  
  temp$clust[i=="High conflict + Moderate climate condition"] <- 2
  temp$clust[i=="High conflict + High climate condition"] <- 1
  temp$clust <- as.factor(temp$clust)
  return(temp)
}
conflict_BF <- reLabel(conflict)
plot(conflict_BF['intersect_conf_clim'])
unique(conflict_BF$intersect_conf_clim)
unique(conflict$conflict_clust_label)

all_cluster <- function(conf){
  #conf=conflict
  unique(conf$intersect_conf_clim)
  county_conf <- st_intersection(conf, merged)
  i <- county_conf$intersect_conf_clim
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate levels of precipitation/Moderate levels of waterlogging]"] <-
    "Moderate conflict + Moderate climate condition" 
  
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate levels of precipitation/Moderate levels of waterlogging]"] <-
    "High conflict + Moderate climate condition"
  
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High levels of precipitation/High levels of waterlogging]"] <-
    "Moderate conflict + High climate condition" 
  
  county_conf$intersect_conf_clim[i=="High conflict-[High levels of precipitation/High levels of waterlogging]" ] <-
    "High conflict + High climate condition"
  
  county_conf$intersect_conf_clim[i=="Limited conflict-[High levels of precipitation/High levels of waterlogging]"] <-
    "Limited conflict + High climate condition"
  
  county_conf$intersect_conf_clim[i== "Limited conflict-[Moderate levels of precipitation/Moderate levels of waterlogging]"] <-
    "Limited conflict + Moderate climate condition"
  
  county_conf$intersect_conf_clim[i=="High conflict-[Low levels of precipitation/High levels of heat stress]"] <-
    "High conflict + High climate condition" 
  
  county_conf$intersect_conf_clim[i=="Limited conflict-[Low levels of precipitation/High levels of heat stress]"] <-
    "Limited conflict + High climate condition"
  
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Low levels of precipitation/High levels of heat stress]"   ] <-
    "Moderate conflict + High climate condition"
  
  i <- county_conf$intersect_conf_clim
  county_conf$clust[i=="Limited conflict + Moderate climate condition"] <- 6
  #county_conf$clust[i=="Limited conflict + High climate condition"] <- 8
  county_conf$clust[i=="Limited conflict + High climate condition"] <- 5
  #county_conf$clust[i=="Limited conflict + High drought stress"] <- 7
  
  
  county_conf$clust[i=="Moderate conflict + Moderate climate condition"] <- 4
  #county_conf$clust[i=="Moderate conflict + High climate condition"] <- 5
  county_conf$clust[i=="Moderate conflict + High climate condition"] <- 3
  #county_conf$clust[i=="Moderate conflict + High drought stress"] <- 3
  
  county_conf$clust[i=="High conflict + Moderate climate condition"] <- 2
  #county_conf$clust[i=="High conflict + High climate condition"] <- 2
  county_conf$clust[i=="High conflict + High climate condition"] <- 1
  
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

conf_no_limited3 <- function(conf){
  county_conf <- conf[conf$conflict_clust_label != "Limited conflict"  ,]
  #county_conf <- st_intersection(county_conf, merged)
  i <- county_conf$intersect_conf_clim
  
  county_conf$intersect_conf_clim[i=="High conflict-[Low levels of precipitation/High levels of heat stress]"] <-
    "High conflict" 
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate levels of precipitation/Moderate levels of waterlogging]"] <-
    "High conflict"
  county_conf$intersect_conf_clim[i=="High conflict-[High levels of precipitation/High levels of waterlogging]" ] <-
    "High conflict"
  
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate levels of precipitation/Moderate levels of waterlogging]"] <-
    "Moderate conflict" 
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High levels of precipitation/High levels of waterlogging]"] <-
    "Moderate conflict" 
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Low levels of precipitation/High levels of heat stress]"   ] <-
    "Moderate conflict"
  
  
  
  
  i <- county_conf$intersect_conf_clim

  
  county_conf$clust[i=="Moderate conflict"] <- 2
  county_conf$clust[i=="High conflict"] <- 1
  county_conf$clust <- as.factor(county_conf$clust)
  return(county_conf)
}


conf_no_limited2 <- function(conf){
  county_conf <- conf[conf$conflict_clust_label != "Limited conflict"  ,]
  #county_conf <- st_intersection(county_conf, merged)
  i <- county_conf$intersect_conf_clim
  
  county_conf$intersect_conf_clim[i=="High conflict-[Low levels of precipitation/High levels of heat stress]"] <-
    "High climate impact" 
  county_conf$intersect_conf_clim[i=="High conflict-[Moderate levels of precipitation/Moderate levels of waterlogging]"] <-
    "Moderate climate impact"
  county_conf$intersect_conf_clim[i=="High conflict-[High levels of precipitation/High levels of waterlogging]" ] <-
    "Low climate impact"
  
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Moderate levels of precipitation/Moderate levels of waterlogging]"] <-
    "Moderate climate impact" 
  county_conf$intersect_conf_clim[i=="Moderate conflict-[High levels of precipitation/High levels of waterlogging]"] <-
    "Low climate impact" 
  county_conf$intersect_conf_clim[i=="Moderate conflict-[Low levels of precipitation/High levels of heat stress]"   ] <-
    "High climate impact"
  
  
  
  
  i <- county_conf$intersect_conf_clim

  
  
  county_conf$clust[i=="Low climate impact"] <- 3
  county_conf$clust[i=="Moderate climate impact"] <- 2
  county_conf$clust[i=="High climate impact"] <- 1
  county_conf$clust <- as.factor(county_conf$clust)
  return(county_conf)
}

conflict__no_limited2 <- conf_no_limited2(conflict)
unique(conflict__no_limited$intersect_conf_clim)
no_limited_label <- c("High conflict + High climate impact", "High conflict + Moderate climate impact",
                      "High conflict + Low climate impact", "Moderate conflict + High climate impact", "Moderate conflict + Moderate climate impact",
                      "Moderate conflict + Low climate impact" )
no_limited_label2=c("Low climate impact","Moderate climate impact", "High climate impact")

unique(conflict_all$clust)
plot(conflict_all['intersect_conf_clim'])

unique(conflict_all$intersect_conf_clim)
label <- c("High conflict + High climate condition", "High conflict + Moderate climate condition",
           "Moderate conflict + High climate condition", "Moderate conflict + Moderate climate condition",
           "Limited conflict + High climate condition", "Limited conflict + Moderate climate condition")

high_conf_label <- c("High conflict + High climate condition",
                     "High conflict + Moderate climate condition")

#plotting CONFLICT FIP
#=================================================================
tmap_mode("plot")

fip_merg_BF=fip_merg
#conflict__no_limited_BF= conflict__no_limited
BF_adm2=BF_adm2
no_limited_label <- c("High conflict + High climate impact", "High conflict + Moderate climate impact",
                      "High conflict + Low climate impact", "Moderate conflict + High climate impact", "Moderate conflict + Moderate climate impact",
                      "Moderate conflict + Low climate impact" )

no_limited_label <-c("Conflit élevé + Impact climatique élevé", 
  "Conflit élevé + Impact climatique modéré",
  "Conflit élevé + Impact climatique faible", 
  "Conflit modéré + Impact climatique élevé", 
  "Conflit modéré + Impact climatique modéré",
  "Conflit modéré + Impact climatique faible")

conflict__no_limited <- conf_no_limited(conflict)

map <- tm_shape(fip_merg)+
  tm_fill(col="FIP", title="Food Insecure Population (%)",style = "cont", palette = mako(10,direction	=-1),legend.show = T)+
  tm_shape(conflict__no_limited) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Conflict-Climate Intersection",
          legend.show = T, labels=no_limited_label)+
  tm_shape(BF_adm2)+
  tm_borders(col="black",lwd=0.9)+
  tm_text("NAME_1", size = 0.6, remove.overlap = TRUE, col ='black')+
  tm_compass(type = "8star", size=4,position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1.5, 
               position = c("right", "bottom"))+
  tm_layout(legend.outside=F, 
            legend.text.size = .8,
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
fpath <- '/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Burkina_Faso/'
tmap_save(map, dpi= 300,  width=11.7, height =8.3, units="in",
          filename=paste0(fpath, "coflict_climate_fip_BF.png"))

unique(conflict__no_limited$conflict_clust_label)

map <- tm_shape(conflict__no_limited)+
  #tm_fill(col="clust", title="Food Insecure Population (%)",style = "cont", palette = mako(10,direction	=-1),legend.show = T)+
  #tm_shape(conflict__no_limited) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Intersection Conflit-Climat",
          legend.show = T, labels=no_limited_label)+
  tm_shape(BF_adm2)+
  tm_borders(col="black",lwd=0.5)+
  tm_text("NAME_1", size = 0.6, remove.overlap = TRUE, col ='black')+
  tm_compass(type = "8star", size=4,position = c("right", "bottom")) +
  tm_scale_bar(breaks = c(0, 5, 10), text.size = 1.5, 
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
tmap_save(map, dpi= 300,  width=11.7, height =8.3, units="in",
          filename=paste0(fpath, "coflict_climate_BF_fr.png"))

conflict__no_limited3 <- conf_no_limited3(conflict)
no_limited_label=c("High conflict","Moderate conflict" )
map <- tm_shape(conflict__no_limited3)+
  #tm_fill(col="clust", title="Food Insecure Population (%)",style = "cont", palette = mako(10,direction	=-1),legend.show = T)+
  #tm_shape(conflict__no_limited) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Conflict",
          legend.show = T, labels=no_limited_label)+
  tm_shape(BF_adm2)+
  tm_borders(col="black",lwd=0.5)+
  tm_text("NAME_1", size = 0.6, remove.overlap = TRUE, col ='black')+
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





fpath <- '/Users/yacoub/Library/CloudStorage/OneDrive-CGIAR/SA_Team/Data/Burkina_Faso/'
tmap_save(map, dpi= 300,  width=11.7, height =8.3, units="in",
          filename=paste0(fpath, "conflict_BF.png"))


no_limited_label2=      c("High climate impact","Moderate climate impact" ,"Low climate impact"   )
map <- tm_shape(conflict__no_limited2)+
  #tm_fill(col="clust", title="Food Insecure Population (%)",style = "cont", palette = mako(10,direction	=-1),legend.show = T)+
  #tm_shape(conflict__no_limited) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Conflict",
          legend.show = T, labels=no_limited_label2)+
  tm_shape(BF_adm2)+
  tm_borders(col="black",lwd=0.5)+
  tm_text("NAME_1", size = 0.6, remove.overlap = TRUE, col ='black')+
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

tmap_save(map, dpi= 300,  width=11.7, height =8.3, units="in",
          filename=paste0(fpath, "climate_BF.png"))
#plotting FCS
#=================================================================
tmap_mode("plot")
#merged <- na.omit(merged, subset = "NAME_1")
merged <- merged[-1, ]
map <- tm_shape(merged)+
  tm_fill(col="FCS", title="FCS",style = "cont", palette = viridis(100,direction	=-1),legend.show = T)+
  tm_shape(conflict__no_limited) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Conflict-Climate Intersection",
          legend.show = T, labels=no_limited_label)+
  tm_shape(BF_adm2)+
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
