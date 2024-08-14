

map_conflict_climate <- tm_shape(conflict__no_limited) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Conflict-Climate Intersection", legend.show = TRUE) +
  tm_borders(col="black", lwd=0.01) +
  tm_layout(frame = FALSE, 
            title = "Map of Conflict-Climate Intersection", 
            title.position = c("center", "top"), 
            fontfamily = "sans",
            legend.text.size = 1.1,
            legend.text.color = "black",
            legend.title.size = 1.2,
            legend.title.color = "black",
            legend.title.fontface = 2,
            legend.position = c("left", "bottom"),
            legend.width = 0.5)

# Affichage de la carte
print(map_conflict_climate)


library(tmap)  # Assurez-vous que la bibliothèque tmap est chargée

# Création de la carte avec des bordures plus visibles
map_conflict_climate <- tm_shape(conflict__no_limited) +
  tm_fill(col= "clust", palette="-YlOrRd", title="Conflict-Climate Intersection", legend.show = TRUE) +
  tm_shape(Mali_adm2) +  # Ajout des bordures administratives
  tm_borders(col="black", lwd=0.7) +  # Choisissez une couleur et une épaisseur qui se démarquent
  tm_layout(frame = FALSE, 
            title = "Map of Conflict-Climate Intersection with Administrative Boundaries", 
            title.position = c("center", "top"), 
            fontfamily = "sans",
            legend.text.size = 1.1,
            legend.text.color = "black",
            legend.title.size = 1.2,
            legend.title.color = "black",
            legend.title.fontface = 2,
            legend.position = c("left", "bottom"),
            legend.width = 0.5,
            inner.margins = c(0.05, 0.05, 0.05, 0.05))

# Affichage de la carte
print(map_conflict_climate)


tmap_mode("plot")
map <- tm_shape(fip_merg)+
  #tm_fill(col="FIP", title="Food Insecure Population (%)",style = "cont", palette = mako(10,direction	=-1),legend.show = T)+
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
plot(conflict__no_limited)
