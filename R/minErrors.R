library(plyr)
library(tidyverse)
library(readr)
library(sf)
library(rgdal)
library(rmapshaper)
library(ggthemes)
library(ggrepel)
#library(hexmapr)
library(geogrid)


 # function to find the sum of the distance between the original centroids and the hex centroids allocated
resSum <- function(dat){
  sum<-NA
  if("distance" %in% colnames(dat)){
    dat %>% distinct(.keep_all = T) %>% select(distance) %>% sum -> sum
  }
  return(sum)
}

wSum <- function(dat){
  sum<-NA
  if("wDist" %in% colnames(dat)){
    dat %>% distinct(.keep_all = T) %>% select(wDist) %>% sum -> sum
  }
  return(sum)
}

res <- list()

res <- lapply(actDistanceList, resSum)


# the simulation with the smallest sum of the residual distances for melbourne inner
min15 <- res %>% unlist %>% data.frame(sim = as.vector((4019:(length(.)+4018))),dist = . ) %>% 
  arrange(dist)

seed <- (min15$sim[[1]])

ap_actSPDF <-
  assign_polygons(
    actSPDF,
    calculate_grid(
      shape = actSPDF,
      learning_rate = 0.001,
      grid_type = "hexagonal",
      seed = seed
    )
  )
# make dataframe
ap_actSPDF@data$id = rownames(ap_actSPDF@data)
ap_actSPDF.points = fortify(ap_actSPDF, region = "id")
ap_actSPDF.df = merge(ap_actSPDF.points, ap_actSPDF@data, by = "id")


ap_actSPDF.df <- ap_actSPDF.df %>% mutate(label = paste(gsub(" ", "\n", gsub(" - ", " ", SA2_NAME16))))


ap_actSPDF.df <- ap_actSPDF.df %>% dplyr::rowwise() %>% 
  mutate(distance = distVincentyEllipsoid(c(V1,
                                            V2), 
                                          c(CENTROIX,
                                            CENTROIY),
                                          a=6378249.145, b=6356514.86955, f=1/293.465))



ggplot(ap_actSPDF.df) +
  geom_polygon(aes(x = long, y = lat, group = group, fill=distance)) +
  geom_text(aes(V1, V2, label = label), size = 3, color = "black") +
  scale_fill_distiller() +
  coord_equal() +
  theme_void()  +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )




#export plots
for (p in seq(plots)) {
  #for (j in 1:15){
  plots[[p]]
  ggsave(paste0(Y, min15$sim[p], ".png"), plots[[p]], bg = "transparent")
 # }
}

