# plot subset - Australian Capital Territory

library(sf)
library(rgdal)
library(rmapshaper)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(geogrid)
library(RColorBrewer)
library(geosphere)

load("~/atlas/data/sa2_data.Rda")
load("~/atlas/data/sa2_map.Rda")
load("~/atlas/data/sa2Small.Rda")

actSPDF <- subset(sa2Small, STE_NAME16=="Australian Capital Territory")


#sa area names
Y <- actSPDF %>% split(.@data$SA4_NAME16) %>%
  map_df(., nrow) %>%
  gather(., key = "number", value = "val") %>%
  filter(val>0) %>% select(number) %>% as.vector()


myPalette <- colorRampPalette(rev(brewer.pal(9, "Greens")))
sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(1, 70000))

# minseed<- 4053

distanceList <- list()
# geogrid hex map simulations
for (i in seq(69:100)){
  
  seed <- (4018 + i)
  
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
  
  distanceList[[i]] <- ap_actSPDF.df %>% select(id, SA2_NAME16,
                                               CENTROIX, CENTROIY,
                                               V1,V2, distance)
  seed 
  
}




#Weighted populations
actDisList <- map(actDistanceList, left_join, SA2population)
#Weighted populations
for (i in 1:length(actDisList)) {
  actDisList[[i]] <- actDisList[[i]] %>% 
  mutate(wDist = distance*(population/max(population))) %>% 
  arrange(desc(population))
}






# function to find the sum of the distance between the original centroids and the hex centroids allocated
wSum <- function(dat){
  sum<-NA
  if("wDist" %in% colnames(dat)){
    dat %>% distinct(.keep_all = T) %>% select(wDist) %>% sum -> sum
  }
  return(sum)
}



wSumList <- lapply(actDisList, wSum)

minwDist <- wSumList %>% unlist %>% data.frame(sim = as.vector((4019:(length(.)+4018))),wDist = . ) %>% 
  arrange(wDist)
