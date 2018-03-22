# plot subset

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

#newcSPDF <- subset(sa2Small, STE_NAME16=="New South Wales")
# Break NSW into multiple Cartograms - Sydney, ACT,


#sa area names

#nswSPDF <- subset(sa2Small, STE_NAME16=="New South Wales")

newcSPDF <- subset(sa2Small, SA4_NAME16=="Newcastle and Lake Macquarie")


#sa area names
Y <- newcSPDF %>% split(.@data$SA4_NAME16) %>%
  map_df(., nrow) %>%
  gather(., key = "number", value = "val") %>%
  filter(val>0) %>% select(number) %>% as.vector()


myPalette <- colorRampPalette(rev(brewer.pal(9, "Greens")))
sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(1, 35000))

distanceList <- list()
# geogrid hex map simulations
for (i in 1:150){
  
  seed <- (4018 + i)
  
  ap_newcSPDF <-
    assign_polygons(
      newcSPDF,
      calculate_grid(
        shape = newcSPDF,
        learning_rate = 0.01,
        grid_type = "hexagonal",
        seed = seed
      )
    )
  # make dataframe
  
  ap_newcSPDF@data$id = rownames(ap_newcSPDF@data)
  ap_newcSPDF.points = fortify(ap_newcSPDF, region = "id")
  ap_newcSPDF.df = merge(ap_newcSPDF.points, ap_newcSPDF@data, by = "id")
  
  
  ap_newcSPDF.df <- ap_newcSPDF.df %>% mutate(label = paste(gsub(" ", "\n", gsub(" - ", " ", SA2_NAME16))))
  
  
  ap_newcSPDF.df <- ap_newcSPDF.df %>% rowwise %>% 
    mutate(distance = distVincentyEllipsoid(c(CENTROIX,
                                              CENTROIY),
                                            c(V1,V2),
                                            a=6378249.145, b=6356514.86955, f=1/293.465))
  maxPopulation <- max(ap_newcSPDF.df$population)
  
  distanceList[[i]] <- ap_newcSPDF.df %>% rowwise %>% 
    mutate(wDist = distance*(population/maxPopulation))
             
  plot <- ggplot(ap_newcSPDF.df) +
    geom_polygon(aes(
      x = long,
      y = lat,
      group = group,
      fill = wDist
    )) +
    geom_text(
      aes(
        V1,
        V2,
        label = label),
      size = 2,
      color = "black"
    ) +
    sc +
    coord_equal() +
    #guides(fill = FALSE) +
    theme_void()  +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA)
    )
  #plot
  #ggsave(paste0("GSydney", seed, ".png", sep=""), plot, bg = "transparent")
  
  # save(distanceList, file = "gsydDistanceList.Rda")
}




 #Not in sydney
nonsSPDF$SA4_NAME16 %>% 
  table %>% 
  as.data.frame() %>% 
  filter(Freq>0) %>% 
  arrange(desc(Freq)) -> nonsTable

nonsTable$. %>% as.vector() ->Y


distanceList <- list()


for (i in 6:13){
  

for (j in seq(100)){
  
  seed <- (4018 + j)
  
  ap_ySPDF <-
    assign_polygons(
      ySPDF,
      calculate_grid(
        shape = ySPDF,
        learning_rate = 0.01,
        grid_type = "hexagonal",
        seed = seed
      )
    )
  # make dataframe
  ap_ySPDF@data$id = rownames(ap_ySPDF@data)
  ap_ySPDF.points = fortify(ap_ySPDF, region = "id")
  ap_ySPDF.df = merge(ap_ySPDF.points, ap_ySPDF@data, by = "id")
  
  
  ap_ySPDF.df <- ap_ySPDF.df %>% mutate(label = paste(gsub(" ", "\n", gsub(" - ", " ", SA2_NAME16))))
  
  
  ap_ySPDF.df <- ap_ySPDF.df %>% rowwise %>% 
    mutate(distance = distVincentyEllipsoid(c(CENTROIX,
                                              CENTROIY),
                                            c(V1,V2),
                                            a=6378249.145, b=6356514.86955, f=1/293.465))

  distanceList[[1]] <- ap_ySPDF.df %>% select(id, SA2_NAME16,
                                             CENTROIX,
                                             CENTROIY, V1,V2, distance)
  
  # plot <- ggplot(ap_ySPDF.df) +
  #   geom_polygon(aes(
  #     x = long,
  #     y = lat,
  #     group = group,
  #     fill = distance
  #   )) +
  #   geom_text(
  #     aes(
  #       V1,
  #       V2,
  #       label = label),
  #     size = 3,
  #     color = "black"
  #   ) +
  #   sc +
  #   coord_equal() +
  #   #guides(fill = FALSE) +
  #   theme_void()  +
  #   theme(
  #     panel.grid.major = element_blank(),
  #     panel.grid.minor = element_blank(),
  #     panel.background = element_rect(fill = "transparent", colour = NA),
  #     plot.background = element_rect(fill = "transparent", colour = NA)
  #   )
  # plot
  # ggsave(paste0(Y[i], seed, ".png", sep=""), plot, bg = "transparent")
  
  cat(paste0(Y[i], seed, ".png", "\n", sep=""))
}
  nswDistanceList[[i]] <- distanceList
}


 ## plot Newcastle

resSum <- function(dat){
  sum<-NA
  if("distance" %in% colnames(dat)){
    dat %>% distinct(.keep_all = T) %>% select(distance) %>% sum -> sum
  }
  return(sum)
}


res <- lapply(newcDisList, resSum)

minDist <- res %>% unlist %>% data.frame(sim = as.vector((4019:(length(.)+4018))),dist = . ) %>% 
  arrange(dist)


# #Weighted populations
# distanceList <- map(distanceList, right_join, sa2_data %>% 
#                      filter(SA4_NAME16=="Newcastle and Lake Macquarie"),
#                    by = c("SA2_NAME16"))

#Weighted populations
newList<-list()
for (i in 1:length(distanceList)) {
  distanceList[[i]] <- distanceList[[i]] %>%rowwise %>%
    mutate(wDist = (population/maxPopulation)) %>% 
    arrange(desc(population))
}

addWdist <- function(dat){
  maxPopulation <- dat %>% select(population) %>% max()
 dat %>% rowwise %>%
    mutate(wDist = (population/maxPopulation)) %>% 
    arrange(desc(population))
} 

  distanceList <- map(addWdist(.))


# function to find the sum of the distance between the original centroids and the hex centroids allocated
wSum <- function(dat){
  sum<-NA
  if("wDist" %in% colnames(dat)){
    dat %>% distinct(.keep_all = T) %>% select(wDist) %>% sum -> sum
  }
  return(sum)
}


wSumList <- lapply(DisList, wSum)

minwDist <- wSumList %>% unlist %>% data.frame(sim = as.vector((4019:(length(.)+4018))),wDist = . ) %>% 
  arrange(wDist)

left_join(minwDist, minDist)
