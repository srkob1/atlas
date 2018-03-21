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

#sydSPDF <- subset(sa2Small, STE_NAME16=="New South Wales")

#nonsSPDF <- subset(sa2Small, STE_NAME16=="New South Wales" &!(GCC_NAME16=="Greater Sydney"))

# Break NSW into multiple Cartograms - Sydney, ACT,

sydSPDF <- subset(sa2Small, GCC_NAME16=="Greater Sydney")


#sa area names

#nswSPDF <- subset(sa2Small, STE_NAME16=="New South Wales")

newSPDF <- subset(sa2Small, SA4_NAME16=="Newcastle and Lake Macquarie")


#sa area names
Y <- newSPDF %>% split(.@data$SA4_NAME16) %>%
  map_df(., nrow) %>%
  gather(., key = "number", value = "val") %>%
  filter(val>0) %>% select(number) %>% as.vector()


myPalette <- colorRampPalette(rev(brewer.pal(9, "Greens")))
sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(1, 35000))

distanceList <- list()


sydSPDF <- subset(sa2Small, SA4_NAME16 %in% southeast)


# geogrid hex map simulations
for (i in 1:150){
  
  seed <- (4018 + i)
  
  ap_sydSPDF <-
    assign_polygons(
      sydSPDF,
      calculate_grid(
        shape = sydSPDF,
        learning_rate = 0.01,
        grid_type = "hexagonal",
        seed = seed
      )
    )
  # make dataframe
  
  ap_sydSPDF@data$id = rownames(ap_sydSPDF@data)
  ap_sydSPDF.points = fortify(ap_sydSPDF, region = "id")
  ap_sydSPDF.df = merge(ap_sydSPDF.points, ap_sydSPDF@data, by = "id")
  
  
  ap_sydSPDF.df <- ap_sydSPDF.df %>% mutate(label = paste(gsub(" ", "\n", gsub(" - ", " ", SA2_NAME16))))
  
  
  ap_sydSPDF.df <- ap_sydSPDF.df %>% rowwise %>% 
    mutate(distance = distVincentyEllipsoid(c(CENTROIX,
                                              CENTROIY),
                                            c(V1,V2),
                                            a=6378249.145, b=6356514.86955, f=1/293.465),
           wDist = distance*(population/max(population)))
  
  
  distanceList[[i]] <- ap_sydSPDF.df %>% select(id, SA2_NAME16,
                                               CENTROIX,
                                               CENTROIY, V1,V2, distance) %>% distinct(.keep_all = T)
  

  plot <- ggplot(ap_sydSPDF.df) +
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
  plot
  ggsave(paste0("GSydney", seed, ".png", sep=""), plot, bg = "transparent")
  
  save(distanceList, file = "gsydDistanceList.Rda")
}




#Not in sydney
nonsSPDF$SA4_NAME16 %>% 
  table %>% 
  as.data.frame() %>% 
  filter(Freq>0) %>% 
  arrange(desc(Freq)) -> nonsTable

nonsTable$. %>% as.vector() ->Y


swdistanceList <- list()


for (j in 1:25){
  
  seed <- (4018 + j)
  
  #nonsSPDF %>% subset(nonsSPDF@data$SA4_NAME16 == Y[i]) -> ySPDF
  
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

  swdistanceList[[1]] <- ap_ySPDF.df %>% select(id, SA2_NAME16,
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
#}
  #nswDistanceList[[i]] <- distanceList
}


 ## plot Newcastle



#Weighted populations
gsydDisList <- map(distanceList, left_join, sa2_data %>% 
                     filter(GCC_NAME16=="Greater Sydney"), by = c("SA2_NAME16"))

#Weighted populations
for (i in 1:length(gsydDisList)) {
  gsydDisList[[i]] <- gsydDisList[[i]] %>% 
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



wSumList <- lapply(gsydDisList, wSum)

minwDist <- wSumList %>% unlist %>% data.frame(sim = as.vector((4019:(length(.)+4018))),wDist = . ) %>% 
  arrange(wDist)


intersect(min15 %>% top_n(15) %>% select(sim), minwDist %>% top_n(15) %>% select(sim))




# areas to hex map
sa2_data %>% filter(GCC_NAME16=="Rest of NSW" & !(SA4_NAME16=="Newcastle and Lake Macquarie")) %>% group_by(SA4_NAME16) %>% summarise(count=plyr::count(SA4_NAME16))

# group areas
southeast <- c("Capital Region", "Illawarra", "Southern Highlands and Shoalhaven")
southwest <- c("Riverina", "Murray")
west <- c("Central West", "Far West and Orana")
north <- c("New England and North West", "Hunter Valley exc Newcastle")
northeast <- c("Richmond - Tweed", "Mid North Coast", "Coffs Harbour - Grafton")




