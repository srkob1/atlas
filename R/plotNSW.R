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
Y <- sydSPDF %>% split(.@data$SA4_NAME16) %>%
  map_df(., nrow) %>%
  gather(., key = "number", value = "val") %>%
  filter(val>0) %>% select(number) %>% as.vector()


myPalette <- colorRampPalette(rev(brewer.pal(9, "Greens")))
sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(1, 170000))

distanceList <- list()
# geogrid hex map simulations
for (i in 1:60){
  
  seed <- (4040 + i)
  
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
                                            a=6378249.145, b=6356514.86955, f=1/293.465))
  
  distanceList[[i]] <- ap_sydSPDF.df %>% select(id, SA2_NAME16,
                                               CENTROIX,
                                               CENTROIY, V1,V2, distance) %>% distinct(.keep_all = T)
  
  plot <- ggplot(ap_sydSPDF.df) +
    geom_polygon(aes(
      x = long,
      y = lat,
      group = group,
      fill = distance
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


distanceList <- list()


for (i in 6:13){

for (j in seq(25)){
  
  seed <- (4018 + j)
  
  nonsSPDF %>% subset(nonsSPDF@data$SA4_NAME16 == Y[i]) -> ySPDF
  
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


