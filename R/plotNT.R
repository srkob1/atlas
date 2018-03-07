# plot subset - Northern Territory

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

ntSPDF <- subset(sa2Small, STE_NAME16=="Northern Territory")


#sa area names
Y <- ntSPDF %>% split(.@data$SA4_NAME16) %>%
  map_df(., nrow) %>%
  gather(., key = "number", value = "val") %>%
  filter(val>0) %>% select(number) %>% as.vector()


myPalette <- colorRampPalette(rev(brewer.pal(11, "Greens")))
sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(1, 2000000))


distanceList <- list()
# geogrid hex map simulations
for (i in seq(7:150)){
  
  seed <- (4018 + i)
  
  ap_ntSPDF <-
    assign_polygons(
      ntSPDF,
      calculate_grid(
        shape = ntSPDF,
        learning_rate = 0.001,
        grid_type = "hexagonal",
        seed = seed
      )
    )
  # make dataframe
  ap_ntSPDF@data$id = rownames(ap_ntSPDF@data)
  ap_ntSPDF.points = fortify(ap_ntSPDF, region = "id")
  ap_ntSPDF.df = merge(ap_ntSPDF.points, ap_ntSPDF@data, by = "id")
  
  
  ap_ntSPDF.df <- ap_ntSPDF.df %>% mutate(label = paste(gsub(" ", "\n", gsub(" - ", " ", SA2_NAME16))))
  
  
  ap_ntSPDF.df <- ap_ntSPDF.df %>% rowwise %>% 
    mutate(distance = distVincentyEllipsoid(c(V1,V2),c(CENTROIX,
                                                       CENTROIY),
                                            a=6378249.145, b=6356514.86955, f=1/293.465))
  
  distanceList[[i]] <- ap_ntSPDF.df %>% select(id, SA2_NAME16,
                                               CENTROIX, CENTROIY,
                                               V1,V2, distance)
  
  plot <- ggplot(ap_ntSPDF.df) +
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
      size = 3,
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
  ggsave(paste0("Northern Territory", seed, ".png", sep=""), plot, bg = "transparent")
  
}
 