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

waSPDF <- subset(sa2Small, STE_NAME16=="Western Australia")

# minSeed <-4059

#sa area names
Y <- waSPDF %>% split(.@data$SA4_NAME16) %>%
  map_df(., nrow) %>%
  gather(., key = "number", value = "val") %>%
  filter(val>0) %>% select(number) %>% as.vector()


myPalette <- colorRampPalette(rev(brewer.pal(9, "Greens")))
sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(1,2500000))

myDistanceList <- list()
# geogrid hex map simulations
for (i in seq(3)){
  
  seed <- (3018 + i)
  
  ap_waSPDF <-
    assign_polygons(
      waSPDF,
      calculate_grid(
        shape = waSPDF,
        learning_rate = 0.01,
        grid_type = "hexagonal",
        seed = seed
      )
    )
  # make dataframe
  ap_waSPDF@data$id = rownames(ap_waSPDF@data)
  ap_waSPDF.points = fortify(ap_waSPDF, region = "id")
  ap_waSPDF.df = merge(ap_waSPDF.points, ap_waSPDF@data, by = "id")
  
  
  ap_waSPDF.df <- ap_waSPDF.df %>% mutate(label = paste(gsub(" ", "\n", gsub(" - ", " ", SA2_NAME16))))
  
  
  ap_waSPDF.df <- ap_waSPDF.df %>% rowwise %>%
    mutate(distance = distVincentyEllipsoid(c(V1,V2),c(CENTROIX,
                                                       CENTROIY),
                                            a=6378249.145, b=6356514.86955, f=1/293.465))
  
  myDistanceList[[i]] <- ap_waSPDF.df %>% select(id, SA2_NAME16,
                                               CENTROIX,
                                               CENTROIY, V1,V2, distance)
  
  plot <- ggplot(ap_waSPDF.df) +
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
  ggsave(paste0("Western Australia", seed, ".png", sep=""), plot, bg = "transparent")
  
}

save(waDistanceList, file = "waDistanceList")