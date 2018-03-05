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


load("~/atlas/data/sa2_data.Rda")
load("~/atlas/data/sa2_map.Rda")
load("~/atlas/data/sa2Small.Rda")

saSPDF <- subset(sa2Small, STE_NAME16=="South Australia")


#sa area names
Y <- saSPDF %>% split(.@data$SA4_NAME16) %>%
  map_df(., nrow) %>%
  gather(., key = "number", value = "val") %>%
  filter(val>0) %>% select(number) %>% as.vector()


myPalette <- colorRampPalette(rev(brewer.pal(11, "Greens")))
sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(1, 1700000))

# geogrid hex map simulations
for (i in seq(50)){
  
  seed <- (4018 + i)
  
  ap_saSPDF <-
    assign_polygons(
      saSPDF,
      calculate_grid(
        shape = saSPDF,
        learning_rate = 0.01,
        grid_type = "hexagonal",
        seed = seed
      )
    )
  # make dataframe
  ap_saSPDF@data$id = rownames(ap_saSPDF@data)
  ap_saSPDF.points = fortify(ap_saSPDF, region = "id")
  ap_saSPDF.df = merge(ap_saSPDF.points, ap_saSPDF@data, by = "id")
  
  
  ap_saSPDF.df <- ap_saSPDF.df %>% mutate(label = paste(gsub(" ", "\n", gsub(" - ", "", SA2_NAME16))))
  
  
  ap_saSPDF.df <- ap_saSPDF.df %>% rowwise %>% 
    mutate(distance = distVincentyEllipsoid(c(CENTROIX,
                                              CENTROIY),
                                            c(V1,V2),
                                            a=6378249.145, b=6356514.86955, f=1/293.465))
  
  
  plot <- ggplot(ap_saSPDF.df) +
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
  ggsave(paste0("South Australia", seed, ".png", sep=""), plot, bg = "transparent")
  
}
