# plot subset
#Hex grid Victoria
library(sf)
library(rgdal)
library(rmapshaper)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(geogrid)


load("data/sa2_data.Rda")
load("data/sa2_map.Rda")
load("data/sa2Small.Rda")

# minSeed <- 4032

tasSPDF <- subset(sa2Small, STE_NAME16=="Tasmania")

# plot geographical map of area
tasPlot <- ggplot(sa2_data %>% filter(.$STE_NAME16=="Tasmania")) +
  geom_polygon(aes(
    x = long,
    y = lat,
    group = group,
    fill = SA4_NAME16, colour= SA2_NAME16
  )) +
  #scale_fill_brewer(alpha=0.4, discrete=TRUE) +
  coord_equal() +
  guides(fill = FALSE) +
  theme_void()  +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )



#tas area names
Y <- tasSPDF %>% split(.@data$SA4_NAME16) %>%
  map_df(., nrow) %>%
  gather(., key = "number", value = "val") %>%
  filter(val>0) %>% select(number) %>% as.vector()



myPalette <- colorRampPalette(rev(brewer.pal(9, "Greens")))
sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(1,
                                                              250000))

#hex map simulations
for (i in seq(50)){
  
  seed <- (4081 + i)

    ap_tasSPDF <-
    assign_polygons(
      tasSPDF,
      calculate_grid(
        shape = tasSPDF,
        learning_rate = 0.01,
        grid_type = "hexagonal",
        seed = seed
      )
    )
  # make dataframe
  ap_tasSPDF@data$id = rownames(ap_tasSPDF@data)
  ap_tasSPDF.points = fortify(ap_tasSPDF, region = "id")
  ap_tasSPDF.df = merge(ap_tasSPDF.points, ap_tasSPDF@data, by = "id")
  
  
  ap_tasSPDF.df <- ap_tasSPDF.df %>% rowwise %>%
    mutate(distance = distVincentyEllipsoid(c(V1,V2),c(CENTROIX,
                                                       CENTROIY),
                                            a=6378249.145, b=6356514.86955, f=1/293.465))
  
  ap_tasSPDF.df <- ap_tasSPDF.df %>% mutate(label = paste(gsub(" ", "\n", SA2_NAME16)))
  
  plot <- ggplot(ap_tasSPDF.df) +
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
        #scale_fill_brewer(alpha=0.4, discrete=TRUE) +
        coord_equal() +
        theme_void()  +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA)
        )
      
      ggsave(paste0("Tasmania", seed, ".png", sep=""), plot, bg = "transparent")
      
}
 


# separate, can we add more points than spatial polygons so it allocates and leaves white space?
# add coordinates as an s4 object
calculate_grid(
  shape = tasSPDF,
  learning_rate = 0.01,
  grid_type = "hexagonal",
  seed = seed
) -> tassieO

SPDF <-
  assign_polygons(
    tasSPDF,tassieO
    )
# make 