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


load("~/atlas/data/sa2_data.Rda")
load("~/atlas/data/sa2_map.Rda")
load("~/atlas/data/sa2Small.Rda")

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
  
  
  ap_tasSPDF.df <- ap_tasSPDF.df %>% mutate(label = paste(gsub(" ", "\n", SA2_NAME16)))
  
  plot <- ggplot(ap_tasSPDF.df) +
    geom_polygon(aes(
      x = long,
      y = lat,
      group = group,
      fill = SA4_NAME16
    )) +
    geom_text(
      aes(
        V1,
        V2,
        label = label),
        size = 2,
        color = "black"
      ) +
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
      
      ggsave(paste0("Tasmania", seed, ".png", sep=""), plot, bg = "transparent")
      
}
 