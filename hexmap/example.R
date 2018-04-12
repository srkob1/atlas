# This file has the code to actually create the Oz hexmap
# Australia 
library(tidyverse)
library(geosphere)
library(hexbin)

load("data/sa2Small.Rda")
load("data/sa2_map.Rda")

# Find the functions to use
source("hexmap/hexagon.R")

# subset to remove Islands
ausSPDF <- subset(sa2Small, (STE_NAME16!="Other Territories"))
ausSPDF <- subset(ausSPDF, (SA2_NAME16!="Lord Howe Island"))

sa2_map <- sa2_map %>% 
  filter(STE_NAME16!="Other Territories") %>%
  filter(SA2_NAME16!="Lord Howe Island")

# set radius for Australian map
radius <- 0.3

# Arrange original centroid data
centroid_data <- NULL
for (i in 1:length(ausSPDF@polygons)) {
   centroid_data$long[i] <- ausSPDF@polygons[[i]]@labpt[1]
   centroid_data$lat[i] <- ausSPDF@polygons[[i]]@labpt[2]
   centroid_data$SA2_NAME16[i] <- ausSPDF@data$SA2_NAME16[which(ausSPDF@data$id == ausSPDF@polygons[[i]]@ID)] 
   centroid_data$population[i] <- ausSPDF@data$population[which(ausSPDF@data$id == ausSPDF@polygons[[i]]@ID)]
}

# Arrange grid data
grid <- create_grid(ausSPDF@bbox, radius) %>%
  cbind(., id = 1:NROW(.), assigned = rep(FALSE, NROW(.)))

# order centroids for allocation
map_dfc(centroid_data, unlist) %>%
  arrange(desc(population)) %>%
  bind_cols(., assign_hexagons(centroid_data, grid)) -> aus_centroids

# Data needs to be output
# csv
write_csv(aus_centroids, path = "aus_centroids.csv")
# and rda for us
save(aus_centroids, file = "aus_centroids.Rda")



# Plotting code
# Points and hexes: inspired by http://unconj.ca/blog/custom-hexbin-functions-with-ggplot.html
# colour by population

(geomap <- ggplot(sa2_map) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=population, label=SA2_NAME16))
  )

(hexmap <-ggplot(aus_centroids) +
  geom_polygon(data= sa2_map,aes(x=long, y=lat, group=group, fill=population, label=SA2_NAME16))+
geom_hex(aes(x = hex_long, y = hex_lat, fill = population, label=SA2_NAME16),
           stat = "identity", colour = NA, alpha = 0.75)
)
# Interactive plot code

