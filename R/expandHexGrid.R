# Expand Grids

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

load("~/atlas/data/sa2Small.Rda")
load("~/atlas/data/sa2_map.Rda")

vicSPDF <- subset(sa2Small, STE_NAME16=="Victoria")

# Vic Starting positions

ncentroids <- length(vicSPDF)

bboxvic <- vicSPDF@bbox %>% as.data.frame()
rownames(bboxvic) <- c("long", "lat")

# Vic Expanded grid
#amount of new centroids to allocate hexagons to
nhexes <- (2*ncentroids)

# ratio of long and lat for vic
ratio <- (bboxvic[2,2] - bboxvic[2,1])/(bboxvic[1,2] - bboxvic[1,1])

# expand to include every combination
radius <- 0.23
vicGrid <- expand.grid(long = seq(bboxvic[1,1], bboxvic[1,2], radius),
lat = seq(bboxvic[2,1], bboxvic[2,2], radius))

# move every second row right by 0.23/2
vicGrid <- vicGrid %>% 
  mutate(lat = ifelse(row_number() %% 2 == 1, lat, lat +(0.23/2)))


# plot grid on map
ggplot(data=sa2_map %>% filter(STE_NAME16=="Victoria")) +
geom_polygon(aes(
  x = long,
  y = lat,
  group = group
)) +
  geom_point(data=vicGrid, aes(x=long, y=lat))


# WESTERN AUSTRALIA
# use long, lat, radius to create a grid

waSPDF <- subset(sa2Small, STE_NAME16=="Western Australia")

bboxwa <- waSPDF@bbox %>% as.data.frame()
rownames(bboxwa) <- c("long", "lat")

waGrid <- expand.grid(long = seq(bboxwa[1,1], bboxwa[1,2], radius),
                       lat = seq(bboxwa[2,1], bboxwa[2,2], radius))

# move every second row right by 0.23/2
waGrid <- waGrid %>% 
  mutate(lat = ifelse(row_number() %% 2 == 1, lat, lat +(0.23/2)))

ggplot(data=sa2_map %>% filter(STE_NAME16=="Western Australia")) +
  geom_polygon(aes(
    x = long,
    y = lat,
    group = group
  )) +
  geom_point(data=waGrid, aes(x=long, y=lat))


# use long, lat, radius to create a grid

ntSPDF <- subset(sa2Small, STE_NAME16=="Northern Territory")

bboxnt <- ntSPDF@bbox %>% as.data.frame()
rownames(bboxnt) <- c("long", "lat")

# create grid that hexagons could be assigned to
ntGrid <- expand.grid(long = seq(bboxnt[1,1], bboxnt[1,2], radius),
                      lat = seq(bboxnt[2,1], bboxnt[2,2], radius))

# move every second row right by 0.23/2
ntGrid <- ntGrid %>% 
  mutate(lat = ifelse(row_number() %% 2 == 1, lat, lat +(0.23/2)))

ggplot(data=sa2_map %>% filter(STE_NAME16=="Northern Territory")) +
  geom_polygon(aes(
    x = long,
    y = lat,
    group = group
  )) +
  geom_point(data=ntGrid, aes(x=long, y=lat))



# Assigning hexagons function
hex_centroids <-NULL

assign_hexagons <- function(long_c, lat_c, hex_long, hex_lat) {
  # consider each hexagon 
  hexVec <- NULL
  
  # check for equal length
  stopifnot(length(long_c) ==length(lat_c))
  
  # create a tibble of all available grid points to allocate an area to
  grid <- cbind(hex_long, hex_lat) %>% as.tibble %>%
    mutate(id = 1:length(hex_long),
           assigned = ifelse(id %in% hexVec, TRUE, FALSE))
  
  # for each area to be allocated to a grid point
  for (i in 1:NROW(long_c)) {
    print(i)
    olong <- long_c[i]
    olat <- lat_c[i]
    
    
    grid_nasgn <- grid %>% filter(!assigned)
    
    distance <- distVincentyEllipsoid(
      c(olong, olat), grid_nasgn,
        a=6378249.145, b=6356514.86955, f=1/293.465)
    mindist_id <- cbind(grid_nasgn, distance) %>% 
      arrange(distance)
    
    id <- mindist_id$id[1]
    
    grid$assigned[id] <- TRUE
    
    hexVec <- c(hexVec, id)
    
    new_centroid <- mindist_id[1,]
    
    hex_centroids <- bind_rows(hex_centroids, new_centroid)
    
  }
  
  return(hex_centroids)
}


#debug(assign_hexagons)

assign_hexagons(data$long, data$lat, waGrid$long, waGrid$lat) ->ah

# create ordered list to allocate hexagons 
sa2_map %>% filter(STE_NAME16=="Western Australia") %>%
  arrange(desc(population)) %>%
  distinct(id, .keep_all = T) %>%
  bind_cols(., assign_hexagons(.$long, .$lat, waGrid$long, waGrid$lat)) -> hexagons


ggplot(data=sa2_map %>% filter(STE_NAME16=="Western Australia")) +
  geom_polygon(aes(
    x = long,
    y = lat,
    group = group), fill="white"
  ) +
  geom_point(data=ah, aes(x=hex_long, y=hex_lat, colour=distance))

