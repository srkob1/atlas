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

load("data/sa2Small.Rda")
load("data/sa2_map.Rda")


sa2_map <- sa2_map %>% filter(!(SA2_NAME16=="Norfolk Island"))

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


# TASMANIA
# use long, lat, radius to create a grid

tasSPDF <- subset(sa2Small, STE_NAME16=="Tasmania")

bboxtas <- tasSPDF@bbox %>% as.data.frame()
rownames(bboxtas) <- c("long", "lat")

tasGrid <- expand.grid(long = seq(bboxtas[1,1], bboxtas[1,2], radius),
                      lat = seq(bboxtas[2,1], bboxtas[2,2], radius))

# move every second row right by 0.23/2
waGrid <- waGrid %>%
  mutate(lat = ifelse(row_number() %% 2 == 1, lat, lat +(0.23/2)))


# NORTHERN TERRITORY
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


# SOUTH AUSTRALIA
# use long, lat, radius to create a grid

saSPDF <- subset(sa2Small, STE_NAME16=="SOUTH AUSTRALIA")

bboxsa <- saSPDF@bbox %>% as.data.frame()
rownames(bboxsa) <- c("long", "lat")

# create grid that hexagons could be assigned to
saGrid <- expand.grid(long = seq(bboxsa[1,1], bboxsa[1,2], radius),
                      lat = seq(bboxsa[2,1], bboxsa[2,2], radius))

# move every second row right by 0.23/2
saGrid <- saGrid %>%
  mutate(lat = ifelse(row_number() %% 2 == 1, lat, lat +(0.23/2)))


# QLD
# use long, lat, radius to create a grid

qldSPDF <- subset(sa2Small, STE_NAME16=="Queensland")

bboxqld <- qldSPDF@bbox %>% as.data.frame()
rownames(bboxqld) <- c("long", "lat")

# create grid that hexagons could be assigned to
qldGrid <- expand.grid(long = seq(bboxqld[1,1], bboxqld[1,2], radius),
                      lat = seq(bboxqld[2,1], bboxqld[2,2], radius))

# move every second row right by 0.23/2
qldGrid <- qldGrid %>%
  mutate(lat = ifelse(row_number() %% 2 == 1, lat, lat +(0.23/2)))



# New South Wales
# Not enough points in ACT to allocate all areas, included with NSW to allow more space

nswSPDF <- subset(sa2Small, STE_NAME16=="New South Wales" | STE_NAME16== "Australian Capital Territory")

bboxnsw <- nswSPDF@bbox %>% as.data.frame()
rownames(bboxnsw) <- c("long", "lat")
bboxnsw[1,2] <- 155.00

# create grid that hexagons could be assigned to
nswGrid <- expand.grid(long = seq(bboxnsw[1,1], bboxnsw[1,2], radius),
                       lat = seq(bboxnsw[2,1], bboxnsw[2,2], radius))


# to fix shifts in grid
# list of all latitudes
latList <- nswGrid %>% select(lat) %>% distinct()
# list of latitudes to shift (every second)
latShift <- latList %>% filter(row_number() %% 2 == 1) 


# move rows in list right by 0.23/2
nswGrid <- nswGrid %>% rowwise %>%
  mutate(long = ifelse(lat %in% latShift$lat, long, long + (0.23/2)))



# New South Wales
# Not enough points in ACT to allocate all areas, included with aus to allow more space

# Aus radius
radius <-0.45

ausSPDF <- sa2Small
bboxaus <- ausSPDF@bbox %>% as.data.frame()
rownames(bboxaus) <- c("long", "lat")
#disregard Islands
bboxaus[1,1] <- 111.00
bboxaus[1,2] <- 154.00

# create grid that hexagons could be assigned to
ausGrid <- expand.grid(long = seq(bboxaus[1,1], bboxaus[1,2], radius),
                       lat = seq(bboxaus[2,1], bboxaus[2,2], radius))


# to fix shifts in grid
# list of all latitudes
latList <- ausGrid %>% select(lat) %>% distinct()
# list of latitudes to shift (every second)
latShift <- latList %>% filter(row_number() %% 2 == 1) 


# move rows in list right by 0.23/2
ausGrid <- ausGrid %>% rowwise %>%
  mutate(long = ifelse(lat %in% latShift$lat, long, long + (0.23/2)))

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


#


# APPLY ASSIGN POLYGONS TO REGIONS
# create ordered list to allocate hexagons
sa2_map %>% 
  arrange(desc(population)) %>%
  distinct(id, .keep_all = T) %>%
  bind_cols(., assign_hexagons(.$long, .$lat, ausGrid$long, ausGrid$lat)) -> ausGridAllocations


#plot all Australia

myPalette <- colorRampPalette(rev(brewer.pal(9, "Greens")))
sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(1, 400000))

ggplot() +
  sc +
  coord_equal() +
  guides(colour = FALSE) +
  theme_void()  +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  ) +
  geom_polygon(data=sa2_map,aes(
    x = long,
    y = lat,
    group = group), fill="grey"
  ) +
  geom_point(data=vicGridAllocations, aes(x=hex_long, y=hex_lat, colour=distance), size=0.7)+
  geom_point(data=tasGridAllocations, aes(x=hex_long, y=hex_lat, colour=distance), size=0.7)+
  geom_point(data=nswGridAllocations, aes(x=hex_long, y=hex_lat, colour=distance), size=0.7)+
  geom_point(data=saGridAllocations, aes(x=hex_long, y=hex_lat, colour=distance), size=0.7)+
  geom_point(data=ntGridAllocations, aes(x=hex_long, y=hex_lat, colour=distance), size=0.7)+
  geom_point(data=qldGridAllocations, aes(x=hex_long, y=hex_lat, colour=distance), size=0.7) +
  
  
  ggplot() +
    geom_polygon(data=sa2_map %>% filter(STE_NAME16=="New South Wales"),aes(
      x = long,
      y = lat,
      group = group), fill="grey"
    ) +
  geom_point(data=nswGridAllocations, 
             aes(x=hex_long, y=hex_lat, colour=distance), size=2, alpha=0.5)





  ausGridAllocations %>% 
    filter(hex_long> 111.0) -> ausGridMainland
  



  
 plot<- ggplot() +
    geom_polygon(data=sa2_map,aes(
      x = long,
      y = lat,
      group = group), fill="grey"
    ) +
    geom_point(data=ausGridMainland, 
               aes(x=hex_long, y=hex_lat, colour=STE_NAME16,label=SA2_NAME16), size=0.5)+
    coord_equal() +
    guides(colour=FALSE)+
    theme_void()  +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.background = element_rect(fill = "transparent", colour = NA)
    )
plotly::ggplotly(plot)

ggplot() +
  geom_polygon(data=sa2_map,aes(
    x = long,
    y = lat,
    group = group), fill="grey"
  ) +
  geom_point(data=ausGrid, 
             aes(x=long, y=lat), size=0.5)+
  coord_equal() +
  guides(colour=FALSE)+
  theme_void()  +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )
