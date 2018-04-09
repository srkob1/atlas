# Expand Grids

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
  mutate(latHex = ifelse(row_number() %% 2 == 1, lat, lat +(0.23/2)))


# plot grid on map
ggplot(data=sa2_map %>% filter(STE_NAME16=="Victoria")) +
geom_polygon(aes(
  x = long,
  y = lat,
  group = group
)) +
  geom_point(data=vicGrid, aes(x=long, y=latHex))


# WESTERN AUSTRALIA
# use long, lat, radius to create a grid

waSPDF <- subset(sa2Small, STE_NAME16=="Western Australia")

bboxwa <- waSPDF@bbox %>% as.data.frame()
rownames(bboxwa) <- c("long", "lat")

waGrid <- expand.grid(long = seq(bboxwa[1,1], bboxwa[1,2], radius),
                       lat = seq(bboxwa[2,1], bboxwa[2,2], radius))

# move every second row right by 0.23/2
waGrid <- waGrid %>% 
  mutate(latHex = ifelse(row_number() %% 2 == 1, lat, lat +(0.23/2)))

ggplot(data=sa2_map %>% filter(STE_NAME16=="Western Australia")) +
  geom_polygon(aes(
    x = long,
    y = lat,
    group = group
  )) +
  geom_point(data=waGrid, aes(x=long, y=latHex))
