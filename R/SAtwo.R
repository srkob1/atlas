#Hex grid Victoria
library(sf)
library(rgdal)
library(rmapshaper)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(hexmapr)

# download from web address:

# http://www.ausstats.abs.gov.au/ausstats/subscriber.nsf/0/A09309ACB3FA50B8CA257FED0013D420/$File/1270055001_sa2_2016_aust_shape.zip

SAtwo = readOGR(dsn="./data/SA2_2016_AUST.shp", layer="SA2_2016_AUST")
SAtwo@data$id = rownames(SAtwo@data)
SAtwos <- rmapshaper::ms_simplify(SAtwo, keep=0.05)
SAtwos.points = fortify(SAtwos, region="id")
SAtwos.df = full_join(SAtwos.points, SAtwos@data, by="id")

polys <- as(SAtwo, "SpatialPolygons")

centroid <- function(i, polys) {
  ctr <- Polygon(polys[i])@labpt
  id <- SAtwo$id[i]
  data.frame(long_c=ctr[1], lat_c=ctr[2], id = id)
}

centroids <- seq_along(polys) %>% purrr::map_df(centroid, polys=polys)

#join the centroids to full data set

SAtwos.df <- full_join(SAtwos.df, centroids)

melb <- SAtwos.df %>% mutate(code = as.numeric(SA3_CODE16)) %>%
  filter(GCC_CODE16=="2GMEL")

ggplot(data=melb) +
  aes(long, lat, group=group, fill = GCC_CODE16) +
  geom_polygon() +
  scale_fill_manual(values = alpha(c("blue"), .02))+
  theme(legend.position = "none") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA), plot.background = element_rect(fill = "transparent", colour = NA)) +
  geom_path(colour="black") + 
  geom_text_repel(data= 
    melb %>%
    group_by(id) %>%
    mutate(x=mean(long), y=mean(lat)) %>%
    distinct(id, .keep_all = TRUE), 
    aes(x=x, y=y,label=SA2_NAME16),
    size = 2)
  

ggplot(melb) +
  geom_polygon(aes(x = long, y = lat, group = group)) +
  geom_text(aes(long_c, lat_c, label = substr(SA2_NAME16, 1, 4)), size = 2,color = "white") +
  coord_equal() +
  scale_fill_viridis() +
  guides(fill = FALSE) +
  theme_void()

#Hex Mapping


melbSPDF <- subset(SAtwos, GCC_CODE16=="2GMEL")
vicSPDF <- subset(SAtwos, STE_NAME16=="Victoria")


new_cells <- calculate_grid(shape = melbSPDF, grid_type = "hexagonal", seed = 1994)
# takes too long!
hexmelb <- assign_polygons(melbSPDF, new_cells)
#save(hexmelb, file="hexmelb.Rdata")



new_vic <- calculate_grid(shape = vicSPDF, grid_type = "hexagonal", seed = 1994)
# takes too long!
hexmelb <- assign_polygons(vicSPDF, new_vic)
#save(hexmelb, file="hexmelb.Rdata")


# From STAT585, use to extract ploygons?
#rather than using the assign_polygons function
extractPolygons <- function(shapes) {
  require(plyr)

  dframe <- ldply(1:length(shapes@polygons), function(i) {
    ob <- shapes@polygons[[i]]@Polygons
    dframe <- ldply(1:length(ob), function(j) {
      x <- ob[[j]]
      co <- x@coords
      data.frame(co, order = 1:nrow(co), group = j)
    })
    dframe$region <- new_cells[[2]]@polygons[[i]]@plotOrder
    dframe
  })
  # construct a group variable from both group and polygon:
  dframe$group <- interaction(dframe$region, dframe$group)

  dframe
}

expol <- extractPolygons(new_cells[[2]])
#expol$id <- substr(expol$region, 3,6)
expol$region %in% melbSPDF@plotOrder
new_cells.df = left_join(expol, melbSPDF@plotOrder, by=c("region" = "id"))


hexmelb@data$id = rownames(shape@data)
shape.points = fortify(shape, region="id")
shape.df = merge(shape.points, shape@data, by="id")

SAtwos.points = fortify(SAtwos, region="id")
SAtwos.df = full_join(SAtwos.points, SAtwos@data, by="id")

hexplot <- ggplot(hexmelb) +
  geom_polygon(aes(x = long, y = lat, group = group)) +
  geom_text(aes(V1, V2, label = substr(SA2_NAME16, 1, 4)), size = 2, color = "white") +
  scale_fill_viridis() +
  coord_equal() +
  guides(fill = FALSE) +
  theme_void()