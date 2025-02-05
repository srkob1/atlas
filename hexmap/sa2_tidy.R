# Create the tidy sa2 data frame

# Download and simplify sa2 2011 shape file
library(sf)
library(rgdal)
library(rmapshaper)
SAtwo = readOGR(dsn="data/SA2_2011_AUST", layer="SA2_2011_AUST")
SAtwo@data$id = rownames(SAtwo@data)
sa2 <- rmapshaper::ms_simplify(SAtwo, keep=0.05)


# Add area centroids
polys <- as(sa2, "SpatialPolygons")

centroid <- function(i, polys) {
  ctr <- Polygon(polys[i])@labpt
  id <- sa2$id[i]
  data.frame(long_c=ctr[1], lat_c=ctr[2], id = id)
}

centroids <- seq_along(polys) %>% purrr::map_df(centroid, polys=polys)

#join the centroids to full data set

sa2@data <- full_join(sa2@data, centroids)

# Add population 
ABS_ANNUAL_ERP <- read_csv("data/ABS_ANNUAL_ERP.csv")

SA2population <- ABS_ANNUAL_ERP %>% filter(REGIONTYPE=="SA2") %>%
 dplyr::select(SA2_NAME11 = Region, population = Value)

sa2@data <- left_join(sa2@data, SA2population)

# Add Significant Urban Area
SUA <- read_csv("data/SA2_SUA_2011_AUST.csv") %>%
  dplyr::select(SA2_NAME_2011, SUA_NAME_2011)

sa2@data <- left_join(sa2@data, SUA, 
                      by = c("SA2_NAME11" = "SA2_NAME_2011"))


# Join data with points
sa2.points <- fortify(sa2, region="id")
sa2_tidy <- full_join(sa2.points, sa2@data, by="id")
