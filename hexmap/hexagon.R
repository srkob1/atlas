# Function to create grid
# Input is the 2x2 matrix defining bounding box
# and the radius, which sets the binwidth for the grid
create_grid <- function(bbox, radius) {
  grid <- expand.grid(hex_long = seq(bbox[1,1],
                                 bbox[1,2],
                                 radius),
                      hex_lat = seq(bbox[2,1],
                                   bbox[2,2],
                                   radius))

  # Only shift every second latitude - to make hex structure
  lat <- grid %>% select(hex_lat) %>%
      distinct() %>%
      filter(row_number() %% 2 == 1) %>% unlist()

  grid <- grid %>% rowwise %>%
    mutate(hex_long = ifelse(hex_lat %in% lat, hex_long, hex_long + (radius/2)))
  
  return(grid)
}

# Function to take lat/long centroids from spatial polygons
# and assign to closest hexgrid location.
# DI SAYS: change separate long, lat input to one tibble for
#         centroids, and one for hexgrid
# STEFF SAYS: what about using only the shape file, create grid internally?
assign_hexagons <- function(centroid_data, grid_data) {
  # consider each hexagon
  hexVec <- NULL
  hex_centroids <- NULL
  
  long_c <- centroid_data$long 
  lat_c <- centroid_data$lat

  # check for equal length
  stopifnot(length(long_c) ==length(lat_c))

  # create a tibble of all available grid points to allocate an area to
  grid <- grid_data %>% as.tibble %>%
    mutate(id = 1:length(grid_data$long),
           assigned = ifelse(id %in% hexVec, TRUE, FALSE))

  # Loop over spatial polygon centroids to assign to nearest hexgrid
  for (i in 1:NROW(long_c)) {
    print(i)
    olong <- long_c[i]
    olat <- lat_c[i]
    
    grid_nasgn <- grid %>% filter(!assigned)
    
    distance <- distVincentyEllipsoid(
      c(olong, olat), c(grid_nasgn$hex_long,grid_nasgn$hex_lat),
      a=6378249.145, b=6356514.86955, f=1/293.465)
    distance_df <- cbind(grid_nasgn, distance) %>%
      arrange(distance)
    
    mindist_id <- distance_df$id[1]
    grid$assigned[mindist_id] <- TRUE
    hexVec <- c(hexVec, mindist_id)
    new_centroid <- distance_df[1,]
    hex_centroids <- bind_rows(hex_centroids, new_centroid)
  }

  return(hex_centroids)
}
