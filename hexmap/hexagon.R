# Function to create grid
# Inputs are the 2x2 matrix defining bounding box;
# the radius, which sets the binwidth for the grid;
# the proportion to expand the long of the grid 
create_grid <- function(bbox, radius, expand_long = 0.1) {
  expand <- (bbox[1,2] - bbox[1,1])*expand_long
  grid <- expand.grid(hex_long = seq(bbox[1,1] - expand,
                                 bbox[1,2] + expand,
                                 radius),
                      hex_lat = seq(bbox[2,1],
                                   bbox[2,2],
                                   radius))

  # Only shift every second latitude - to make hex structure
    lat <- grid %>% select(hex_lat) %>%
      distinct() %>%
      filter(row_number() %% 2 == 1) %>% unlist()

  grid <- grid %>% rowwise %>%
    mutate(hex_long = ifelse(hex_lat %in% lat, hex_long,
                             hex_long + (radius/2))) %>%
    ungroup()

  return(grid)
}

# Function to take lat/long centroids from spatial polygons
# and assign to closest hexgrid location.
# DI SAYS: change separate long, lat input to one tibble for
#         centroids, and one for hexgrid
# STEFF SAYS: what about using only the shape file, create grid internally?
assign_hexagons <- function(centroids, grid) {
  centroids$hex_long <- NA
  centroids$hex_lat <- NA
  centroids$hex_id <- NA
  centroids$distance <- NA

  # Loop over centroids to assign to nearest grid location
  for (i in 1:NROW(centroids)) {
    print(i)
    olong <- centroids$long[i]
    olat <- centroids$lat[i]

    grid_nasgn <- grid %>% filter(!assigned)

    distance <- distVincentyEllipsoid(
      c(olong, olat), cbind(grid_nasgn$hex_long,grid_nasgn$hex_lat),
      a=6378137, b=6356752.3141, f=1/298.257222101)
    distance_df <- cbind(grid_nasgn, distance) %>%
      arrange(distance)

    mindist_id <- distance_df$id[1]
    grid$assigned[mindist_id] <- TRUE
    centroids$hex_id[i] <- mindist_id
    centroids$hex_long[i] <- distance_df$hex_long[1]
    centroids$hex_lat[i] <- distance_df$hex_lat[1]
    centroids$distance[i] <- distance_df$distance[1]
  }

  return(centroids)
}
