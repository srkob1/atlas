# Function to create grid
# Input is the 2x2 matrix defining bounding box
# and the radius, which sets the binwidth for the grid
create_grid <- function(bbox, radius) {
  grid <- expand.grid(long = seq(bbox[1,1],
                                 bbox[1,2],
                                 radius),
                      lat = seq(bbox[2,1],
                                   bbox[2,2],
                                   radius))

  # Only shift every second latitude - to make hex structure
  # DI SAYS: check if this creates a vector
    lat <- grid %>% select(lat) %>%
      distinct() %>%
      filter(row_number() %% 2 == 1) %>% unlist()

  grid <- grid %>% rowwise %>%
    mutate(long = ifelse(lat %in% lat, long, long + (radius/2)))
  return(grid)
}

# Function to take lat/long centroids from spatial polygons
# and assign to closest hexgrid location.
# DI SAYS: change separate long, lat input to one tibble for
#         centroids, and one for hexgrid
assign_hexagons <- function(long_c, lat_c, hex_long, hex_lat) {
  # consider each hexagon
  hexVec <- NULL

  # check for equal length
  stopifnot(length(long_c) ==length(lat_c))

  # create a tibble of all available grid points to allocate an area to
  grid <- cbind(hex_long, hex_lat) %>% as.tibble %>%
    mutate(id = 1:length(hex_long),
           assigned = ifelse(id %in% hexVec, TRUE, FALSE))

  # Loop over spatial polygon centroids to assign to nearest hexgrid
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
