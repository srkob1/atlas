# Function to create grid
# Inputs are the 2x2 matrix defining bounding box;
# the radius, which sets the binwidth for the grid;
# the proportion to expand the long of the grid 
# alternative to expand.grid
#

create_grid <- function(bbox, radius, expand_long = 0.1) {
  expand <- (bbox[1,2] - bbox[1,1])*expand_long
  #browser()
  grid <- as.tibble(expand.grid(hex_long = seq(bbox[1,1] - expand,
                                 bbox[1,2] + expand,
                                 radius),
                      hex_lat = seq(bbox[2,1],
                                   bbox[2,2],
                                   radius)))
  
  
  # Only shift every second latitude - to make hex structure
    lat <- grid %>% select(hex_lat) %>%
      distinct() %>%
      filter(row_number() %% 2 == 1) %>% unlist()

  grid <- grid %>%
    mutate(hex_long = ifelse(hex_lat %in% lat, hex_long,
                             hex_long + (radius/2)))

  return(grid)
}

grid_filter <- function(fgrid, gridbox, folong, folat){
  #browser()
  fgrid %>% filter(!assigned) %>% 
    filter(between(hex_lat, folat-gridbox, folat+gridbox)) %>%
    filter(between(hex_long, folong-gridbox, folong+gridbox))
}


# Function to take lat/long centroids from spatial polygons
# and assign to closest hexgrid location.
# DI SAYS: change separate long, lat input to one tibble for
#         centroids, and one for hexgrid
# STEFF SAYS: what about using only the shape file, create grid internally?
assign_hexagons <- function(centroids, grid, radius) {
  centroids$hex_long <- NA
  centroids$hex_lat <- NA
  centroids$hex_id <- NA
  centroids$distance <- NA

  # Loop over centroids to assign to nearest grid location
  for (i in 1:NROW(centroids)) {
    print(i)
    #browser()
    olong <- centroids$long[i]
    olat <- centroids$lat[i]

    width = max(grid$hex_long)-min(grid$hex_long)
    
    equation = ((exp(radius))/width)
    
    # implement filtering safely
        grid_nasgn <- grid_filter(fgrid = grid, 
                                           gridbox = equation,
                                           folong=olong,
                                           folat=olat)
        while (NROW(grid_nasgn)==0){
          message("Expanded")
          large_radius <- radius*1.5
          grid_nasgn <- grid_filter(fgrid = grid, 
                                    gridbox = equation,
                                    folong=olong,
                                    folat=olat)
        }
        
    distance <- distVincentyEllipsoid(
      c(olong, olat), cbind(grid_nasgn$hex_long,grid_nasgn$hex_lat),
      a=6378160, b=6356774.719, f=1/298.257222101)
    distance_df <- cbind(grid_nasgn, distance) %>%
      arrange(distance)
    
    print(distance_df[1,])
    mindist_id <- distance_df$id[1]
    
    grid$assigned[mindist_id] <- TRUE
    centroids$hex_id[i] <- mindist_id
    centroids$hex_long[i] <- distance_df$hex_long[1]
    centroids$hex_lat[i] <- distance_df$hex_lat[1]
    centroids$distance[i] <- distance_df$distance[1]
  }

  return(centroids)
}

