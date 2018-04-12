# Assign Hexagons using shape file

assign_hexagons <- function(shape, radius = 0.3) {
  # tracks allocated hex grid points
  hexVec <- NULL
  hex_centroids <- NULL
  
  # create a tibble of all available grid points to allocate an area to
  grid <- create_grid(shape@bbox, radius) %>%
    cbind(., id = 1:NROW(.), assigned = rep(FALSE, NROW(.)))
 
  # for each area to be allocated to a grid point
  
  for (i in 1:NROW(shape@data)) {
    print(i)
    # specific to shape file structure
    olong <- shape@polygons[[i]]@labpt[1]
    olat <- shape@polygons[[i]]@labpt[2]
     
    grid_nasgn <- grid %>% filter(!assigned)
    
    distance <- distVincentyEllipsoid(
      c(olong, olat), grid_nasgn,
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
