# This file has the code to actually create the Oz hexmap
# Australia
library(tidyverse)
library(geosphere)
library(hexbin)
library(gridExtra)
library(ggthemes)
library(plotly)
library(geosphere)

# source(sa2_tidy.R)

load("data/sa2_2011.Rda")
load("data/sa2_tidy11.Rda")

# Remove islands because we think they are not used in the atlas
# If they are, we simply need to shift them closer to the mainland
# to automatically lay out
sa2 <- subset(sa2, (STE_NAME11 != "Other Territories"))
sa2 <- subset(sa2, (SA2_NAME11 != "Lord Howe Island"))

sa2_tidy <- sa2_tidy %>%
  filter(STE_NAME11!="Other Territories") %>%
  filter(SA2_NAME11!="Lord Howe Island")


centroids <- sa2@data %>%
  mutate(name = SA2_NAME11,
         pop = population,
         long = long_c,
         lat = lat_c,
         SUA = case_when(SUA_NAME_2011 == "Not in any Significant Urban Area (NT)"
                                                     ~ "Not in any Significant Urban Area",
                         SUA_NAME_2011 == "Not in any Significant Urban Area (NSW)"
                                                     ~ "Not in any Significant Urban Area",
                         SUA_NAME_2011 == "Not in any Significant Urban Area (Qld)"
                                                     ~ "Not in any Significant Urban Area",
                         SUA_NAME_2011 == "Not in any Significant Urban Area (Vic.)"
                                                     ~ "Not in any Significant Urban Area",
                         SUA_NAME_2011 == "Not in any Significant Urban Area (WA)"
                                                     ~ "Not in any Significant Urban Area",
                         SUA_NAME_2011 == "Not in any Significant Urban Area (SA)"
                                                     ~ "Not in any Significant Urban Area",
                         SUA_NAME_2011 == "Not in any Significant Urban Area (Tas.)"
                                                     ~ "Not in any Significant Urban Area",
                         SUA_NAME_2011 == "Not in any Significant Urban Area (ACT)"
                                                     ~ "Not in any Significant Urban Area",
                         SUA_NAME_2011 == "Not in any Significant Urban Area (OT)"
                                                     ~ "Not in any Significant Urban Area",
                                                     TRUE ~ as.character(SUA_NAME_2011)))

# Find the functions to use
source("hexagon.R")

# Set radius for hexagons to layout a reasonable grid for the country
# This is chosen arbitrarily, depending on the desired amount of geographical space each hex is allocated
radius <- 0.3

# Create hexagon grid
grid <- create_grid(sa2@bbox, radius)
grid <- grid %>% mutate(id=1:nrow(grid), assigned=FALSE)

# Mapping SA2 to closest hexagon grid, in order of population,
# and then distance
# sa2_hex <- centroids %>%
#   arrange(desc(pop)) %>%
#   assign_hexagons(., grid)

load("data/sa2_hex11.Rda")

# Checks
ggplot(sa2_hex, aes(x=hex_long, y=hex_lat, colour=name)) + geom_point() + geom_point(aes(x=long, y=lat), shape=2)
p1 <- ggplot(sa2_hex, aes(x=long, y=hex_long, colour=rank(desc(pop)))) +
  geom_abline(slope=1, intercept=0) + geom_point(alpha=0.1) +
  theme(aspect.ratio=1, legend.position="none")
p2 <- ggplot(sa2_hex, aes(x=lat, y=hex_lat, colour=rank(desc(pop)))) +
  geom_abline(slope=1, intercept=0) + geom_point(alpha=0.1) +
  theme(aspect.ratio=1, legend.position="none")
grid.arrange(p1, p2, ncol=2)

# Save as csv
write_csv(sa2_hex, path = "data/sa2_hex.csv")
# and as Rda
save(sa2_hex, file = "data/sa2_hex.Rda")
load("data/sa2_hex.Rda")

ggplot(sa2_tidy) +
  geom_polygon(aes(x=long, y=lat, group=group),
               fill="white", colour="grey90") +
  geom_point(data=sa2_hex, aes(x=hex_long, y=hex_lat, label=name), alpha=0.5) +
  coord_map() + theme_map()

ggplotly()

# Using ochRe palette namatjira_qual
ggplot(sa2_tidy) +
  geom_polygon(aes(x=long, y=lat, group=group),
               fill="white", colour="grey90") +
  geom_hex(data=sa2_hex, aes(x = hex_long, y = hex_lat,
                             fill = pop, label=name),
         stat = "identity", colour = NA, alpha = 0.75) +
  scale_fill_gradient(low = "#d8c0a8", high = "#a86030") +
  theme_map()

ggplot(sa2_tidy) +
  geom_polygon(aes(x=long, y=lat, group=group, fill=population)) +
  scale_fill_gradient(low = "#d8c0a8", high = "#a86030") +
  theme_map()

sa2_hex <- sa2_hex %>%
  left_join(distinct(select(sa2_tidy, SA2_NAME11, STE_NAME11, GCC_NAME11)), by=c("name"="SA2_NAME11")) %>%
  rename(state=STE_NAME11, capitals=GCC_NAME11)
# Save with the extra info
write_csv(sa2_hex, path = "data/sa2_hex.csv")
save(sa2_hex, file = "data/sa2_hex.Rda")

ggplot(sa2_tidy) +
  geom_polygon(aes(x=long, y=lat, group=group),
               fill="white", colour="grey90") +
  geom_hex(data=sa2_hex, aes(x = hex_long, y = hex_lat,
                             fill = state, label=name),
           stat = "identity", colour=NA, alpha = 0.75) +
  theme_map()
