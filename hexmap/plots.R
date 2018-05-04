# Load libraries and data sets
library(tidyverse)
library(ggthemes)
library(gridExtra)
library(plotly)
# Tidy format spatial polygons, for easy plotting
load("data/sa2_tidy.Rda")
# Hex gridded sa2 regions
load("data/sa2_hex.Rda")

# Error asociated with hexgrid, in comparison to original latitude and longitude. # We can see that most error occurs in a few locations, which corresponds to the # population centres - a lot of small polygons need to be expanded out to make
# the grid.

p1 <- ggplot(sa2_hex, aes(x=long, y=hex_long)) +
  geom_abline(slope=1, intercept=0) + geom_point(alpha=0.1) +
  theme(aspect.ratio=1, legend.position="none")
p2 <- ggplot(sa2_hex, aes(x=lat, y=hex_lat)) +
  geom_abline(slope=1, intercept=0) + geom_point(alpha=0.1) +
  theme(aspect.ratio=1, legend.position="none")
grid.arrange(p1, p2, ncol=2)

# Map of Australian SA2 areas coloured by population.

ggplot(sa2_tidy) +
  geom_polygon(aes(x=long, y=lat, group=group,
               fill=population), alpha=0.75)+
  scale_fill_gradient("Population", low = "#184848", high = "#c0c030") +
  theme_map()
ggplotly()

# Map of Australian SA2 areas represented by hexagons, coloured by population.

ggplot(sa2_tidy) +
  geom_polygon(aes(x=long, y=lat, group=group),
               fill="white", colour="grey90") +
  geom_hex(data=sa2_hex, aes(x = hex_long, y = hex_lat,
                             fill = pop, label=name),
         stat = "identity", colour = NA, alpha = 0.75) +
  scale_fill_gradient("Population", low = "#184848", high = "#c0c030") +
  theme_map()

ggplotly()

