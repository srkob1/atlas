# Animated hex maps

# Australia
library(tidyverse)
library(gganimate)
library(hexbin)
library(ggthemes)

read_plus <- function(flnm) {
  read_csv(flnm) %>% dplyr::select(
    SA2_NAME11, hex_long, hex_lat, distance, population)%>%
    mutate(filename = flnm)
}

allhex <-
list.files(path = "data/hex", pattern = "sa2_hex*",
full.names = T) %>%
map_df(~read_plus(.))

#allhex <- read_csv("data/sa2_hexanimate.csv")


load("data/sa2_tidy11.Rda")
sa2_tidy <- sa2_tidy %>%
  filter(STE_NAME11!="Other Territories") %>%
  filter(SA2_NAME11!="Lord Howe Island")


g1 <- ggplot(sa2_tidy) +
  geom_polygon(aes(x=long, y=lat, group=group),
               fill="white", colour="grey90") +
  geom_hex(data=allhex, aes(x = hex_long, y = hex_lat,
                                fill = population),
           stat = "identity", alpha = 0.75) +
  theme_map() + facet_wrap(~filename)

p <- ggplot(sa2_tidy) +
  geom_polygon(aes(x=long, y=lat, group=group),
               fill="white", colour="grey90") +
  geom_hex(data=allhex, aes(x = hex_long, y = hex_lat,
                            fill = filename, frame = filename),
           stat = "identity", alpha = 0.75) +
  theme_map()


ggplotly(p)
