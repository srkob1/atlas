# Load libraries and data sets
library(tidyverse)
library(ggthemes)
library(gridExtra)
library(ggpolypath)
library(readxl)

# Set up standard colours
nac_colours <- c("A" = "#33809d",
                 "B" = "#aec6c7",
                 "C" = "#fff4bc",
                 "D" = "#ff9a64",
                 "E" = "#ff3500")


# Tidy format spatial polypaths, for easy plotting
load("data/sa2_tidy11.Rda")
# Hex gridded sa2 regions
load("data/sa2_hex11.Rda")
# Load Oz map - base for hexmap - smaller, more manageable
load("data/aus_map.Rda")

# Cancer SIR data - read xlsx, more reliable!
sa2_info <- read_xlsx("data/SIR_all_test.xlsx")
sa2_estimates <- read_xlsx("data/SIR_all_2014.xlsx",
                           col_types=c("text", "text", rep("numeric", 21)))
sa2_estimates <- sa2_estimates %>%
  mutate(cancergrp = as.factor(cancergrp),
         sa2 = as.factor(sa2))

cancers <- sa2_estimates %>% count(cancergrp)
sa2_info <- sa2_info %>% mutate(cancergrp = as.factor(cancergrp))
cancers <- inner_join(cancers, sa2_info[,1:2], by="cancergrp") %>% distinct()

# Test for oesophageal cancer
cr11 <- sa2_estimates %>%
  filter(cancergrp == 11) %>%
  select(cancergrp, sa2, persons_p50)
# Check match - there is one mismatch
cr11 %>% anti_join(sa2_hex, by=c("sa2"="SA2_5DIG11"))

# Now join, using hexes as base
cr11_hex <- left_join(sa2_hex %>% select(SA2_5DIG11, hex_long, hex_lat, long_c, lat_c, SA2_NAME11, GCC_NAME11, STE_NAME11, SUA), cr11, by=c("SA2_5DIG11"="sa2"))

cr11_hex <- cr11_hex %>% mutate(colrs = case_when(
  persons_p50 < 0.75  ~ "A",
  persons_p50 >= 0.75 & persons_p50 < 1  ~ "B",
  persons_p50 >= 1 & persons_p50 < 1.25  ~ "C",
  persons_p50 >= 1.25 & persons_p50 < 1.5  ~ "D",
  persons_p50 > 1.5  ~ "E")) %>%
  mutate(colrs = factor(colrs, levels = c("A", "B", "C", "D", "E")))

# Hexmap
ggplot() + geom_polypath(data=aus_map, aes(x=long, y=lat, group=group),
                         fill="grey98", colour="white", size=1) +
  geom_hex(data=cr11_hex, aes(x = hex_long, y = hex_lat,
                              fill = colrs),
           colour = NA, stat = "identity") +
  scale_fill_manual(values = nac_colours) +
  theme_map() +
  guides(fill=FALSE)
ggsave("Oesophageal_hex.png")

# Choropleth map
sa2_map <- left_join(sa2_tidy, cr11_hex, by="SA2_5DIG11")
ggplot() +
  geom_polygon(data=sa2_map, aes(x=long, y=lat, group=group,
                                  fill = colrs),
                colour="white", size=0.1) +
  #expand_limits(x = sa2_map$long, y = sa2_map$lat) +
  scale_fill_manual(values = nac_colours) +
  theme_map() +
  guides(fill=FALSE)
ggsave("Oesophageal_map.png")

# loop over cancers
for (i in 3:nrow(cancers)) {
  cat(i, "\n")
  cr <- sa2_estimates %>%
    filter(cancergrp == cancers$cancergrp[i]) %>%
    select(cancergrp, sa2, persons_p50)
  # Check match - there is one mismatch
  cr %>% anti_join(sa2_hex, by=c("sa2"="SA2_5DIG11"))

  # Now join, using hexes as base
  cr_hex <- left_join(sa2_hex %>% select(SA2_5DIG11, hex_long, hex_lat, long_c, lat_c, SA2_NAME11, GCC_NAME11, STE_NAME11, SUA), cr, by=c("SA2_5DIG11"="sa2"))

  cr_hex <- cr_hex %>% mutate(colrs = case_when(
    persons_p50 < 0.75  ~ "A",
    persons_p50 >= 0.75 & persons_p50 < 1  ~ "B",
    persons_p50 >= 1 & persons_p50 < 1.25  ~ "C",
    persons_p50 >= 1.25 & persons_p50 < 1.5  ~ "D",
    persons_p50 > 1.5  ~ "E")) %>%
    mutate(colrs = factor(colrs, levels = c("A", "B", "C", "D", "E")))

  # Hexmap
  p1 <- ggplot() + geom_polypath(data=aus_map, aes(x=long, y=lat, group=group),
                           fill="grey98", colour="white", size=1) +
    geom_hex(data=cr_hex, aes(x = hex_long, y = hex_lat,
                                fill = colrs),
             colour = NA, stat = "identity") +
    scale_fill_manual(values = nac_colours) +
    theme_map() +
    guides(fill=FALSE)
  flnm <- paste0(cancers$cancergrpname[i], "_hex.png")
  flnm <- sub(" cancer", "", flnm)
  ggsave(flnm, plot=p1)

  # Choropleth map
  sa2_map <- left_join(sa2_tidy, cr_hex, by="SA2_5DIG11")
  p2 <- ggplot() +
    geom_polygon(data=sa2_map, aes(x=long, y=lat, group=group,
                                   fill = colrs),
                 colour="white", size=0.1) +
    #expand_limits(x = sa2_map$long, y = sa2_map$lat) +
    scale_fill_manual(values = nac_colours) +
    theme_map() +
    guides(fill=FALSE)
  flnm <- paste0(cancers$cancergrpname[i], "_map.png")
  flnm <- sub(" cancer", "", flnm)
  ggsave(flnm, plot=p2)

}


# loop over female cancers
female_cancers <- cancers %>% filter(cancergrp %in% c(33, 35, 36, 37))
for (i in 1:nrow(female_cancers)) {
  cat(i, "\n")
  cr <- sa2_estimates %>%
    filter(cancergrp == female_cancers$cancergrp[i]) %>%
    select(cancergrp, sa2, females_p50)
  # Check match - there is one mismatch
  cr %>% anti_join(sa2_hex, by=c("sa2"="SA2_5DIG11"))

  # Now join, using hexes as base
  cr_hex <- left_join(sa2_hex %>% select(SA2_5DIG11, hex_long, hex_lat, long_c, lat_c, SA2_NAME11, GCC_NAME11, STE_NAME11, SUA), cr, by=c("SA2_5DIG11"="sa2"))

  cr_hex <- cr_hex %>% mutate(colrs = case_when(
    females_p50 < 0.75  ~ "A",
    females_p50 >= 0.75 & females_p50 < 1  ~ "B",
    females_p50 >= 1 & females_p50 < 1.25  ~ "C",
    females_p50 >= 1.25 & females_p50 < 1.5  ~ "D",
    females_p50 > 1.5  ~ "E")) %>%
    mutate(colrs = factor(colrs, levels = c("A", "B", "C", "D", "E")))

  # Hexmap
  p1 <- ggplot() + geom_polypath(data=aus_map, aes(x=long, y=lat, group=group),
                                 fill="grey98", colour="white", size=1) +
    geom_hex(data=cr_hex, aes(x = hex_long, y = hex_lat,
                              fill = colrs),
             colour = NA, stat = "identity") +
    scale_fill_manual(values = nac_colours) +
    theme_map() +
    guides(fill=FALSE)
  flnm <- paste0(female_cancers$cancergrpname[i], "_hex.png")
  flnm <- sub(" cancer", "", flnm)
  ggsave(flnm, plot=p1)

  # Choropleth map
  sa2_map <- left_join(sa2_tidy, cr_hex, by="SA2_5DIG11")
  p2 <- ggplot() +
    geom_polygon(data=sa2_map, aes(x=long, y=lat, group=group,
                                   fill = colrs),
                 colour="white", size=0.1) +
    #expand_limits(x = sa2_map$long, y = sa2_map$lat) +
    scale_fill_manual(values = nac_colours) +
    theme_map() +
    guides(fill=FALSE)
  flnm <- paste0(female_cancers$cancergrpname[i], "_map.png")
  flnm <- sub(" cancer", "", flnm)
  ggsave(flnm, plot=p2)

}

# loop over male cancers
male_cancers <- cancers %>% filter(cancergrp == 39)
  cr <- sa2_estimates %>%
    filter(cancergrp == male_cancers$cancergrp[1]) %>%
    select(cancergrp, sa2, males_p50)
  # Check match - there is one mismatch
  cr %>% anti_join(sa2_hex, by=c("sa2"="SA2_5DIG11"))

  # Now join, using hexes as base
  cr_hex <- left_join(sa2_hex %>% select(SA2_5DIG11, hex_long, hex_lat, long_c, lat_c, SA2_NAME11, GCC_NAME11, STE_NAME11, SUA), cr, by=c("SA2_5DIG11"="sa2"))

  cr_hex <- cr_hex %>% mutate(colrs = case_when(
    males_p50 < 0.75  ~ "A",
    males_p50 >= 0.75 & males_p50 < 1  ~ "B",
    males_p50 >= 1 & males_p50 < 1.25  ~ "C",
    males_p50 >= 1.25 & males_p50 < 1.5  ~ "D",
    males_p50 > 1.5  ~ "E")) %>%
    mutate(colrs = factor(colrs, levels = c("A", "B", "C", "D", "E")))

  # Hexmap
  p1 <- ggplot() + geom_polypath(data=aus_map, aes(x=long, y=lat, group=group),
                                 fill="grey98", colour="white", size=1) +
    geom_hex(data=cr_hex, aes(x = hex_long, y = hex_lat,
                              fill = colrs),
             colour = NA, stat = "identity") +
    scale_fill_manual(values = nac_colours) +
    theme_map() +
    guides(fill=FALSE)
  flnm <- paste0(male_cancers$cancergrpname[1], "_hex.png")
  flnm <- sub(" cancer", "", flnm)
  ggsave(flnm, plot=p1)

  # Choropleth map
  sa2_map <- left_join(sa2_tidy, cr_hex, by="SA2_5DIG11")
  p2 <- ggplot() +
    geom_polygon(data=sa2_map, aes(x=long, y=lat, group=group,
                                   fill = colrs),
                 colour="white", size=0.1) +
    #expand_limits(x = sa2_map$long, y = sa2_map$lat) +
    scale_fill_manual(values = nac_colours) +
    theme_map() +
    guides(fill=FALSE)
  flnm <- paste0(male_cancers$cancergrpname[1], "_map.png")
  flnm <- sub(" cancer", "", flnm)
  ggsave(flnm, plot=p2)

