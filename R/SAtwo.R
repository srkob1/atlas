#Hex grid Victoria
library(sf)
library(rgdal)
library(rmapshaper)
library(tidyverse)
library(ggplot2)
library(ggthemes)

# download from web address:

# http://www.ausstats.abs.gov.au/ausstats/subscriber.nsf/0/A09309ACB3FA50B8CA257FED0013D420/$File/1270055001_sa2_2016_aust_shape.zip

SAtwo = readOGR(dsn="./data/SA2_2016_AUST.shp", layer="SA2_2016_AUST")
SAtwo@data$id = rownames(SAtwo@data)
SAtwos <- rmapshaper::ms_simplify(SAtwo, keep=0.05)
SAtwos.points = fortify(SAtwos, region="id")
SAtwos.df = full_join(SAtwos.points, SAtwos@data, by="id")


melb <- SAtwos.df %>% mutate(code = as.numeric(SA3_CODE16)) %>%
  filter(GCC_CODE16=="2GMEL")

ggplot(data=melb) +
  aes(long, lat, group=group, fill = GCC_CODE16) +
  geom_polygon() +
  scale_fill_manual(values = alpha(c("blue"), .02))+
  theme(legend.position = "none") +
  theme(panel.background = element_rect(fill = "transparent", colour = NA), plot.background = element_rect(fill = "transparent", colour = NA)) +
  geom_path(colour="black") + 
  geom_text_repel(data= 
    melb %>%
    group_by(id) %>%
    mutate(x=mean(long), y=mean(lat)) %>%
    distinct(id, .keep_all = TRUE), 
    aes(x=x, y=y,label=SA2_NAME16),
    size = 2)
  
