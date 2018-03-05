## Cartogramming VIC

library(eechidna)
load("~/atlas/data/sa2Small.Rda")
load("~/atlas/data/sa2_map.Rda")
load("~/atlas/data/sa2_data.Rda")

sa2_data %>% distinct(SA2_NAME16, .keep_all = T) %>% select(id, SA2_MAIN16:POPULATION)-> sa2info

Vic <- list("Ballarat","Bendigo","Geelong","Hume","Latrobe","Melbourne - Inner East","Melbourne - Inner South","Melbourne - Inner","Melbourne - North East","Melbourne - North West","Melbourne - Outer East","Melbourne - South East","Melbourne - West","Mornington Peninsula","North West","Shepparton","Warrnambool and South West")

sa2_data %>% filter(SA4_NAME16 %in% Vic) %>%
  select(id, SA2_NAME16, SA4_NAME16, SA2_5DIG16, long_c, lat_c, AREASQKM16) %>%
  distinct() -> Victoria

#Tasmania
library(plotly)
library(eechidna)
plot_tas <- sa2_data %>% filter(STE_NAME16=="Tasmania")
centroids_tas <- centroids %>% filter(ste=="Tasmania")

left_join(centroids_tas)
tas_carto <- aec_carto_f(centroids_tas, polygon.vertex=6, 
                         density=100, dist.ratio = 5)

tas_carto <- left_join(tas_carto, 
                       centroids_tas,
                       by = c("region"="id"))
tas_carto <- tas_carto %>% mutate(label = paste(gsub(" ", "\n", gsub(" - ", " ", name))))

distance <- c()

library(geosphere)
for (i in 1:nrow(tas_carto)) {
  distVincentyEllipsoid(
    c(tas_carto$x[i], tas_carto$y[i]),
    c(tas_carto$long_c[i], tas_carto$lat_c[i]),
    a = 6378249.145,
    b = 6356514.86955,
    f = 1 / 293.465
  ) -> distance[i]
}

tas_carto$distance <- distance


#Edit Tas locations

tas_carto$x[tas_carto$name=="King Island"] <- tas_carto$long_c[tas_carto$name=="King Island"] 
tas_carto$y[tas_carto$name=="King Island"] <- tas_carto$lat_c[tas_carto$name=="King Island"] 

tas_carto$x[tas_carto$name=="Flinders and Cape Barren Islands"] <- tas_carto$long_c[tas_carto$name=="Flinders and Cape Barren Islands"] 
tas_carto$y[tas_carto$name=="Flinders and Cape Barren Islands"] <- tas_carto$lat_c[tas_carto$name=="Flinders and Cape Barren Islands"] 



tas_carto$x[tas_carto$name=="Burnie - Ulverstone Region"] <- 146.0000
tas_carto$y[tas_carto$name=="Burnie - Ulverstone Region"] <- -42.0000

tas_carto$x[tas_carto$name=="Beauty Point - Beaconsfield"] <- 146.8800
tas_carto$y[tas_carto$name=="Beauty Point - Beaconsfield"] <- -41.2000

tas_carto$x[tas_carto$name=="Derwent Park - Lutana"] <- 147.514
tas_carto$y[tas_carto$name=="Derwent Park - Lutana"] <- -41.889

ggplot(data=plot_data) + 
 # geom_path(aes(x=long, y=lat, group=group),
  #          colour="grey50") +
  geom_hex(data=tas_carto, aes(x=x, y=y), size=3, alpha=0.1,
             colour="white", binwidth=c(0.5))+
  geom_segment(data=tas_carto, aes(x=long_c, xend=x, y=lat_c, yend=y, color=distance))+
  geom_point(data=tas_carto, aes(x=long_c, y=lat_c), colour="pink")+
  geom_text(data=tas_carto, aes(label=label, x=x, y=y),size=2)+
  coord_equal()




p <- ggplot(data=plot_tas) +
  geom_path(aes(x=long, y=lat, group=group),
            colour="grey50") +
  geom_point(data=tas_carto, aes(x=x, y=y, label=label), size=2, alpha=0.4,
             colour="#f0027f") +
  coord_equal()

ggplotly(p)

## hex bin points

#South Australia
centroids %>% filter(ste=="South Australia") ->centroids_sa

sa_carto <- aec_carto_f(centroids_sa)
sa_carto <- left_join(sa_carto, 
                       centroids_sa,
                       by = c("region"="id"))
sa_carto <- sa_carto %>% mutate(label = paste(gsub(" ", "\n", gsub(" - ", " ", name))))

p <- ggplot(data=sa2_data %>% filter(STE_NAME16=="South Australia")) + 
  geom_path(aes(x=long, y=lat, group=group),
            colour="grey50") +
  geom_point(data=sa_carto, aes(x=x, y=y, label=label), size=2, alpha=0.4,
             colour="#f0027f") +
  coord_equal() 


ggplotly(p)

## hex bin points

ggplot(data=sa2_data %>% filter(STE_NAME16=="South Australia")) + 
   geom_path(aes(x=long, y=lat, group=group),
            colour="grey50") +
  geom_hex(data=sa_carto, aes(x=x, y=y), size=1, alpha=0.2,
           colour="#f0027f", binwidth=c(0.5)) +
  geom_text(data=sa_carto, aes(label=label, x=x, y=y),size=2)+
  coord_equal()
