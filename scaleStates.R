# Combine Australia

load("~/atlas/actDataFrame.Rda")
load("~/atlas/tasDataFrame.Rda")
load("~/atlas/saDataFrame.Rda")
load("~/atlas/ntDataFrame.Rda")
load("~/atlas/waDataFrame.Rda")

#Distance between centroids (diameter) in ACT
#0.0445

#Distance between centroids (diameter) in Tas
#0.29088

#Distance between centroids (diameter) in SA
#0.76752

#Distance between centroids (diameter) in WA
#1.0229

#Distance between centroids (diameter) in NT
#1.32965

ggplot() +
  geom_polygon(data=ap_tasSPDF.df, aes(
    x = long,
    y = lat,
    group = group,
    fill = distance
  )) +
  geom_polygon(data=ap_waSPDF.df,aes(
    x = long,
    y = lat,
    group = group,
    fill = distance
  )) +
  geom_polygon(data=ap_saSPDF.df, aes(
    x = long,
    y = lat,
    group = group,
    fill = distance
  ))+
  geom_polygon(data=ap_ntSPDF.df, aes(
    x = long,
    y = lat,
    group = group,
    fill = distance
  )) +
  geom_polygon(data=actDataFrame, aes(
    x = long,
    y = lat,
    group = group,
    fill = distance
  )) +
  sc +
  coord_equal() +
  #guides(fill = FALSE) +
  theme_void()  +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent", colour = NA),
    plot.background = element_rect(fill = "transparent", colour = NA)
  )

# radius of smallest state
rad <- 0.0445
scale <- 0.0445/0.29088

# Tasmania rescaled
mean(ap_tasSPDF.df$V1)
mean(ap_tasSPDF.df$V2)

ap_tasSPDF.df<- ap_tasSPDF.df %>% mutate(sv1 = ((V1 - 146.0927)*scale) + 146.0927,
                                         slong = ((long - 146.0927)*scale) + 146.0927,
                                         slat = ((lat  + 41.65901)*scale) -41.65901,
                                         sv2 = ((V2 + 41.65901)*scale) -41.65901)


# to do
# change long lat to plot around new centroid (with radius equal to ACT radius)
# only ove points if distance is greater than a certain value
