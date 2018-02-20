library(plyr)
library(tidyverse)
library(readr)

library(sf)
library(rgdal)
library(rmapshaper)
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(ggrepel)
#library(hexmapr)
library(geogrid)

#path = "data/simulations/qld",

filenames <- list.files(path = "data/simulations/qld", pattern="*.csv", full.names=TRUE)

# list of areas
#Y <- list("Ballarat","Bendigo","Geelong","Hume","Latrobe","Melbourne - Inner East","Melbourne - Inner South","Melbourne - Inner","Melbourne - North East","Melbourne - North West","Melbourne - Outer East","Melbourne - South East","Melbourne - West","Mornington Peninsula","North West","Shepparton","Warrnambool and South West")
 Y <- list("Brisbane - East", "Brisbane - North", "Brisbane - South", "Brisbane - West", "Brisbane Inner City", "Cairns", "Central Queensland", "Darling Downs - Maranoa", "Gold Coast", "Ipswich", "Logan - Beaudesert", "Mackay - Isaac - Whitsunday", "Moreton Bay - North", "Moreton Bay - South", "Queensland - Outback", "Sunshine Coast", "Toowoomba")
   
# function to find the sum of the distance between the original centroids and the hex centroids allocated
resSum <- function(dat){
  sum<-NA
  if("distance" %in% colnames(dat)){
    dat %>% distinct(.$SA2_NAME16, .keep_all = T) %>% select(distance) %>% sum -> sum
  }
  return(sum)
}


dfs <- list()
plots <- list()
res <- list()

for (i in seq(Y)){
  
sub <- subset(filenames, grepl(Y[[i]], filenames))
dfs[[i]] <- dfsi <- llply(sub, read_csv)
print(paste(i, Y[[i]]))

# collect distance column
# calculate residuals
res[[i]] <- lapply(dfsi, resSum)
# maxs <- lapply(ldf, maxDist)

# the sum of the residual distances for melbourne inner
#res <- lapply(dfs[[i]], resSum)

# the simulation with the smallest sum of the residual distances for melbourne inner
mymin <- res[[i]] %>% unlist() %>% which.min()

# data frame of simulation that had the minimum sum of the residual distances
dst_min_df <- dfs[[i]][[mymin]]
#ggplot(dst_min_df%>% distinct(SA2_NAME16, .keep_all = T), aes(x=distance)) + geom_histogram()

# hex map of simulation that had the minimum sum of the residual distances
dst_min_df <- dst_min_df %>% mutate(label = paste(gsub("-", "- \n", SA2_NAME16)))

plots[[i]] <- 
  ggplot(dst_min_df) +
  geom_polygon(aes(x = long, y = lat, group = group, fill=distance)) +
  geom_text(aes(V1, V2, label = label), size = 4, color = "black") +
  scale_fill_distiller() +
  coord_equal() +
  theme_void()  +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",colour = NA),
    plot.background = element_rect(fill = "transparent",colour = NA)
  )

names(plots)[[i]] <- paste0(Y[[i]], mymin)
names(dfs)[[i]] <- Y[[i]]
}



#export plots
for (i in seq(plots)) {
ggsave(paste0(names(plots)[i], ".png"), plots[i][[1]], bg = "transparent")
}

