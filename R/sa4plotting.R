library(plyr)
library(tidyverse)
library(readr)

library(sf)
library(rgdal)
library(rmapshaper)
library(ggthemes)
library(ggrepel)
#library(hexmapr)
library(geogrid)

#path = "data/simulations/vic", 

filenames <- list.files(pattern="*.csv", full.names=TRUE)

# list of areas
Y <- list("Melbourne - Inner")

# function to find the sum of the distance between the original centroids and the hex centroids allocated
resSum <- function(dat){
  sum<-NA
  if("distance" %in% colnames(dat)){
    dat %>% distinct(.$SA2_NAME16, .keep_all = T) %>% select(distance) %>% sum -> sum
  }
  return(sum)
}


plot_data <- function (dfsi=dfsi, resdf=resdf, b){
  
  id <- resdf$id[[b]]
  dst_df <- dfsi[[id]]
  
  # hex map of simulation 
  dst_df <- dst_df %>% mutate(label = paste(gsub(" ", "\n", SA2_NAME16)))
  
  
  plot <- ggplot(dst_df) +
    geom_polygon(aes(x = long, y = lat, group = group, fill=distance)) +
    geom_text(aes(V1, V2, label = label), size = 3, color = "black") +
    scale_fill_distiller() +
    coord_equal() +
    theme_void()  +
    theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "transparent",colour = NA),
      plot.background = element_rect(fill = "transparent",colour = NA)
    )
  ggsave(paste0("Melbourne - Inner", res15, ".png"), plots[[1]][[p]], bg = "transparent")
  plot
}


dfs <- list()
plots <- list()
res <- list()

#create plots
for (i in seq(Y)){
  
  sub <- subset(filenames, grepl(Y[[i]], filenames))
  dfs[[i]] <- dfsi <- llply(sub, read_csv)
  print(paste(i, Y[[i]]))
  
  # collect distance column
  # calculate residuals
  res[[i]] <- lapply(dfsi, resSum)
  
  # the sum of the residual distances for melbourne inner
  #res <- lapply(dfs[[i]], resSum)
  
  # the simulation with the smallest sum of the residual distances for melbourne inner
  #mymin <- res[[i]] %>% unlist() %>% which.min()
  
  #data frame of sim and distance
  resdf <- res[[i]] %>% unlist() %>% data.frame(id = seq(res[[1]]), distance=.) %>% arrange(distance)
  res15 <-resdf$id[1:15]
  
  # data frame of simulation that had the minimum sum of the residual distances
  #dst_min_df <- dfs[[i]][[mymin]]
  #ggplot(dst_min_df%>% distinct(SA2_NAME16, .keep_all = T), aes(x=distance)) + geom_histogram()
  
  for (b in seq(res15)){
   plots[[b]] <- plot_data(dfsi=dfsi, resdf=resdf, b=b)
  }
  
  
}
  
  
  #names(dfsi)[[i]] <- Y[[i]]



#export plots
for (p in seq(plots)) {
  plots[[p]]
  ggsave(paste0("Melbourne - Inner", res15[[p]], ".png"), plots[[p]], bg = "transparent")
}

