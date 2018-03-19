library(readr)
ABS_ANNUAL_ERP <- read_csv("data/ABS_ANNUAL_ERP_ASGS2016_14032018111559407.csv")
View(ABS_ANNUAL_ERP)


ABS_ANNUAL_ERP %>% filter(REGIONTYPE=="SA2") %>% 
  select(SA2_NAME16 = Region, population = Value) -> SA2population

load("~/atlas/data/sa2Small.Rda")


SA2population[which(!(SA2population$SA2_NAME16 %in% sa2Small@data$SA2_NAME16)),] -> extraPop

sa2Small@data[which(!(sa2Small@data$SA2_NAME16 %in% SA2population$SA2_NAME16)),] -> extraAreas


sa2Small@data <- left_join(sa2Small@data, SA2population)

#save(sa2Small, file = "sa2Small.Rda")
