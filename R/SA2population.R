library(readr)
ABS_ANNUAL_ERP <- read_csv("data/ABS_ANNUAL_ERP_ASGS_13032018113413026.csv")
View(ABS_ANNUAL_ERP)


ABS_ANNUAL_ERP %>% spread(`Region Type`, Value)

ABS_ANNUAL_ERP %>% filter(REGIONTYPE=="SA2") %>% 
  select(SA2_NAME16 = Region, population = Value) -> SA2population

SA2population[which(!(SA2population$SA2_NAME16 %in% sa2Small@data$SA2_NAME16)),] -> extraPop

sa2Small@data[which(!(sa2Small@data$SA2_NAME16 %in% SA2population$SA2_NAME16)),] -> extraAreas


#map into df, matching possible suburbs
as.vector(extraPop$SA2_NAME16) %>%
map(~ grep(.,sa2Small@data$SA2_NAME16, value = T)) -> extraPopMatches

names(extraPopMatches) <- extraPopMatches$SA2_NAME16




#map into df, matching possible suburbs
as.vector(extraAreas$SA2_NAME16) %>%
  map(~ grep(.,sa2Small@data$SA2_NAME16, value = T)) -> extraAreaMatches

names(extraAreaMatches) <- extraAreaMatches$SA2_NAME16

