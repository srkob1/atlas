# Implement Paula's smoothing for experiments

# Cancer Data
```{r}
library(readr)
library(tidyverse)

load("data/sa2_2011.Rda")

SIR_all_2014 <- read_csv("data/SIR_all_2014.csv") %>% as.tibble()
SIR_all_2014 %>% filter(cancergrp ==27) %>% mutate(SA2_5DIG11 = as.factor(sa2)) %>%
  dplyr::select(SA2_5DIG11, persons_logp50, persons_p50) %>% 
  left_join(sa2@data, .) -> sa2@data
```

# Find neighbours
We create the neighbourhood matrix needed to define the spatial random effect using the poly2nb() and the nb2INLA() functions of the spdep package. First, we use poly2nb() to create a neighbours list based on areas with contiguous boundaries. Each element of the list nb represents one area and contains the indices of its neighbours. For example, nb[[2]] contains the neighbours of area 2.

Then, we use nb2INLA() to convert this list into a file with the representation of the neighbourhood matrix as required by INLA. Then we read the file using the inla.read.graph() function of INLA, and store it in the object g which we will later use for specifying the spatial disease model with INLA.

```{r}
library(spdep)
library(INLA)

nb <- poly2nb(sa2)
head(nb)

nb2INLA("sa2.adj", nb)
g <- inla.read.graph(filename = "sa2.adj")
```

 
 
# Modelling
Yi|θi∼Poisson(Ei×θi), i=1,…,n,

log(θi)=β0+ui+vi.

β0 is the intercept
ui is a structured spatial effect, ui|u_i∼N(u¯δi,/1τ_u*n_δi)
(Conditionally autoregressive model (CAR)),
vi is an unstructured spatial effect, vi∼N(0,1/τ_v)


```{r}
sa2@data$re_u <- 1:nrow(sa2@data)
sa2@data$re_v <- 1:nrow(sa2@data)

formula <- persons_logp50 ~ 1 + f(re_u, model = "besag", graph = g, scale.model = TRUE) + f(re_v, model = "iid")

res <- inla(formula, family = "poisson", data = sa2@data,  control.predictor = list(compute = TRUE))

summary(res)
```


#Map spatially smoothed estimates

```{r}
sa2@data$RR <- res$summary.fitted.values[, "mean"]
sa2@data$LL <- res$summary.fitted.values[, "0.025quant"]
sa2@data$UL <- res$summary.fitted.values[, "0.975quant"]
```


```{r}
pal <- colorNumeric(palette = "YlOrRd", domain = sa2@data$RR)

labels <- sprintf("<strong> %s </strong> <br/> Population: %s <br/>
                  Smoothed: <br/>RR: %s (%s, %s)",
                  sa2@data$SA2_NAME11, sa2@data$population,
                  round(sa2@data$RR, 2), round(sa2@data$LL, 2), round(sa2@data$UL, 2)) %>%
  lapply(htmltools::HTML)

leaflet(sa2) %>% addTiles() %>%
    addPolygons(color = "grey", weight = 1, fillColor = ~pal(RR),  fillOpacity = 0.5,
    highlightOptions = highlightOptions(weight = 4),
    label = labels,
    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px", direction = "auto")) %>%
    addLegend(pal = pal, values = ~RR, opacity = 0.5, title = "RR", position = "bottomright")

```


