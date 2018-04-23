# gstat package variograms
library(sp)
library(gstat)

points <- sa2_hex %>% select(longitude = as.numeric(hex_long), 
                             latitude = as.numeric(hex_lat), pop)
coordinates(points) = ~longitude+latitude
v = variogram(pop~longitude + latitude, data=points, alpha=c(0,45,90,135),tol.hor=5)
ggplot(v,aes(x=dist,y=gamma,size=np)) + geom_point() + facet_wrap(~dir.hor)


g <- gstat(id='pop',formula=pop~1,
           model=vgm(0.9,'Sph',60,0.1,anis=c(45,0.1)),data=points,
           dummy=TRUE,beta=0)
expvar <- variogram(g,alpha=c(0,45,90,135),tol.hor=5)
ggplot(expvar,aes(x=dist,y=gamma,size=np,col=factor(dir.hor))) + geom_point() + facet_wrap(~dir.hor)
