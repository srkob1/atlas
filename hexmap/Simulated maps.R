# Generate data for examples


w <- as.owin(list(xrange=sa2@bbox[1,],yrange=sa2@bbox[2,]))

sa2_ppp <- as.ppp(sa2_hex %>% select(hex_long, hex_lat), w)

ausCauchy <- rCauchy(4, 3, 100, win = w)

df <- tibble(x1, x2,  y2=5-2*x1+4*x2+10*(-x1)*x2+rnorm(500)*0.5)

ggplot(sa2_tidy) + geom_polygon(aes(x=long, y=lat, colour=hex_long))
ggplot(sa2_hex %>% mutate(c=hex_long)) + geom_point(aes(x=hex_long, y=hex_lat, colour=hex_long))

# Apply random process to our points based on a model:
set.seed(1244)
sa2_hex <- sa2_hex %>% mutate(c = 10 + rnorm(hex_long) + rnorm(hex_lat))
sa2_tidy <- sa2_hex %>% mutate(c = 10 + rnorm(hex_long) + rnorm(hex_lat))

ggplot(sa2_tidy) + geom_polygon(aes(x=long, y=lat, colour=hex_long))
ggplot(sa2_hex %>% mutate(c=hex_long)) + geom_point(aes(x=hex_long, y=hex_lat, colour=hex_long))


# homogeneous
X <- rCauchy(30, 0.01, 5)
# inhomogeneous
ff <- function(x,y){ exp(2 - 3 * abs(x)) }
Z <- as.im(ff, W= owin())
Y <- rCauchy(50, 0.01, Z)
YY <- rCauchy(ff, 0.01, 5)
plot(X)