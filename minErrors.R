library(plyr)

filenames <- list.files(pattern="*.csv", full.names=TRUE)

Y<-list("Ballarat",
        "Bendigo",
        "Geelong",
        "Hume",
        "Latrobe",
        "Melbourne - Inner East",
        "Melbourne - Inner South",
        "Melbourne - Inner",
        "Melbourne - North East",
        "Melbourne - North West",
        "Melbourne - Outer East",
        "Melbourne - South East",
        "Melbourne - West",
        "Mornington Peninsula",
        "North West",
        "Shepparton",
        "Warrnambool and South West")

for (i in seq(Y)){
  
sub <- subset(filenames, grepl(Y[[i]], filenames))
ldf <- llply(filenames, read.csv)
#collect distance column
#calculate residuals
#res <- lapply(ldf, summary)
names(res) <- filenames
}

#find simulation with minimum error