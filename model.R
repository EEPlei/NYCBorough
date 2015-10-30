library(raster)
library(e1071)
library(dplyr)
load("data.Rdata")
load("NotNYC.Rdata")
names(data) = c("borough","x","y")

data1 <- sample_frac(data, 0.1)
names(data1) = c("borough","x","y")

sample$new <- sample$x
sample$x <- NULL
names(sample) <- c("borough","x","y")
data2 <- rbind(data1,sample)
data2$x <- as.numeric(data2$x)
data2$y <- as.numeric(data2$y)
data2 <- na.omit(data2)
data2$borough <- as.factor(data2$borough)


l<-svm(borough ~ .,
       data2,
       kernel = "radial"
)
dims = trunc(sqrt(nrow(data2))) + 1
boundary <- sapply(data2[,-1],range)
r = raster(nrows = dims, ncols = dims,
           xmn = boundary[,"x"][1] - 0.01, xmx = boundary[,"x"][2] + 0.01,
           ymn = boundary[,"y"][1] - 0.01, ymx = boundary[,"y"][2] + 0.01)
r[] = NA

pred_locs = data.frame(xyFromCell(r,1:dims^2))
pred = predict(l,pred_locs)
r[] = pred
plot(r)
points(data1$x,data1$y,pch=16,cex=0.1)


## Create Polygons
short_to_long = c("BROOKLYN"="Brooklyn", 
                  "BRONX"="Bronx",
                  "MANHATTAN"="Manhattan",
                  "QUEENS"="Queens",
                  "STATEN ISLAND"="Staten Island")
poly = rasterToPolygons(r,dissolve=TRUE)
poly = poly[-4,]
names(poly@data) = "Name"
poly@data$Names = short_to_long[levels(pred)[-4]]
source("write_geojson.R")
write_geojson(poly,"boroughs.json")
