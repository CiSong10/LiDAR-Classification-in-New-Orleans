# This code is originally developed by Sean Woznicki. https://www.gvsu.edu/wri/woznicki/
# Modified and Implemented by Ci Song, Oct 2022.
# Instructed by Derek Van Berkel

library(rgdal)
library(raster)
library(caret)
library(sp)
library(ggplot2)
library(RStoolbox)
library(klaR)
library(dplyr)
library(sf)

set.seed(22587)

int <- raster("NOLA_intensity_5m.tif")
hag <- raster("NOLA_HAG_5m.tif")
ndvi <- raster("NOLA_NDVI_5m.tif")
imp <- raster("NOLA_IMP_5m.tif")
naip <- brick("NOLA_RGBIR_5m.tif")

img <- addLayer(naip, ndvi, int, hag, imp)
names(img) <- c(paste0("B",1:8,coll="")) #R,G,B,NIR,NDVI,Intensity,HAG,Impervious (binary)

poly <- rgdal::readOGR("TrainingPolygon1010.shp")
test <- spsample(poly, n = 25000, type = "stratified")
# spsample: sample point locations in (or on) a spatial object
## pick a point in the sample polygon
## n: (approximate) sample size
## "stratified" for stratified random (one single random location in each "cell")
## type(test): SpatialPoints*
test.lulc.df <- extract(poly, test, df = TRUE)
# extract: Extract values from Raster objects
# extract(x, y, df=FALSE, ...)
## x: Raster* Object
## y: SpatialPoints*
## df=TRUE, results will be returned as a data.frame. 
### The first column is a sequential ID, the other column(s) are the extracted values
## type(test.lulc.df): dataframe

test.img.df <- extract(img, test, df=TRUE)

drops <- c("Shape_Leng","Shape_Area")
test.lulc.df2 <- test.lulc.df[, !(names(test.lulc.df) %in% drops)]
names(test.lulc.df2) <- c("ID", "LULC")

test.lulc.df2$LULC <- as.integer(test.lulc.df2$LULC)

# 1 impervious; 2 grass; 3 forest; 4 soil; 5 water

df.train <- merge(test.lulc.df2, test.img.df, by="ID")
df.train$LULC <- as.factor(df.train$LULC)

df.train %>% 
  group_by(LULC) %>%
  summarise(no_rows = length(LULC))

df.train <- na.omit(df.train)

split = 0.7
trainIndex <- createDataPartition(df.train$LULC, p=split, list=FALSE)
# Data Splitting functions
## y: a vector of outcomes
## p: the percentage of data that goes to training
## list: should the results be in a list (TRUE) or 
### a matrix with the number of rows equal to floor(p * length(y)) and times columns.

data_train <- df.train[ trainIndex,]
data_test <- df.train[-trainIndex,]

rf.train <- train(LULC~B1+B2+B3+B4+B5+B6+B7+B8, method='rf', data=data_train)

x_test <- data_test[,3:10] # B1~B8
y_test <- data_test[,2] # LULC
predictions <- predict(rf.train,x_test)
# predict(object, model,...)
## object: Raster* object. 
### Typically a multi-layer type (RasterStack or RasterBrick)
## model: fitted model of any class that has a 'predict' method

confusionMatrix(predictions, y_test)

importance <- varImp(rf.train)
# Variable importance refers to how much a given model "uses" that variable to make accurate predictions. 
plot(importance)

classify <- predict(img,rf.train)

classify.save <- writeRaster(classify,filename="NOLA_Classified_2017_5m_221010.tif", format="GTiff", overwrite=TRUE)
