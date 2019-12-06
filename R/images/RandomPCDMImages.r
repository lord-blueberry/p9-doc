#library(rasterVis)
library(pals)
library(ggplot2)

asinh <- scales::trans_new(name = 'asinh', transform = function(x) asinh(x*1000), 
                           inverse = function(x) sinh(x)/1000)


inputFolder <- "./images/RandomPCDM/"
outputfolder <- "./images/output/RandomPCDM/"


data <- read.table(paste(inputFolder, "random_1k_block1.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "random_1k_block1.png", sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.5), guide=FALSE) +
  theme_void()
dev.off()

data <- read.table(paste(inputFolder, "random_10k_block1.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "random_10k_block1.png", sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.5), guide=FALSE) +
  theme_void()
dev.off()

data <- read.table(paste(inputFolder, "random_1k_block8.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "random_1k_block8.png", sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.9), guide=FALSE) +
  theme_void()
dev.off()

data <- read.table(paste(inputFolder, "random_10k_block8.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "random_10k_block8.png", sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.9), guide=FALSE) +
  theme_void()
dev.off()

#--------------------------------------------------------------PCDM Comparison----------------------------------------------------------------------
data <- read.table(paste(inputFolder, "SerialCD-N132.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "SerialCD-N132.png", sep=""),
    width = 6.0,
    height = 5.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.9))
dev.off()
data <- read.table(paste(inputFolder, "PCDM-N132.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "PCDM-N132.png", sep=""),
    width = 6.0,
    height = 5.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.9))
dev.off()

data <- read.table(paste(inputFolder, "SerialCD-Calibration.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "SerialCD-Calibration.png", sep=""),
    width = 6.0,
    height = 5.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.3))
dev.off()
data <- read.table(paste(inputFolder, "PCDM-Calibration.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "PCDM-Calibration.png", sep=""),
    width = 6.0,
    height = 5.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.3))
dev.off()