#library(rasterVis)
library(pals)
library(ggplot2)

asinh <- scales::trans_new(name = 'asinh', transform = function(x) asinh(x*1000), 
                           inverse = function(x) sinh(x)/1000)


inputFolder <- "./images/simulated/"
outputfolder <- "./images/output/simulated/"

psf <- read.table(paste(inputFolder, "psf.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "psf.png", sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 200)
ggplot(psf, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.8), guide=FALSE) + 
  theme_void()
dev.off()

psfCut <- read.table(paste(inputFolder, "psfCut.csv" ,sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "psfCut.png", sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 200)
ggplot(psfCut, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.8), guide=FALSE) + 
  theme_void()
dev.off()

data <- read.table(paste(inputFolder, "psfZeroPadding.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "psfZeroPadding.png", sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.8), guide=FALSE) + 
  theme_void()
dev.off()

data <- read.table(paste(inputFolder, "psfCircular.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "psfCircular.png", sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.8), guide=FALSE) + 
  theme_void()
dev.off()

data <- read.table(paste(inputFolder, "elasticNet.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "elasticNet.png", sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.8), guide=FALSE) + 
  theme_void()
dev.off()

data <- read.table(paste(inputFolder, "dirty.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "dirty.png", sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.8), guide=FALSE) + 
  theme_void()
dev.off()






psfSquared <- read.table(paste(inputFolder, "psfSquared.csv" ,sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "psfSquared.png", sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 200)
ggplot(psfSquared, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -1.5, hue = 1.5, gamma = 0.5), guide=FALSE) + 
  theme_void()
dev.off()

psfSquaredCut <- read.table(paste(inputFolder, "psfSquaredCut.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "psfSquaredCut.png", sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 200)
ggplot(psfSquaredCut, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.5), guide=FALSE) + 
  theme_void()
dev.off()

data <- read.table(paste(inputFolder, "psfSquaredEdge.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "psfSquaredEdge.png", sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.5), guide=FALSE) + 
  theme_void()
dev.off()