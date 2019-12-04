#library(rasterVis)
library(pals)
library(ggplot2)

asinh <- scales::trans_new(name = 'asinh', transform = function(x) asinh(x*1000), 
                           inverse = function(x) sinh(x)/1000)


inputFolder <- "./images/MSClean/"
outputfolder <- "./images/output/MSClean/"

data <- read.table(paste(inputFolder, "Briggs-CLEAN.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "Briggs-CLEAN.png", sep=""),
    width = 16.0,
    height = 16.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.5), guide=FALSE) + 
  theme_void()
dev.off()

data <- read.table(paste(inputFolder, "Briggs-N132.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "Briggs-N132.png", sep=""),
    width = 6.0,
    height = 5.0,
    units = "in",
    res = 256)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.9))
dev.off()

data <- read.table(paste(inputFolder, "Briggs-Calibration.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "Briggs-Calibration.png", sep=""),
    width = 6.0,
    height = 5.0,
    units = "in",
    res = 256)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.3))
dev.off()


data <- read.table(paste(inputFolder, "Natural-N132.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "Natural-N132.png", sep=""),
    width = 6.0,
    height = 5.0,
    units = "in",
    res = 256)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.9))
dev.off()

data <- read.table(paste(inputFolder, "Natural-Calibration.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "Natural-Calibration.png", sep=""),
    width = 6.0,
    height = 5.0,
    units = "in",
    res = 256)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.3))
dev.off()


