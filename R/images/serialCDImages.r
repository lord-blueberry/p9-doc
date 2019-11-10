#library(rasterVis)
library(pals)
library(ggplot2)

asinh <- scales::trans_new(name = 'asinh', transform = function(x) asinh(x*1000), 
                           inverse = function(x) sinh(x)/1000)


inputFolder <- "./images/SerialCD/"
outputfolder <- "./images/output/SerialCD/"

data <- read.table(paste(inputFolder, "CD-reference.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "CD-reference.png", sep=""),
    width = 16.0,
    height = 16.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.3), guide=FALSE) + 
  theme_void()
dev.off()

data <- read.table(paste(inputFolder, "CD-N132.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "CD-N132.png", sep=""),
    width = 6.0,
    height = 5.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.9))
dev.off()

data <- read.table(paste(inputFolder, "CD-Calibration.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "CD-Calibration.png", sep=""),
    width = 6.0,
    height = 5.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -2, hue = 1.5, gamma = 0.3))
dev.off()
