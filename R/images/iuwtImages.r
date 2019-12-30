#library(rasterVis)
library(pals)
library(ggplot2)
library(reshape2)
library(pixmap)

asinh <- scales::trans_new(name = 'asinh', transform = function(x) asinh(x*1000), 
                           inverse = function(x) sinh(x)/1000)


inputFolder <- "./images/iuwt/"
outputfolder <- "./images/output/iuwt/"

data <- read.table(paste(inputFolder, "iuwt-model.csv", sep=""), sep=";", header=TRUE, dec=",")
limit <- 0.0005
negLimit <- -0.00001
data$intensity[data$intensity < negLimit] = negLimit
data$intensity[data$intensity > limit] = limit
data$intensity <- data$intensity * 1000
png(paste(outputfolder, "iuwt-model.png", sep=""),
    width = 8.0,
    height = 8.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.5), limits=c(negLimit*1000, limit*1000), name="mJy/Pixel") + 
  xlab("x (arc seconds)") +
  ylab("y (arc seconds)") +
  theme(legend.position = c(0.92, 0.15))
dev.off()

minimum <- -0.01279203
maximum <- 0.05184013
data <- read.table(paste(inputFolder, "iuwt-residuals.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "iuwt-residuals.png", sep=""),
    width = 8.0,
    height = 8.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.5), name="Jy/PSF", limit = c(minimum, maximum)) +
  xlab("x (arc seconds)") +
  ylab("y (arc seconds)") +
  theme(legend.position = c(0.92, 0.15))
dev.off()

data <- read.table(paste(inputFolder, "iuwt-Calibration.csv", sep=""), sep=";", header=TRUE, dec=",")
data$x <- (data$x - min(data$x)) * 1.5 / 60.0
data$y <- (data$y - min(data$y)) * 1.5 / 60.0
limit <- 0.0005
negLimit <- -0.00001
data$intensity[data$intensity > limit] = limit
data$intensity[data$intensity < negLimit] = negLimit
data$intensity = data$intensity * 1000
png(paste(outputfolder, "iuwt-Calibration.png", sep=""),
    width = 5.0,
    height = 5.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.5), name="mJy/Pixel", limit=c(negLimit*1000, limit*1000)) +
  xlab("x (arc seconds)") +
  ylab("y (arc seconds)") +
  theme(legend.position = c(0.92, 0.2))
dev.off()

data <- read.table(paste(inputFolder, "iuwt-image-Calibration.csv", sep=""), sep=";", header=TRUE, dec=",")
data$x <- (data$x - min(data$x)) * 1.5 / 60.0
data$y <- (data$y - min(data$y)) * 1.5 / 60.0
maximum <- 0.3406
minimum <- -0.001587562
#data$intensity[data$intensity < negLimit] = negLimit
png(paste(outputfolder, "iuwt-image-Calibration.png", sep=""),
    width = 5.0,
    height = 5.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.5), name="Jy/PSF", limit = c(minimum, maximum)) +
  xlab("x (arc seconds)") +
  ylab("y (arc seconds)") +
  theme(legend.position = c(0.92, 0.2))
dev.off()


data <- read.table(paste(inputFolder, "iuwt-N132.csv", sep=""), sep=";", header=TRUE, dec=",")
data$x <- (data$x - min(data$x)) * 1.5 / 60.0
data$y <- (data$y - min(data$y)) * 1.5 / 60.0
maximum <- 0.003016856
data$intensity = data$intensity * 1000
png(paste(outputfolder, "iuwt-N132.png", sep=""),
    width = 6.0,
    height = 5,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.9), limits=c(NA, maximum*1000), name="mJy/Pixel") +
  xlab("x (arc seconds)") +
  ylab("y (arc seconds)")
dev.off()

data <- read.table(paste(inputFolder, "iuwt-image-N132.csv", sep=""), sep=";", header=TRUE, dec=",")
data$x <- (data$x - min(data$x)) * 1.5 / 60.0
data$y <- (data$y - min(data$y)) * 1.5 / 60.0
maximum <- 1.343942
png(paste(outputfolder, "iuwt-image-N132.png", sep=""),
    width = 6.0,
    height = 5,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.9), name="Jy/PSF", limit = c(0, maximum)) +
  xlab("x (arc seconds)") +
  ylab("y (arc seconds)")
dev.off()
