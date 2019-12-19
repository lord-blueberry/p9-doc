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
png(paste(outputfolder, "iuwt-model.png", sep=""),
    width = 16.0,
    height = 16.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.5), guide=FALSE) + 
  theme_void()
dev.off()

minimum <- -0.01279203
maximum <- 0.0217463
data <- read.table(paste(inputFolder, "iuwt-residuals.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "iuwt-residuals.png", sep=""),
    width = 9.0,
    height = 8.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.5), name="Jansky/Beam", limit = c(minimum, maximum)) +
  theme_void()
dev.off()

data <- read.table(paste(inputFolder, "iuwt-Calibration.csv", sep=""), sep=";", header=TRUE, dec=",")
data$x <- (data$x - min(data$x)) * 1.5 / 60.0
data$y <- (data$y - min(data$y)) * 1.5 / 60.0
limit <- 0.0005
negLimit <- -0.00001
data$intensity[data$intensity > limit] = limit
data$intensity[data$intensity < negLimit] = negLimit
png(paste(outputfolder, "iuwt-Calibration.png", sep=""),
    width = 6.0,
    height = 4.5,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.5), name="Jansky/Beam") +
  xlab("Ascension (arc minutes)") +
  ylab("Declination (arc minutes)")
dev.off()


data <- read.table(paste(inputFolder, "iuwt-N132.csv", sep=""), sep=";", header=TRUE, dec=",")
data$x <- (data$x - min(data$x)) * 1.5 / 60.0
data$y <- (data$y - min(data$y)) * 1.5 / 60.0
png(paste(outputfolder, "iuwt-N132.png", sep=""),
    width = 6.0,
    height = 4.5,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.9), name="Jansky/Beam") +
  xlab("Ascension (arc minutes)") +
  ylab("Declination (arc minutes)")
dev.off()
