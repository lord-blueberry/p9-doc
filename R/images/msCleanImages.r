#library(rasterVis)
library(pals)
library(ggplot2)

asinh <- scales::trans_new(name = 'asinh', transform = function(x) asinh(x*1000), 
                           inverse = function(x) sinh(x)/1000)


inputFolder <- "./images/MSClean/"
outputfolder <- "./images/output/MSClean/"

data <- read.table(paste(inputFolder, "Briggs-CLEAN.csv", sep=""), sep=";", header=TRUE, dec=",")
limit <- 0.05
data$intensity[data$intensity > limit] = limit
png(paste(outputfolder, "Briggs-CLEAN.png", sep=""),
    width = 16.0,
    height = 16.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.50, hue = 1.0, gamma = 0.5), guide=FALSE) + 
  theme_void()
dev.off()

data <- read.table(paste(inputFolder, "briggs-CLEAN-residuals.csv", sep=""), sep=";", header=TRUE, dec=",")
minimum <- -0.01279203
maximum <- 0.05184013
png(paste(outputfolder, "briggs-CLEAN-residuals.png", sep=""),
    width = 9.0,
    height = 8.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.50, hue = 1.0, gamma = 0.5), name="Jansky/Beam", limit = c(minimum, maximum)) + 
  theme_void()
dev.off()

data <- read.table(paste(inputFolder, "Briggs-N132.csv", sep=""), sep=";", header=TRUE, dec=",")
data$x <- (data$x - min(data$x)) * 1.5 / 60.0
data$y <- (data$y - min(data$y)) * 1.5 / 60.0
png(paste(outputfolder, "Briggs-N132.png", sep=""),
    width = 6.0,
    height = 4.50,
    units = "in",
    res = 256)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.00, r = -1.50, hue = 1.0, gamma = 0.9), name="Jansky/Beam") +
  xlab("Ascension (arc seconds)") +
  ylab("Declination (arc seconds)")
dev.off()

data <- read.table(paste(inputFolder, "Briggs-Calibration.csv", sep=""), sep=";", header=TRUE, dec=",")
data$x <- (data$x - min(data$x)) * 1.5 / 60.0
data$y <- (data$y - min(data$y)) * 1.5 / 60.0
limit <- 0.05
data$intensity[data$intensity > limit] = limit
png(paste(outputfolder, "Briggs-Calibration.png", sep=""),
    width = 6.0,
    height = 4.50,
    units = "in",
    res = 256)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.00, r = -1.5, hue = 1.0, gamma = 0.5), name="Jansky/Beam") +
  xlab("Ascension (arc seconds)") +
  ylab("Declination (arc seconds)")
dev.off()





data <- read.table(paste(inputFolder, "Natural-CLEAN.csv", sep=""), sep=";", header=TRUE, dec=",")
limit <- 0.0005
negLimit <- -0.00001
data$intensity[data$intensity < negLimit] = negLimit
data$intensity[data$intensity > limit] = limit
png(paste(outputfolder, "Natural-CLEAN.png", sep=""),
    width = 16.0,
    height = 16.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.50, hue = 1.0, gamma = 0.5), guide=FALSE) + 
  theme_void()
dev.off()

data <- read.table(paste(inputFolder, "Natural-CLEAN-residuals.csv", sep=""), sep=";", header=TRUE, dec=",")
minimum <- -0.01279203
maximum <-0.05184013
png(paste(outputfolder, "Natural-CLEAN-residuals.png", sep=""),
    width = 9.0,
    height = 8.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.50, hue = 1.0, gamma = 0.5), name="Jansky/Beam", limit = c(minimum, maximum)) + 
  theme_void()
dev.off()

data <- read.table(paste(inputFolder, "Natural-N132.csv", sep=""), sep=";", header=TRUE, dec=",")
data$x <- (data$x - min(data$x)) * 1.5 / 60.0
data$y <- (data$y - min(data$y)) * 1.5 / 60.0
negLimit <- -0.00001
data$intensity[data$intensity < negLimit] = negLimit
png(paste(outputfolder, "Natural-N132.png", sep=""),
    width = 6.0,
    height = 4.50,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.50, hue = 1.0, gamma = 0.5), name="Jansky/Beam") +
  xlab("Ascension (arc seconds)") +
  ylab("Declination (arc seconds)")
dev.off()

data <- read.table(paste(inputFolder, "Natural-image-N132.csv", sep=""), sep=";", header=TRUE, dec=",")
data$x <- (data$x - min(data$x)) * 1.5 / 60.0
data$y <- (data$y - min(data$y)) * 1.5 / 60.0
maximum <- 1.343942
png(paste(outputfolder, "Natural-image-N132.png", sep=""),
    width = 6.0,
    height = 4.50,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.50, hue = 1.0, gamma = 0.5), name="Jansky/Beam", limit = c(0, maximum)) +
  xlab("Ascension (arc seconds)") +
  ylab("Declination (arc seconds)")
dev.off()

data <- read.table(paste(inputFolder, "Natural-Calibration.csv", sep=""), sep=";", header=TRUE, dec=",")
data$x <- (data$x - min(data$x)) * 1.5 / 60.0
data$y <- (data$y - min(data$y)) * 1.5 / 60.0
limit <- 0.0005
negLimit <- -0.00001
data$intensity[data$intensity > limit] = limit
data$intensity[data$intensity < negLimit] = negLimit
png(paste(outputfolder, "Natural-Calibration.png", sep=""),
    width = 6.0,
    height = 4.50,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.50, hue = 1.0, gamma = 0.5), name="Jansky/Beam") +
  xlab("Ascension (arc seconds)") +
  ylab("Declination (arc seconds)")
dev.off()

data <- read.table(paste(inputFolder, "Natural-image-Calibration.csv", sep=""), sep=";", header=TRUE, dec=",")
data$x <- (data$x - min(data$x)) * 1.5 / 60.0
data$y <- (data$y - min(data$y)) * 1.5 / 60.0
maximum <- 0.3406
negLimit <- -0.00001
data$intensity[data$intensity < negLimit] = negLimit
png(paste(outputfolder, "Natural-image-Calibration.png", sep=""),
    width = 6.0,
    height = 4.50,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.50, hue = 1.0, gamma = 0.5), name="Jansky/Beam", limit = c(negLimit, maximum)) +
  xlab("Ascension (arc seconds)") +
  ylab("Declination (arc seconds)")
dev.off()
