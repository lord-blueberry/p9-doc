#library(rasterVis)
library(pals)
library(ggplot2)
library(reshape2)
library(pixmap)

asinh <- scales::trans_new(name = 'asinh', transform = function(x) asinh(x*1000), 
                           inverse = function(x) sinh(x)/1000)


inputFolder <- "./images/SerialCD/"
outputfolder <- "./images/output/SerialCD/"

data <- read.table(paste(inputFolder, "CD-reference.csv", sep=""), sep=";", header=TRUE, dec=",")
limit <- 0.0005
data$intensity[data$intensity > limit] = limit
png(paste(outputfolder, "CD-reference.png", sep=""),
    width = 16.0,
    height = 16.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.5), guide=FALSE) + 
  theme_void()
dev.off()

data <- read.table(paste(inputFolder, "CD-reference-residuals.csv", sep=""), sep=";", header=TRUE, dec=",")
minimum <- -0.01279203
maximum <- 0.05184013
png(paste(outputfolder, "CD-reference-residuals.png", sep=""),
    width = 9.0,
    height = 8.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.5), name="Jansky/Beam", limit = c(minimum, maximum)) +
  theme_void()
dev.off()

data <- read.table(paste(inputFolder, "CD-Calibration.csv", sep=""), sep=";", header=TRUE, dec=",")
data$x <- (data$x - min(data$x)) * 1.5 / 60.0
data$y <- (data$y - min(data$y)) * 1.5 / 60.0
limit <- 0.0005
negLimit <- -0.00001
data$intensity[data$intensity > limit] = limit
png(paste(outputfolder, "CD-Calibration.png", sep=""),
    width = 6.0,
    height = 4.5,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.5), name="Jansky/Beam", limits=c(negLimit, limit)) +
  xlab("Ascension (arc seconds)") +
  ylab("Declination (arc seconds)")
dev.off()

data <- read.table(paste(inputFolder, "CD-image-Calibration.csv", sep=""), sep=";", header=TRUE, dec=",")
data$x <- (data$x - min(data$x)) * 1.5 / 60.0
data$y <- (data$y - min(data$y)) * 1.5 / 60.0
maximum <- 0.3405348
minimum <- -0.001587562
png(paste(outputfolder, "CD-image-Calibration.png", sep=""),
    width = 6.0,
    height = 4.5,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.5), name="Jansky/Beam", limit = c(minimum, maximum)) +
  xlab("Ascension (arc seconds)") +
  ylab("Declination (arc seconds)")
dev.off()

data <- read.table(paste(inputFolder, "CD-N132.csv", sep=""), sep=";", header=TRUE, dec=",")
data$x <- (data$x - min(data$x)) * 1.5 / 60.0
data$y <- (data$y - min(data$y)) * 1.5 / 60.0
png(paste(outputfolder, "CD-N132.png", sep=""),
    width = 6.0,
    height = 4.5,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.9), name="Jansky/Beam") +
  xlab("Ascension (arc seconds)") +
  ylab("Declination (arc seconds)")
dev.off()

data <- read.table(paste(inputFolder, "CD-image-N132.csv", sep=""), sep=";", header=TRUE, dec=",")
data$x <- (data$x - min(data$x)) * 1.5 / 60.0
data$y <- (data$y - min(data$y)) * 1.5 / 60.0
maximum <- 1.343942
png(paste(outputfolder, "CD-image-N132.png", sep=""),
    width = 6.0,
    height = 4.5,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.9), name="Jansky/Beam", limit = c(0, maximum)) +
  xlab("Ascension (arc seconds)") +
  ylab("Declination (arc seconds)")
dev.off()



png(paste(outputfolder, "CD-N132-naked.png", sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.9), guide=FALSE) +
  theme_void()
dev.off()










#reshaped <- acast(data, y~x, value.var="intensity")
#write.table(reshaped, file ="N132.csv", sep = ";", row.names = FALSE, col.names = FALSE)

ftN132 <- read.table(paste(inputFolder, "ftN132.csv", sep=""), sep=";", header=FALSE)
minFT <- min(ftN132)
mask <- getChannels(read.pnm(paste(inputFolder, "mask.pgm", sep="")))
masked <- ftN132 * mask
masked[masked == 0] = minFT

ftData <- c()
yData <- c()
xData <- c()
for(i in 1:nrow(ftN132)) {
  for(j in 1:ncol(ftN132)) {
    ftData <- c(ftData, masked[i, j])
    yData <- c(yData, j)
    xData <- c(xData, i)
  }
}
dfN132 <- data.frame(y=yData, x=xData, intensity = log(ftData))

png(paste(outputfolder, "CD-N132-FT.png", sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 200)
min(ftData)
ggplot(dfN132, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors= cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.9), guide=FALSE) +
  theme_void()
dev.off()
