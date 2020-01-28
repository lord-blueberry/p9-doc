#library(rasterVis)
library(pals)
library(ggplot2)
library(plotly)
library(orca)
library(stringr)

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
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.00, r = -1.5, hue = 1.0, gamma = 0.8), guide=FALSE) + 
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
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.00, r = -1.5, hue = 1.0, gamma = 0.8), guide=FALSE) + 
  theme_void()
dev.off()

psfCut <- read.table(paste(inputFolder, "psfReverseCut.csv" ,sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "psfReverseCut.png", sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 200)
ggplot(psfCut, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.00, r = -1.5, hue = 1.0, gamma = 0.8), guide=FALSE) + 
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
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.00, r = -1.5, hue = 1.0, gamma = 0.8), guide=FALSE) + 
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
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.8), guide=FALSE) + 
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
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.8), guide=FALSE) + 
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
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.8), guide=FALSE) + 
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
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -1.5, hue = 1.0, gamma = 0.5), guide=FALSE) + 
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
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -1.5, hue = 1.0, gamma = 0.5), guide=FALSE) + 
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
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -1.5, hue = 1.0, gamma = 0.5), guide=FALSE) + 
  theme_void()
dev.off()

data <- read.table(paste(inputFolder, "gradients.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "gradients.png", sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -1.5, hue = 1.0, gamma = 0.5), guide=FALSE) + 
  theme_void()
dev.off()


#----------------------------------------------- CLEAN images--------------------------------------------------------
inputFolder <- "./images/simulated/exampleCLEAN/"
outputfolder <- "./images/output/simulated/exampleCLEAN/"

data <- read.table(paste(inputFolder, "dirty_CLEAN_0.csv", sep=""), sep=";", header=TRUE, dec=",")
minD <- min(data$intensity)
maxD <- max(data$intensity)
for(i in 0:9) {
  data <- read.table(paste(inputFolder, "dirty_CLEAN_", i, ".csv", sep=""), sep=";", header=TRUE, dec=",")
  png(paste(outputfolder, paste("dirty_CLEAN_", str_pad(i, 2, pad="0"), ".png",sep=""), sep=""),
      width = 4.0,
      height = 4.0,
      units = "in",
      res = 192)
  plot <- ggplot(data, aes(x=x, y=y, fill=intensity))  +
    geom_tile() +
    scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -1.5, hue = 1.0, gamma = 0.8), guide=FALSE, limits=c(minD, maxD)) + 
    theme_void()
  print(plot)
  dev.off()
}

data <- read.table(paste(inputFolder, "rec_CLEAN.csv", sep=""), sep=";", header=TRUE, dec=",")
minR <- min(data$intensity)
maxR <- max(data$intensity)
png(paste(outputfolder, paste("rec_CLEAN.png",sep=""), sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 192)
plot <- ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -1.5, hue = 1.0, gamma = 0.8), guide=FALSE, limits=c(minR, maxR)) + 
  theme_void()
print(plot)
dev.off()

i <- 0
data <- read.table(paste(inputFolder, "model_CLEAN_", i, ".csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, paste("model_CLEAN_", str_pad(i, 2, pad="0"), ".png",sep=""), sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 192)
plot <- ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -1.5, hue = 1.0, gamma = 0.8), guide=FALSE, limits=c(minR, maxR)) + 
  theme_void()
print(plot)
dev.off()

for(i in 1:9) {
  data <- read.table(paste(inputFolder, "model_CLEAN_", i, ".csv", sep=""), sep=";", header=TRUE, dec=",")
  png(paste(outputfolder, paste("model_CLEAN_", str_pad(i, 2, pad="0"), ".png",sep=""), sep=""),
      width = 4.0,
      height = 4.0,
      units = "in",
      res = 192)
  plot <- ggplot(data, aes(x=x, y=y, fill=intensity))  +
    geom_tile() +
    scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.50, r = -1.5, hue = 1.0, gamma = 0.8), guide=FALSE) + 
    theme_void()
  print(plot)
  dev.off()
}




png(paste(outputfolder, "L2Norm.png", sep=""),
    width = 4.0,
    height = 4.0,
    units = "in",
    res = 200)
gauss = matrix(nrow=64, ncol=64)
x0 = 32
y0 = 32
for (y in 1:nrow(gauss))
  for (x in 1:ncol(gauss))
    gauss[y, x] = 1.0 * exp(-((x0 - x)^2 / 128 + (y0 - y)^2 / 128))
plot_ly(z=gauss, type="surface", colors="Blues")



single_pixel = matrix(nrow=64, ncol=64)
for (y in 1:nrow(single_pixel))
  for (x in 1:ncol(single_pixel))
    if(y == y0 & x == x0) {
      single_pixel[y, x] = 1.0
    } else {
      single_pixel[y, x] = 0.0
    }
plot_ly(z=single_pixel, type="surface", colors="Blues")





data <- read.table(paste(inputFolder, "cd-example/", "gradients_CD_0.csv", sep=""), sep=";", header=TRUE, dec=",")
data2 <- read.table(paste(inputFolder, "cd-example/", "gradients_CD_1.csv", sep=""), sep=";", header=TRUE, dec=",")
maxLimit <- max(data2$intensity)
minLimit <- min(data2$intensity)
png(paste(outputfolder, "cd-example/","gradients_CD_00.png", sep=""),
    width = 3.0,
    height = 3.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.5), guide=FALSE) + 
  theme_void()
dev.off()
for(i in 1:100) {
  print(i)
  data <- read.table(paste(inputFolder, "cd-example/", "gradients_CD_",i, ".csv", sep=""), sep=";", header=TRUE, dec=",")
  png(paste(outputfolder, "cd-example/","gradients_CD_", str_pad(i, 3, pad="0"),".png", sep=""),
      width = 3.0,
      height = 3.0,
      units = "in",
      res = 200)
  plot = ggplot(data, aes(x=x, y=y, fill=intensity))  +
    geom_tile() +
    scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.5), limit=c(NA, maxLimit),  guide=FALSE) + 
    theme_void()
  print(plot)
  dev.off()
}

data <- read.table(paste(inputFolder, "cd-example/", "model_CD_1.csv", sep=""), sep=";", header=TRUE, dec=",")
maxLimit <- max(data$intensity)
minLimit <- min(data$intensity)
data <- read.table(paste(inputFolder, "cd-example/", "model_CD_0.csv", sep=""), sep=";", header=TRUE, dec=",")
png(paste(outputfolder, "cd-example/","model_CD_000.png", sep=""),
    width = 3.0,
    height = 3.0,
    units = "in",
    res = 200)
ggplot(data, aes(x=x, y=y, fill=intensity))  +
  geom_tile() +
  scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.5), limit=c(NA, maxLimit),  guide=FALSE) + 
  theme_void()
dev.off()
for(i in 1:100) {
  print(i)
  data <- read.table(paste(inputFolder, "cd-example/", "model_CD_", i, ".csv", sep=""), sep=";", header=TRUE, dec=",")
  png(paste(outputfolder, "cd-example/","model_CD_",str_pad(i, 3, pad="0"),".png", sep=""),
      width = 3.0,
      height = 3.0,
      units = "in",
      res = 200)
  plot = ggplot(data, aes(x=x, y=y, fill=intensity))  +
    geom_tile() +
    scale_fill_gradientn(colors=cubehelix(n = 200, start = 0.0, r = -1.5, hue = 1.0, gamma = 0.5),  guide=FALSE) + 
    theme_void()
  print(plot)
  dev.off()
}
