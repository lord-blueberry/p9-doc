library(ggplot2)
library(dplyr)

imageSize <- 3072
asinh <- scales::trans_new(name = 'asinh', transform = function(x) asinh(x*1000), 
                           inverse = function(x) sinh(x)/1000)

scale_color_discrete(name = "PSF Fraction", labels = paste("1 /" ,levels(combined$psfSize)))

folder <- "approx/PsfSize"
file <- "PsfSize4.txt"
outputfolder <- "./approx/output/"

data = read.table(file.path(folder,file), header=TRUE, dec=",", sep=";")
data[,"cycle"] = as.factor(data$cycle)
data["experimentName"] = "psf4"
for(i in c(8, 16,32, 64)) {
  file <- paste("PsfSize", i, ".txt", sep="")
  data2 = read.table(file.path(folder,file), header=TRUE, dec=",", sep=";")
  data2[,"cycle"] = as.factor(data2$cycle)
  data2["experimentName"] = paste("psf",i,sep="")
  data <- rbind(data, data2)
}
png(paste(outputfolder, "psfSize.png", sep=""),
    width = 6.0,
    height = 4.0,
    units = "in",
    res = 256)
ggplot(data = data, mapping = aes(x = seconds, y = objectiveNormal, color= experimentName)) + 
  xlab("time") +
  ylab("Objective value") +
  geom_line() +
  scale_y_continuous(trans= "log10")+
  scale_x_continuous(trans= "log10")+
  coord_cartesian(ylim=c(30, 280.0))
dev.off()


folder <- "approx/BlockSize"
file <- "block1.txt"
data = read.table(file.path(folder,file), header=TRUE, dec=",", sep=";")
data[,"cycle"] = as.factor(data$cycle)
data["experimentName"] = "block 1"
for(i in c(2, 4, 8)) {
  file <- paste("block", i, ".txt", sep="")
  data2 = read.table(file.path(folder,file), header=TRUE, dec=",", sep=";")
  data2[,"cycle"] = as.factor(data2$cycle)
  data2["experimentName"] = paste("block ",i,sep="")
  data <- rbind(data, data2)
}
png(paste(outputfolder, "blockSize.png", sep=""),
    width = 6.0,
    height = 4.0,
    units = "in",
    res = 256)
ggplot(data = data, mapping = aes(x = seconds, y = objectiveNormal, color= experimentName)) + 
  xlab("time") +
  ylab("Objective value") +
  geom_line() +
  scale_y_continuous(trans= "log10")+
  scale_x_continuous(trans= "log10")+
  coord_cartesian(ylim=c(30, 280.0))
dev.off()