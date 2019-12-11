library(ggplot2)
library(dplyr)

imageSize <- 3072
asinh <- scales::trans_new(name = 'asinh', transform = function(x) asinh(x*1000), 
                           inverse = function(x) sinh(x)/1000)



folder <- "approx/PsfSize"
file <- "PsfSize4.txt"
outputfolder <- "./approx/output/"

data = read.table(file.path(folder,file), header=TRUE, dec=",", sep=";")
data[,"cycle"] = as.factor(data$cycle)
data["experimentName"] = 4
for(i in c(8, 16,32, 64)) {
  file <- paste("PsfSize", i, ".txt", sep="")
  data2 = read.table(file.path(folder,file), header=TRUE, dec=",", sep=";")
  data2[,"cycle"] = as.factor(data2$cycle)
  data2["experimentName"] = i
  data <- rbind(data, data2)
}
data[, "experimentName"] <- as.factor(data[, "experimentName"])

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
  scale_color_discrete(name = "PSF Fraction", labels = paste("1 /" ,levels(data$experimentName))) +
  coord_cartesian(ylim=c(30, 280.0))
dev.off()


folder <- "approx/BlockSize"
file <- "block1.txt"
data = read.table(file.path(folder,file), header=TRUE, dec=",", sep=";")
data[,"cycle"] = as.factor(data$cycle)
data["experimentName"] = 1
for(i in c(2, 4, 8)) {
  file <- paste("block", i, ".txt", sep="")
  data2 = read.table(file.path(folder,file), header=TRUE, dec=",", sep=";")
  data2[,"cycle"] = as.factor(data2$cycle)
  data2["experimentName"] = i
  data <- rbind(data, data2)
}
data[, "experimentName"] <- as.factor(data[, "experimentName"])

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
  scale_color_discrete(name = "Block Size", labels = levels(data$experimentName)) +
  coord_cartesian(ylim=c(30, 280.0))
dev.off()



folder <- "approx/searchTest"
file <- "SearchPercent0,01.txt"
data = read.table(file.path(folder,file), header=TRUE, dec=",", sep=";")
data[,"cycle"] = as.factor(data$cycle)
data["experimentName"] = 0.01
for(i in c(0.1, 0.2, 0.4, 0.8)) {
  id = paste(i)
  id = sub("\\.", ",", id)
  file <- paste("SearchPercent", id, ".txt", sep="")
  data2 = read.table(file.path(folder,file), header=TRUE, dec=",", sep=";")
  data2[,"cycle"] = as.factor(data2$cycle)
  data2["experimentName"] = i
  data <- rbind(data, data2)
}
data[, "experimentName"] <- as.factor(data[, "experimentName"])

png(paste(outputfolder, "searchPercent.png", sep=""),
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
  scale_color_discrete(name = "Search %", labels = levels(data$experimentName)) +
  coord_cartesian(ylim=c(30, 280.0))
dev.off()



folder <- "approx/noAcceleration"
file <- "PsfSize32.txt"
data = read.table(file.path(folder,file), header=TRUE, dec=",", sep=";")
data[,"cycle"] = as.factor(data$cycle)
data["experimentName"] = "APPROX"
file <- "TestNotAccelerated.txt"
data2 = read.table(file.path(folder,file), header=TRUE, dec=",", sep=";")
data2[,"cycle"] = as.factor(data2$cycle)
data2[, "experimentName"] <- "PCDM"
data <- rbind(data, data2)
data[, "experimentName"] <- as.factor(data[, "experimentName"])
png(paste(outputfolder, "acceleration.png", sep=""),
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
  scale_color_discrete(name = "Algorithm", labels = levels(data$experimentName)) +
  coord_cartesian(ylim=c(30, 280.0))
dev.off()



folder <- "approx/pcdm-run/"
totalIters <- c()
totalTimes <- c()
processors <- c(1, 4, 8, 16, 32)
for(i in processors) {
  file <- paste("pcdm", i, ".txt", sep="")
  data = read.table(file.path(folder,file), header=TRUE, dec=".", sep=";")
  data$iter[6] = 0
  totalIters <- c(totalIters, sum(data$iter))
  totalTimes <- c(totalTimes, sum(data$time))
}
timePerIteration <- totalTimes/totalIters
df <- data.frame(times = totalTimes, iters = totalIters, processors = processors, timePerIter=timePerIteration, speedupIter=timePerIteration[1]/timePerIteration, speedupTime = totalTimes[1]/totalTimes)

png(paste(outputfolder, "speedup_pcdm_time.png", sep=""),
    width = 2.5,
    height = 3.0,
    units = "in",
    res = 256)
ggplot(data = df, mapping = aes(x = processors, y = speedupTime)) + 
  xlab("Async. Processors") +
  ylab("Total Speedup") +
  geom_line() +
  scale_x_continuous(trans= "log2")
dev.off()

png(paste(outputfolder, "speedup_pcdm_iter.png", sep=""),
    width = 2.5,
    height = 3.0,
    units = "in",
    res = 256)
ggplot(data = df, mapping = aes(x = processors, y = totalIters)) + 
  xlab("Async. Processors") +
  ylab("Total number of iterations") +
  geom_line() +
  scale_x_continuous(trans= "log2")
dev.off()
  
