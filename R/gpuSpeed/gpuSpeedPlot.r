library(ggplot2)


table = read.table("gpuSpeed/GPUSpeedup.txt", header=TRUE, dec=",", sep=";")
table["speedup"] <- table["timeCPU"] / table["timeGPU"]

png("./gpuSpeed/speedup.png",
    width = 6.0,
    height = 4.0,
    units = "in",
    res = 200)
ggplot(data = table, mapping = aes(x = imgSize, y = speedup)) + 
  xlab("Image Size deconvolved on the GPU") +
  ylab("Speedup Factor") +
  geom_line() + 
  geom_point() + scale_x_continuous(trans= "log2")
dev.off()



folder <- "gpuSpeed/distributed-run/"
file <- "1runtimestats.txt"
data = read.table(file.path(folder,file), header=FALSE, dec=".", sep=";")
processors <- c(1, 2, 4, 8, 16, 32)
sumTime <- c(sum(data$V3))
for(i in c(2, 4,8, 16,32)) {
  id = paste(i)
  id = sub("\\.", ",", id)
  file <- paste(i, "runtimestats.txt", sep="")
  data2 = read.table(file.path(folder,file), header=FALSE, dec=".", sep=";")
  sumTime <- c(sumTime, sum(data2$V3))
}
data <- data.frame(processors = processors, time = sumTime, speedup = sumTime[1]/sumTime)

png("./gpuSpeed/dist-speedup.png",
    width = 6.0,
    height = 4.0,
    units = "in",
    res = 200)
ggplot(data = data, mapping = aes(x = processors, y = speedup)) + 
  xlab("Number of Nodes in MPI") +
  ylab("Speedup Factor") +
  geom_line() + 
  geom_point() + scale_x_continuous(trans= "log2")
dev.off() 