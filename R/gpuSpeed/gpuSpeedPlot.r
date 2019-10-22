library(ggplot2)


table = read.table("gpuSpeed/GPUSpeedup.txt", header=TRUE, dec=",", sep=";")
table["speedup"] <- table["timeCPU"] / table["timeGPU"]

png("./gpuSpeed/speedup.png",
    width = 6.0,
    height = 4.0,
    units = "in",
    res = 200)
ggplot(data = table, mapping = aes(x = imgSize, y = speedup)) + 
  xlab("Image Size") +
  ylab("Speedup factor by GPU") +
  geom_line() + 
  geom_point() + scale_x_continuous(trans= "log2")
dev.off() 