
library(ggplot2)

Rotate <- function(x, omega) {
  xCopy <- x
  xCopy[,"u"] <- cos(omega) * x[,"u"] - sin(omega) * x[,"v"]
  xCopy[,"v"] <- sin(omega) * x[,"u"] + cos(omega) * x[,"v"]
  
  return(xCopy)
}

uv <- read.csv("uv_meerkat.txt", sep=";", dec=",")
uv_freq <- uv
uv_freq["frequency"] = 1.0
uv_freq2 = uv * 1.1
uv_freq2["frequency"] = 1.1
uv_freq3 = uv * 1.2
uv_freq3["frequency"] = 1.2
frequencies <- data.frame(rbind(as.matrix(uv_freq), as.matrix(uv_freq2), as.matrix(uv_freq3)))
frequencies[, "frequency"] <- as.factor(frequencies[, "frequency"])

uvrot0 <- Rotate(uv, -pi/30)
uvrot1 <- Rotate(uv, -2*pi/30)
uv["timestep"]<- 0.0
uvrot0["timestep"] <- 1.0
uvrot1["timestep"] <- 2.0
timesteps <- data.frame(rbind(as.matrix(uv), as.matrix(uvrot0), as.matrix(uvrot1)))
timesteps[, "timestep"] <- as.factor(timesteps[, "timestep"])

minVal <- min(frequencies[,"u"], timesteps[,"u"], frequencies[,"v"], timesteps[,"v"])
maxVal <- max(frequencies[,"u"], timesteps[,"u"],frequencies[,"v"], timesteps[,"v"])


png("./uvPlots/snapshot.png",
    width = 6.0,
    height = 6.0,
    units = "in",
    res = 200)
ggplot(data = uv, mapping = aes(x = u, y = v)) + geom_point(alpha = 0.4) + xlim(minVal, maxVal) + ylim(minVal, maxVal)
dev.off()

png("./uvPlots/frequencies.png",
    width = 7.0,
    height = 6.0,
    units = "in",
    res = 200)
ggplot(data = frequencies, mapping = aes(x = u, y = v, color= frequency)) + geom_point(alpha = 0.4) + xlim(minVal, maxVal) + ylim(minVal, maxVal)
dev.off()


png("./uvPlots/timesteps.png",
    width = 7.0,
    height = 6.0,
    units = "in",
    res = 200)
ggplot(data = timesteps, mapping = aes(x = u, y = v, color=timestep)) + geom_point(alpha = 0.4) + xlim(minVal, maxVal) + ylim(minVal, maxVal)
dev.off()
