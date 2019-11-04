
library(ggplot2)

SPEED_OF_LIGHT = 299792458.0
ToWavenumer <- function(uv, frequency)
{
  return(uv * frequency / SPEED_OF_LIGHT)
}

Rotate <- function(x, omega) {
  xCopy <- x
  xCopy[,"u"] <- cos(omega) * x[,"u"] - sin(omega) * x[,"v"]
  xCopy[,"v"] <- sin(omega) * x[,"u"] + cos(omega) * x[,"v"]
  
  return(xCopy)
}

uv <- read.csv("uvPlots/uv_meerkat.txt", sep=";", dec=",")
uv_freq <- ToWavenumer(uv, 1e9)
uv_freq["frequency"] = 1.0
uv_freq2 = ToWavenumer(uv, 1.1e9)
uv_freq2["frequency"] = 1.1
uv_freq3 = ToWavenumer(uv, 1.2e9)
uv_freq3["frequency"] = 1.2
frequencies <- data.frame(rbind(as.matrix(uv_freq), as.matrix(uv_freq2), as.matrix(uv_freq3)))
frequencies[, "frequency"] <- as.factor(frequencies[, "frequency"])

uvrot0 <- Rotate(ToWavenumer(uv, 1e9), -pi/30)
uvrot1 <- Rotate(ToWavenumer(uv, 1e9), -2*pi/30)
uv["timestep"]<- 0.0
uvrot0["timestep"] <- 1.0
uvrot1["timestep"] <- 2.0
timesteps <- data.frame(rbind(as.matrix(ToWavenumer(uv, 1e9)), as.matrix(uvrot0), as.matrix(uvrot1)))
timesteps[, "timestep"] <- as.factor(timesteps[, "timestep"])

minVal <- min(frequencies[,"u"], timesteps[,"u"], frequencies[,"v"], timesteps[,"v"])
maxVal <- max(frequencies[,"u"], timesteps[,"u"],frequencies[,"v"], timesteps[,"v"])


png("./uvPlots/plots/snapshot.png",
    width = 6.0,
    height = 6.0,
    units = "in",
    res = 200)
ggplot(data = ToWavenumer(uv, 1e9), mapping = aes(x = u, y = v)) + 
  geom_point(alpha = 0.4) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", scale=1e-3), limits=c(minVal, maxVal)) +
  scale_x_continuous(labels = scales::unit_format(unit = "k", scale=1e-3), limits=c(minVal, maxVal))
dev.off()

png("./uvPlots/plots/frequencies.png",
    width = 7.0,
    height = 6.0,
    units = "in",
    res = 200)
ggplot(data = frequencies, mapping = aes(x = u, y = v, color= frequency)) +
  geom_point(alpha = 0.4) + 
  scale_y_continuous(labels = scales::unit_format(unit = "k", scale=1e-3), limits=c(minVal, maxVal)) +
  scale_x_continuous(labels = scales::unit_format(unit = "k", scale=1e-3), limits=c(minVal, maxVal))
dev.off()


png("./uvPlots/plots/timesteps.png",
    width = 7.0,
    height = 6.0,
    units = "in",
    res = 200)
ggplot(data = timesteps, mapping = aes(x = u, y = v, color=timestep)) +
  geom_point(alpha = 0.4) +
  scale_y_continuous(labels = scales::unit_format(unit = "k", scale=1e-3), limits=c(minVal, maxVal)) +
  scale_x_continuous(labels = scales::unit_format(unit = "k", scale=1e-3), limits=c(minVal, maxVal))
dev.off()


ants <- read.table("uvPlots/ants_hand.csv", sep=";", dec=".", header=TRUE)
minVal <- min(ants$x, ants$y)
maxVal <- max(ants$x, ants$y)
png("./uvPlots/plots/ants.png",
    width = 7.0,
    height = 6.0,
    units = "in",
    res = 200)
ggplot(data = ants, mapping = aes(x = x, y = y)) + geom_point(alpha = 0.4) + xlim(minVal, maxVal) + ylim(minVal, maxVal) + xlab("X") + ylab("Y")
dev.off()
