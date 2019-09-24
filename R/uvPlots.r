
library(ggplot2)
gText <- c("1.0 Ghz", "1.2 Ghz")
ghz <- factor(gText)
uv <- read.csv("uv_meerkat.txt", sep=";", dec=",")
uv_freq <- uv
uv_freq["frequency"] = 1.0
uv_freq2 = uv * 1.1
uv_freq2["frequency"] = 1.1
combined <- data.frame(rbind(as.matrix(uv_freq), as.matrix(uv_freq2)))

ggplot(data = combined, mapping = aes(x = u, y = v, color= frequency)) + geom_point(alpha = 0.2)
