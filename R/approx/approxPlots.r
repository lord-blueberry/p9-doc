library(ggplot2)
library(dplyr)

imageSize <- 3072
asinh <- scales::trans_new(name = 'asinh', transform = function(x) asinh(x*1000), 
                           inverse = function(x) sinh(x)/1000)

readExperimet <- function(folder, file, experimentName, df){
  print(file)
  t = read.table(file.path(folder,file), header=TRUE, dec=",", sep=";")
  t[,"cycle"] = as.factor(t$cycle)
  t["experimentName"] = experimentName
  df <- rbind(df, t)
  
  return(df)
}

folder <- "approx/PsfSize"
file <- "PsfSize8.txt"
data = read.table(file.path(folder,file), header=TRUE, dec=",", sep=";")
data[,"cycle"] = as.factor(data$cycle)
data["experimentName"] = "psf8"
for(i in c(16,32, 64)) {
  file <- paste("PsfSize", i, ".txt", sep="")
  data2 = read.table(file.path(folder,file), header=TRUE, dec=",", sep=";")
  data2[,"cycle"] = as.factor(data2$cycle)
  data2["experimentName"] = paste("psf",i,sep="")
  data <- rbind(data, data2)
}

outputfolder <- "./approx/output/"

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
