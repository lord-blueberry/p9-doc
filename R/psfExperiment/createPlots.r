library(ggplot2)
library(dplyr)

imageSize <- 3072
asinh <- scales::trans_new(name = 'asinh', transform = function(x) asinh(x*1000), 
                           inverse = function(x) sinh(x)/1000)

readExperimet <- function(folder, experimentName, df){
  for(file in list.files(folder))
  {
    offset <- nchar(file) - nchar("Psf.txt")
    psfSize <- substr(file, 0, offset)
    print(file)
    t = read.table(file.path(folder,file), header=TRUE, dec=",", sep=";")
    t["objective"] <- t["dataPenalty"] + t["regPenalty"]
    t["psfSize"] = as.integer(psfSize)
    t["experimentName"] = experimentName
    df <- rbind(df, t)
  }
  
  return(df)
}



#-------------------------------------------------Generate plots for approximate deconvolution (simply cut the PSF in half and use this)
table = read.table("psfExperiment/1Psf.txt", header=TRUE, dec=",", sep=";")
table["objective"] <- table["dataPenalty"] + table["regPenalty"]
table["experimentName"] = "Standard"
table["psfSize"] = 1

combined <- readExperimet("psfExperiment/psfSpeed", "Approx. deconvolution", table)
#combined[, "psfSize"] <- imageSize * 1.0/as.numeric(combined[,"psfSize"])
combined["psfFraction"] <- combined["psfSize"]
combined[, "psfSize"] <- as.factor(combined[, "psfSize"])

png("./psfExperiment/plots/ApproxDeconv/size.png",
    width = 6.0,
    height = 3.0,
    units = "in",
    res = 200)
ggplot(data = combined, mapping = aes(x = cycle, y = objective, color=psfSize)) + 
  xlab("Major cycle index") +
  ylab("Objective value") +
  scale_color_discrete(name = "PSF fraction", labels = paste("1 /" ,levels(combined$psfSize))) +
  geom_line() + 
  geom_point() + 
  scale_y_continuous(trans= "log10")
dev.off()

timeGroup <- group_by(combined, psfFraction)
agg <- summarize(timeGroup, time=sum(ElapsedTime), iter= sum(iterCount))
agg["timePerIter"] <- agg$time / as.numeric(agg$iter)
agg["speedup"] <- agg$timePerIter[agg$psfFraction == 1] / agg$timePerIter
agg["speedup_total"] <- agg$time[agg$psfFraction == 1] / agg$time

png("./psfExperiment/plots/ApproxDeconv/speedup_iter.png",
    width = 3.0,
    height = 3.0,
    units = "in",
    res = 200)
ggplot(data = agg, aes(x = psfFraction, y = speedup)) + 
  xlab("PSF Fraction") +
  ylab("Per iteration Speedup") +
  geom_line() + geom_point() +
  scale_x_continuous(trans="log2")
dev.off()

png("./psfExperiment/plots/ApproxDeconv/speedup_total.png",
    width = 3.0,
    height = 3.0,
    units = "in",
    res = 200)
ggplot(data = agg, aes(x = psfFraction, y = speedup_total)) + 
  xlab("PSF Fraction") +
  ylab("Total Speedup") +
  geom_line() + geom_point() +
  scale_x_continuous(trans="log2")
dev.off() 


#-------------------------------------------------Generate plots for approximate Update (use a fraction of the psf to update the gradient map)
table = read.table("psfExperiment/1Psf.txt", header=TRUE, dec=",", sep=";")
table["objective"] <- table["dataPenalty"] + table["regPenalty"]
table["experimentName"] = "Standard"
table["psfSize"] = 1

combined <- readExperimet("psfExperiment/psfSpeedApproxUpdate", "Approx. Update", table)
#combined[, "psfSize"] <- imageSize * 1.0/as.numeric(combined[,"psfSize"])
combined["psfFraction"] <- combined["psfSize"]
combined[, "psfSize"] <- as.factor(combined[, "psfSize"])

png("./psfExperiment/plots/ApproxUpdate/size.png",
    width = 6.0,
    height = 3.0,
    units = "in",
    res = 200)
ggplot(data = combined, mapping = aes(x = cycle, y = objective, color=psfSize)) + 
  xlab("Major cycle index") +
  ylab("Objective value") +
  scale_color_discrete(name = "PSF fraction", labels = paste("1 /" ,levels(combined$psfSize))) +
  geom_line() + 
  geom_point() + 
  scale_y_continuous(trans= "log10")
dev.off()

timeGroup <- group_by(combined, psfFraction)
agg <- summarize(timeGroup, time=sum(ElapsedTime), iter= sum(iterCount))
agg["timePerIter"] <- agg$time / as.numeric(agg$iter)
agg["speedup"] <- agg$timePerIter[agg$psfFraction == 1] / agg$timePerIter
agg["speedup_total"] <- agg$time[agg$psfFraction == 1] / agg$time

png("./psfExperiment/plots/ApproxUpdate/speedup_iter.png",
    width = 3.0,
    height = 3.0,
    units = "in",
    res = 200)
ggplot(data = agg, aes(x = psfFraction, y = speedup)) + 
  xlab("PSF Fraction") +
  ylab("Per iteration Speedup") +
  geom_line() + geom_point() +
  scale_x_continuous(trans="log2")
dev.off()

png("./psfExperiment/plots/ApproxUpdate/speedup_total.png",
    width = 3.0,
    height = 3.0,
    units = "in",
    res = 200)
ggplot(data = agg, aes(x = psfFraction, y = speedup_total)) + 
  xlab("PSF Fraction") +
  ylab("Total Speedup") +
  geom_line() + geom_point() +
  scale_x_continuous(trans="log2")
dev.off() 







table = read.table("psfExperiment/1Psf.txt", header=TRUE, dec=",", sep=";")
table["objective"] <- table["dataPenalty"] + table["regPenalty"]
table["experimentName"] = "Standard"
table["psfSize"] = 1
combined <- readExperimet("psfExperiment/psfApprox", "Approx. gradient update", table)
combined <- readExperimet("psfExperiment/convolutionApprox", "Approx. deconvolution", combined)
combined <- readExperimet("psfExperiment/combined", "Combination", combined)
subset <- subset(combined, psfSize %in% c(1,16))


t = read.table(file.path("psfExperiment/psfApprox","16Psf.txt"), header=TRUE, dec=",", sep=";")

png("./psfExperiment/plots/comparison.png",
    width = 9.0,
    height = 3.0,
    units = "in",
    res = 200)
ggplot(subset, mapping = aes(x = cycle, y = objective, color=experimentName)) + 
  xlab("Major cycle index") +
  ylab("Objective value") +
  labs(color="Method") +
  geom_line() + 
  geom_point() + 
  scale_y_continuous(trans="log10")
dev.off() 

png("./psfExperiment/plots/comparison_zoom.png",
    width = 9.0,
    height = 3.0,
    units = "in",
    res = 200)
ggplot(subset, mapping = aes(x = cycle, y = objective, color=experimentName)) + 
  xlab("Major cycle index") +
  ylab("Objective value") +
  labs(color="Method") +
  geom_line() + 
  geom_point() + 
  scale_y_continuous() +
  coord_cartesian(xlim= c(0, 5), ylim=c(93.75, 94.25))
dev.off()