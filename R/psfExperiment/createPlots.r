library(ggplot2)

imageSize <- 4096
asinh <- scales::trans_new(name = 'asinh', transform = function(x) asinh(x*1000), 
                           inverse = function(x) sinh(x)/1000)

readExperimet <- function(folder, experimentName, df){
  for(file in list.files(folder))
  {
    offset <- nchar(file) - nchar("Psf.txt")
    psfSize <- substr(file, 0, offset)
    
    t = read.table(file.path(folder,file), header=TRUE, dec=",", sep=";")
    t["objective"] <- t["dataPenalty"] + t["regPenalty"]
    t["experimentName"] = experimentName
    t["psfSize"] = as.integer(psfSize)
    df <- rbind(df, t)
  }
  
  return(df)
}

table = read.table("psfExperiment/1Psf.txt", header=TRUE, dec=",", sep=";")
table["objective"] <- table["dataPenalty"] + table["regPenalty"]
table["experimentName"] = "Standard"
table["psfSize"] = 1

combined <- readExperimet("psfExperiment/convolutionApprox", "Approx. deconvolution", table)
combined[, "psfSize"] <- imageSize * 1.0/as.numeric(combined[,"psfSize"])
combined[, "psfSize"] <- as.factor(combined[, "psfSize"])

png("./psfExperiment/plots/size.png",
    width = 12.0,
    height = 6.0,
    units = "in",
    res = 200)
ggplot(data = combined, mapping = aes(x = cycle, y = objective, color=psfSize)) + 
  xlab("Major cycle index") +
  ylab("Objective value") +
  labs(color="PSF size in pixels") +
  geom_line() + 
  geom_point() + 
  scale_y_continuous(trans= "log10")
dev.off() 

table = read.table("psfExperiment/1Psf.txt", header=TRUE, dec=",", sep=";")
table["objective"] <- table["dataPenalty"] + table["regPenalty"]
table["experimentName"] = "Standard"
table["psfSize"] = 1
combined <- readExperimet("psfExperiment/psfApprox", "Approx. gradient update", combined)
combined <- readExperimet("psfExperiment/convolutionApprox", "Approx. deconvolution", combined)
combined <- readExperimet("psfExperiment/combined", "Combination", combined)
subset <- subset(combined, psfSize %in% c(1,32))

png("./psfExperiment/plots/comparison.png",
    width = 12.0,
    height = 6.0,
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



+ coord_cartesian(ylim=c(30, 40))