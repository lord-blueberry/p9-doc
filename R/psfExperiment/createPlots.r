library(ggplot2)

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
table["experimentName"] = "FullPsf"
table["psfSize"] = 1

combined <- readExperimet("psfExperiment/changeLipschitz", "LipschitzApprox", table)
combined <- readExperimet("psfExperiment/mine", "Combined", combined)


ggplot(data = combined, mapping = aes(x = cycle, y = objective, color=experimentName)) + geom_path() + geom_point() + scale_y_continuous() + coord_cartesian(ylim=c(30, 40))
