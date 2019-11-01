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

folder <- "approx"
file <- "approxConvergence.txt"
table = read.table(file.path(folder,file), header=TRUE, dec=",", sep=";")
table[,"cycle"] = as.factor(table$cycle)
table["experimentName"] = "standard"
combined = readExperimet(folder, "approxConvergence2cores.txt", "2cores", table)

ggplot(data = combined, mapping = aes(x = seconds, y = objectiveNormal, shape=cycle, color= experimentName)) + 
  xlab("time") +
  ylab("Objective value") +
  geom_line() + 
  geom_point()  
  #scale_y_continuous(trans= "log10")