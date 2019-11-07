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

folder <- "approx/gridTest"
file <- "cpuTest8block1.txt"
table = read.table(file.path(folder,file), header=TRUE, dec=",", sep=";")
table[,"cycle"] = as.factor(table$cycle)
table["experimentName"] = "block1"

folder2 <- "approx/searchTest"
file2 <- "Grid_cpu8block1search0.05.txt"
t = read.table(file.path(folder2,file2), header=TRUE, dec=",", sep=";")
t[,"cycle"] = as.factor(t$cycle)
t["experimentName"] = "searchFraction"
combined <- rbind(table, t)

combined = readExperimet(folder, file2, "searchFraction", table)

ggplot(data = combined, mapping = aes(x = seconds, y = objectiveNormal, linetype=cycle, color= experimentName)) + 
  xlab("time") +
  ylab("Objective value") +
  geom_line() +
  scale_y_continuous(trans= "log10")+
  scale_x_continuous(trans= "log10")+
  coord_cartesian(ylim=c(28, 280.0))
