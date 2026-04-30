rm(list=ls())

library(ggplot2)

path <- "~/Documents/projects/lunar_regolith/analysis"
setwd(path)

data <- read.table(paste0(path,"/data/Tab17.txt"), sep = "\t", quote = "", header = T, row.names = 1)
data <- log2(data+1)

data <- na.omit(data)
data <- data[,5:8] #control
#data <- data[,1:4]

############################
#shapiro null hypothesis = data comes from a normal distribution
#shapiro_p <- apply(data, 1, function(x) shapiro.test(x)$p.value)

shapiro_p <- apply(data, 1, function(x) {
  if(length(unique(x)) > 1) {
    return(shapiro.test(x)$p.value)
  } else {
    return(NA)  
  }
})

shapiro_p_adjusted <- p.adjust(shapiro_p, method = "fdr")

png("~/Documents/projects/lunar_regolith/analysis/res_shapiro/shapiro_control.png",
    width = 6.12, height = 4.19, units = "in", res = 300)

p <- hist(shapiro_p_adjusted, breaks = 50,
     main = "Shapiro-Wilk test (JSC-1A)",
     xlab = "adjusted p-value",
     col = "skyblue", border = "white")

abline(v = 0.05, col = "red", lwd = 2, lty = 2)
dev.off()

############################
p_values <- apply(data, 1, function(gene) shapiro.test(gene)$p.value)

normale <- subset(p_values, p_values > 0.05)
non_normale <- subset(p_values, p_values < 0.05)

x <- apply(data, 1, mean)
p_values <- shapiro.test(x)$p.value

qqnorm(x)
qqline(x, col = "red")

hist(x)

