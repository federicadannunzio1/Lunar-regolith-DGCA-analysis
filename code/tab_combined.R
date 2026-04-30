rm(list=ls())


path <- "~/Documents/papers/lunar_regolith/analysis"
setwd(path)

#case
data12 <- read.table(paste0(path,"/data/Tab12.txt"), sep = "\t", quote = "", header = T, row.names = 1)
data12 <- log2(data12+1)

data17 <- read.table(paste0(path,"/data/Tab17.txt"), sep = "\t", quote = "", header = T, row.names = 1)
data17 <- log2(data17+1)

x <- intersect(rownames(data12),rownames(data17))

data12 <- data12[x,]
data17 <- data17[x,]

data12 <- data12[,1:4]
data17 <- data17[,1:4]

tab <- cbind(data12, data17)

write.table(tab, "/tab_combined.txt", sep = "\t", col.names = NA, row.names = T, quote = F)
