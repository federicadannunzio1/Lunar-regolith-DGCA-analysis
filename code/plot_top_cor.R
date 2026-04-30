rm(list = ls())

library(DGCA)
library(ggplot2)
library(patchwork)

path <- "~/Documents/papers/lunar_regolith/analysis"
setwd(path)

tab17 <- read.table(paste0(path,"/data/Tab17.txt"), sep = "\t", quote = "", header = T, row.names = 1)
tab17 <- log2(tab17+1)

tab12 <- read.table(paste0(path,"/data/Tab12.txt"), sep = "\t", quote = "", header = T, row.names = 1)
tab12 <- log2(tab12+1)
#build model matrix
type = c(rep("case", 4), rep("control", 4))
design_mat = model.matrix(~ type + 0)
colnames(design_mat) = c("case", "control")
rownames(design_mat) <- colnames(tab17)
#design_mat = makeDesign(type)

res <- read.table(paste0(path,"/res_DGCA/df_res12_pearson_corr_by_hand.txt"), sep = "\t", quote = "", header = T)
res_ordered <- res[order(-abs(res$dz)),]

top_pairs <- res_ordered[1, c(1,2)]

plot12 <- plotCors(inputMat = tab12, design = design_mat, compare = c("case", "control"), 
         geneA = top_pairs[1,1], geneB = top_pairs[1,2])
plot12 + ggtitle("Apollo 12 regolith vs JSC-1A") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), legend.position = "none") 

res <- read.table(paste0(path,"/res_DGCA/df_res17_pearson_corr_by_hand.txt"), sep = "\t", quote = "", header = T)
res_ordered <- res[order(-abs(res$dz)),]

top_pairs <- res_ordered[2, c(1,2)]

plot17 <- plotCors(inputMat = tab17, design = design_mat, compare = c("case", "control"), 
         geneA = top_pairs[1,1], geneB = top_pairs[1,2])
plot17 + ggtitle("Apollo 17 regolith vs JSC-1A") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), legend.position = "none") 

res <- read.table(paste0(path,"/res_DGCA/df_res17_pearson_corr_by_hand.txt"), sep = "\t", quote = "", header = T)
res_ordered <- res[order(-abs(res$dz)),]

top_pairs <- res_ordered[3, c(1,2)]

plot17_2 <- plotCors(inputMat = tab17, design = design_mat, compare = c("case", "control"), 
         geneA = top_pairs[1,1], geneB = top_pairs[1,2])
plot17_2 + ggtitle("Apollo 17 regolith vs JSC-1A") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) 
########################

plot1 <- plot12 + 
  ggtitle("Apollo 12 regolith vs JSC-1A") + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(size = 0.25, color = "#ACEEFB"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),  legend.position = "none"
  ) + 
  scale_x_continuous(limits = c(0, 4)) +  # Minimo x = 0, massimo x = 4
  scale_y_continuous(limits = c(0, 8))     # Minimo y = 0, massimo y = 8

plot2 <- plot17 + 
  ggtitle("Apollo 17 regolith vs JSC-1A") + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(size = 0.25, color = "#ACEEFB"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),  legend.position = "none"
  ) + 
  scale_x_continuous(limits = c(0, 4)) +  # Minimo x = 0, massimo x = 4
  scale_y_continuous(limits = c(0, 8))     # Minimo y = 0, massimo y = 8

plot3 <- plot17_2 + 
  ggtitle("Apollo 17 regolith vs JSC-1A") + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    panel.grid.major = element_line(size = 0.25, color = "#ACEEFB"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  ) + 
  scale_x_continuous(limits = c(0, 4)) +  # Minimo x = 0, massimo x = 4
  scale_y_continuous(limits = c(0, 8))     # Minimo y = 0, massimo y = 8

combined_plot <- plot1 | plot2 | plot3  # Stack plots horizontally
print(combined_plot)


