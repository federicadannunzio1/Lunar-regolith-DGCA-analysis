rm(list = ls())

path <- "~/Documents/papers/lunar_regolith/analysis"
setwd(path)

tab <- read.table(paste0(path, "/DEGs_sheet.txt"), header = T , quote = "", check.names = F, sep = "\t", fill = T)

tab11 <- tab[,c(1,3)]
tab11 <- na.omit(tab11)

tab12 <- tab[,c(1,4)]
tab12 <- na.omit(tab12)

tab17 <- tab[,c(1,5)]
tab17 <- na.omit(tab17)

complete_tab <- read.table(paste0(path, "/GSE188852_Ferl_Paul_Lunar-DiffExp-by-Apollo-Site_20211030.txt"), header = T , quote = "", check.names = F, sep = "\t", fill = T)

process_tab <- function(tab, complete_tab, pattern_L) {
  
  #get the intersection between names in the complete mat and names in tab
  intersection_tab <- intersect(complete_tab$Model_name, tab$Model_name)
  
  #keep just lines in complete tab in which names of tab are present
  complete_tab_subset <- complete_tab[complete_tab$Model_name %in% intersection_tab, ]
  
  first_col <- 1
  P_L_cols <- grep(paste0("^P[0-9]+", pattern_L), colnames(complete_tab_subset)) #case
  JSC1_cols <- grep("JSC1$", colnames(complete_tab_subset)) #control
  cols_to_keep <- c(first_col, P_L_cols, JSC1_cols)
  
  complete_tab_subset <- complete_tab_subset[, cols_to_keep]
  
  return(complete_tab_subset)
}

complete_tab11 <- process_tab(tab11, complete_tab, "L1")
complete_tab12 <- process_tab(tab12, complete_tab, "L2")
complete_tab17 <- process_tab(tab17, complete_tab, "L3")


if (!dir.exists(paste0(path, "/data"))) {
  dir.create(paste0(path, "/data"))
} else {
  print("The directory already exists")
}

output_dir <- paste0(path,"/data")
write.table(complete_tab11, paste0(output_dir, "/Tab11.txt"), col.names = T, row.names = F, sep = "\t", quote = F)
write.table(complete_tab12, paste0(output_dir, "/Tab12.txt"), col.names = T, row.names = F, sep = "\t", quote = F)
write.table(complete_tab17, paste0(output_dir, "/Tab17.txt"), col.names = T, row.names = F, sep = "\t", quote = F)

