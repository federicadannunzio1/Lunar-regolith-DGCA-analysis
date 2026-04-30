rm(list = ls())

path <- "~/Documents/papers/lunar_regolith/analysis"
setwd(path)


library(openxlsx)
library(dplyr)

tab <- read.table(paste0(path, "/res_DGCA/df_res_combined_corr_by_hand.txt"), 
                  header = TRUE, sep = "\t", quote = "")

tab <- tab %>%
  mutate(Class = case_when(
    Class %in% c("+/0", "-/0") ~ "lost",
    Class %in% c("0/+", "0/-") ~ "acquired",
    Class %in% c("+/+", "-/-") ~ "conserved",
    Class %in% c("+/-", "-/+") ~ "changed" 
  ))

dir_output <- paste0(path, "/res_DGCA")
write.table(tab, paste0(dir_output, "/tab_combined_macroclasses.txt"), sep = "\t", col.names = T, row.names = F, quote = F)

output_file <- "~/Documents/papers/lunar_regolith/analysis/res_DGCA/macroclassi_combined_output.xlsx"

wb <- createWorkbook()

classi <- unique(tab$Class)

for (classe in classi) {
  
  tab_filt <- tab[tab$Class == classe, c("source", "target", "Class") ]
  
  addWorksheet(wb, classe)

  writeData(wb, classe, tab_filt)
}

saveWorkbook(wb, output_file, overwrite = T)

