rm(list = ls())
library(biomaRt)

path <- "~/Documents/papers/lunar_regolith/analysis/GO_res12"
setwd(path)

BP_table <- read.table(paste0(path, "/GO_BP_changed.txt"), sep = "\t", header = T, row.names = 1, check.names = F)
leaf_dev <- BP_table[which(BP_table[,2] %in%  "leaf development"),]

leaf_dev_genes <- leaf_dev$geneID
leaf_dev_genes <- strsplit(leaf_dev_genes, "/")

ensembl <- useMart("plants_mart", dataset = "athaliana_eg_gene", host = "https://plants.ensembl.org")
# gene_list <- c("AT5G24160", "AT5G60120", "AT1G13609", "AT3G56970") #12 vs 17 acquired
# gene_list <- c("AT1G14250","AT1G26930","AT2G30766","AT1G76930","AT5G24080","AT1G12570") #12 vs 17 lost

gene_info <- getBM(attributes = c("ensembl_gene_id", "external_gene_name"),
                   filters = "ensembl_gene_id",
                   values = gene_list,
                   mart = ensembl)

write.table(gene_info, "specific_lost_genes.txt", sep = "\t", col.names = T, row.names = F)
