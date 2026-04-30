library(biomaRt)

path <- "~/Documents/papers/lunar_regolith/analysis/res_DGCA"
setwd(path)

ensembl <- useMart("plants_mart", dataset = "athaliana_eg_gene", host = "https://plants.ensembl.org", path = "/biomart/martservice")



# Example list of AT gene IDs
at_genes <- c("AT1G01010", "AT2G01020", "AT3G01150")

# Get gene symbols
gene_info <- getBM(attributes = c("tair_locus", "external_gene_name"), 
                   filters = "tair_locus", 
                   values = at_genes, 
                   mart = ensembl)

print(gene_info)
