rm(list = ls())

path <- "~/Documents/papers/lunar_regolith/analysis"
setwd(path)

library(clusterProfiler)
library(org.At.tair.db)  #For Arabidopsis annotation
library(readxl)

excel_path <- paste0(path, "/res_DGCA/macroclassi12_output.xlsx")

data_excel <- read_excel(excel_path, sheet = 3) 

gene_list <- data_excel[, 1:2] 

col1 <- gene_list[,1]
col2 <- gene_list[,2]
colnames(col2) <- "source"

gene_list <- rbind(col1, col2)
gene_list <- unique(gene_list)

gene_list<- as.vector(gene_list$source)
gene_list <- c("AT5G24160", "AT5G60120", "AT1G13609", "AT3G56970") #12 vs 17 acquired
gene_list <- c("AT1G14250","AT1G26930","AT2G30766","AT1G76930","AT5G24080","AT1G12570") #12 vs 17 lost


#BP functional enrichment 
go_result <- enrichGO(
  gene = gene_list,          
  OrgDb = org.At.tair.db,    
  keyType = "TAIR",          
  ont = "BP",                #Ontology:BP, MF, CC
  pvalueCutoff = 0.1      
)

dotplot(go_result, showCategory = 10, title = "GO Biological Process")
res_GO <- as.data.frame(go_result@result)

write.table(res_GO, paste0(path, "GO_BP_changed.txt"), sep = "\t", col.names = NA, row.names = T)

#MF functional enrichment 
go_result <- enrichGO(
  gene = gene_list,          
  OrgDb = org.At.tair.db,    
  keyType = "TAIR",          
  ont = "MF",                #Ontology:BP, MF, CC
  pvalueCutoff = 0.1      
)

dotplot(go_result, showCategory = 10, title = "GO Molecular Function")

#CC functional enrichment 
go_result <- enrichGO(
  gene = gene_list,          
  OrgDb = org.At.tair.db,    
  keyType = "TAIR",          
  ont = "CC",                #Ontology:BP, MF, CC
  pvalueCutoff = 0.1      
)

dotplot(go_result, showCategory = 10, title = "GO Cellular Component")

#KEGG enrichment
kegg_result <- enrichKEGG(
  gene = gene_list,
  organism = 'ath',          
  pvalueCutoff = 0.1
)

dotplot(kegg_result, showCategory = 10, title = "KEGG Pathway")

