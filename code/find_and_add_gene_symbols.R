library(biomaRt)

setwd("~/Documents/papers/lunar_regolith/analysis/res_DGCA")

data <- read.table("~/Documents/papers/lunar_regolith/analysis/res_DGCA/tab_combined_macroclasses.txt", sep = "\t", header = T, quote = "", check.names = F)
# Connessione a BioMart per Arabidopsis
ensembl <- useMart("plants_mart", 
                   dataset = "athaliana_eg_gene",
                   host = "plants.ensembl.org")

# Ottenere tutti gli ID unici da source e target
all_ids <- unique(c(data$source, data$target))

# Recuperare i mappaggi
gene_map <- getBM(attributes = c("ensembl_gene_id", "external_gene_name"),
                  filters = "ensembl_gene_id",
                  values = all_ids,
                  mart = ensembl)

# Rimuovere righe con gene_name vuoto e creare un dizionario
gene_map <- gene_map[gene_map$external_gene_name != "", ]
id_to_symbol <- setNames(gene_map$external_gene_name, gene_map$ensembl_gene_id)

# Funzione di lookup che mantiene l'ID originale se il symbol non è trovato
convert_id <- function(id) {
  symbol <- id_to_symbol[id]
  return(ifelse(is.na(symbol), id, symbol))
}

# Applicare la conversione mantenendo l'ordine
data$source_symbol <- sapply(data$source, convert_id, USE.NAMES = FALSE)
data$target_symbol <- sapply(data$target, convert_id, USE.NAMES = FALSE)

# Visualizzare i risultati
head(data)
str(data)

# Opzionale: salvare in file CSV
write.table(data, "~/Documents/papers/lunar_regolith/analysis/res_DGCA/tabs_with_symbols/tab_combined_with_symbols.txt", row.names = FALSE, col.names = T, sep = "\t", quote = F)

