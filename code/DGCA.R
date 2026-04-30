rm(list = ls())

library(DGCA)
library(fdrtool)

path <- "~/Documents/papers/lunar_regolith/analysis"
setwd(path)

tab17 <- read.table(paste0(path,"/data/tab17.txt"), sep = "\t", quote = "", header = T, row.names = 1)
tab17 <- log2(tab17+1)

#build model matrix
type = c(rep("case", 4), rep("control", 4))
design_mat = model.matrix(~ type + 0)
colnames(design_mat) = c("case", "control")
rownames(design_mat) <- colnames(tab17)
#write.table(design_mat, paste0(path, "/design_mat17.txt"), sep = "\t", row.names = T, col.names = T, quote = F)
#design_mat = makeDesign(type)

ddcor_res = ddcorAll(inputMat = tab17, design = design_mat,
                     adjust = "fdr", nPerms = 0,
                     compare = c("case", "control"), corrType = "pearson",
                     sigThresh = 1, corSigThresh = 0.05,
                     heatmapPlot = T, corPower = 2)


if (!dir.exists(paste0(path, "/res_DGCA"))) {
  dir.create(paste0(path, "/res_DGCA"))} else {
    print("the dir already exists")
  }

dir_output <- paste0(path, "/res_DGCA")
write.table(ddcor_res, paste0(dir_output, "/res_pacchetto_pearson.txt"), sep = "\t", col.names = T, quote = F, row.names = F)

######################
#code step by step

library(matrixStats)
library(ggplot2)
library(impute)
library(gplots)
library(fdrtool)
library(GOstats)
library(HGNChelper)
library(org.Hs.eg.db)


# Step 2: Filter genes by median expression and variance
# filtered_data <- filterGenes(darmanis, filterTypes = c("central", "dispersion"), 
#                              filterCentralType = "median", filterCentralPercentile = 0.3,
#                              filterDispersionType = "cv", filterDispersionPercentile = 0.3)

# Step 3: Compute correlations per condition
cor_results <- getCors(inputMat = tab17, design = design_mat)

# Step 4: Compute differential correlation
dc_pairs <- pairwiseDCor(cor_results, compare = c("case", "control"))

# Step 5: Extract top differentially correlated pairs
top_pairs <- dcTopPairs(dc_pairs, nPairs = 100, classify = TRUE)
print(head(top_pairs))

# Step 6: Plot expression of selected gene pairs
plotCors(inputMat = tab12, design = design_mat, compare = c("case", "control"), 
         geneA = top_pairs[1,1], geneB = top_pairs[1,2])

# Step 7: Adjust p-values using Benjamini-Hochberg correction
top_pairs_adjusted <- dcTopPairs(dc_pairs, nPairs = 100, classify = TRUE, adjust = "BH")
print(head(top_pairs_adjusted))

# Step 8: Spearman-based correlation analysis
ddcor_res_spearman <- ddcorAll(inputMat = filtered_data, design = design_mat, compare = c("oligodendrocyte", "neuron"),
                               adjust = "none", heatmapPlot = FALSE, nPerm = 0, corrType = "spearman", nPairs = 100)
print(head(ddcor_res_spearman))

# Step 9: Log transformation
log_data <- log(filtered_data + 1)
ddcor_res_log <- ddcorAll(inputMat = log_data, design = design_mat, compare = c("oligodendrocyte", "neuron"),
                          adjust = "none", heatmapPlot = FALSE, nPerm = 0, splitSet = "RTN4", nPairs = 525)

# Step 10: Impute missing values
darmanis_na <- filtered_data
darmanis_na[1, 1] <- NA  # Introduce an NA for demonstration
imputed_data <- impute.knn(as.matrix(darmanis_na))$data

# Step 11: Classify differential correlations
classified_pairs <- dcTopPairs(dc_pairs, nPairs = 100, classify = TRUE)
print(head(classified_pairs))


# Step 13: Heatmap visualization
ddcor_res_heatmap <- ddcorAll(inputMat = filtered_data, design = design_mat, compare = c("oligodendrocyte", "neuron"),
                              adjust = "none", heatmapPlot = TRUE, nPerm = 0, nPairs = "all")

# Step 14: Compute average differential correlation
ddcor_res_avg <- ddcorAll(inputMat = filtered_data, design = design_mat, compare = c("oligodendrocyte", "neuron"),
                          adjust = "perm", heatmapPlot = FALSE, nPerm = 20, getDCorAvg = TRUE,
                          dCorAvgType = "gene_average", dCorAvgMethod = "median")
print(head(ddcor_res_avg$avg_dcor))

# Step 15: Gene Ontology enrichment analysis
ddcorGO_res <- ddcorGO(ddcor_res_trait, universe = rownames(filtered_data), gene_ontology = "all",
                       HGNC_clean = TRUE, HGNC_switch = TRUE, annotation = "org.Hs.eg.db")
print(str(ddcorGO_res))



