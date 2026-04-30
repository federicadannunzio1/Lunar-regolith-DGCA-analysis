rm(list=ls())

library(psych)
library(stats)

path <- "~/Documents/papers/lunar_regolith/analysis"
setwd(path)

source(paste0(path, "/code/get_correlation_class.R"))
source(paste0(path, "/code/compute_dz.R"))

############################# 
#case
data <- read.table(paste0(path,"/data/Tab_combined.txt"), sep = "\t", quote = "", header = T, row.names = 1)
#data <- log2(data+1) #con tab_combined commenta questa linea

df <- data[, 1:4] #tab12 in tab combined

element <- row.names(df)

N <- length(element)

corr <- matrix(0,N,N)
pval <- matrix(0,N,N)
pval_adj <- matrix(0,N,N)

colnames(corr) <- element
row.names(corr) <- element

colnames(pval) <- element
row.names(pval) <- element

colnames(pval_adj) <- element
row.names(pval_adj) <- element

#compute correlation
for(i in 1:(N-1)){
  
  for(j in (i+1):N){
    
  x <- as.numeric(df[element[i],])
  y <- as.numeric(df[element[j],])
    
  res <- corr.test(x, y, use = "pairwise", method = "pearson", adjust = "fdr")
  
  corr[i,j] = res$r
  corr[j,i] =  corr[i,j] 
  
  pval[i,j] = res$p
  pval[j,i] =  pval[i,j]
  
  pval_adj[i,j] = res$p.adj
  pval_adj[j,i] =  pval_adj[i,j]
  
  }

}

#network building
ut <- upper.tri(corr)

source <- rownames(corr)[row(corr)[ut]]
target <- rownames(corr)[col(corr)[ut]]

net <- data.frame(
  source = source,
  target = target,
  cor  = corr[ut],
  pvalue = pval[ut],
  pval_adj = pval_adj[ut]
)

#####################################
#control
df1 <- data[, 5:8] #tab17 in tab combined

element1 <- row.names(df1)

N <- length(element1)

corr1 <- matrix(0,N,N)
pval1 <- matrix(0,N,N)
pval_adj1 <- matrix(0,N,N)

colnames(corr1) <- element1
row.names(corr1) <- element1

colnames(pval1) <- element1
row.names(pval1) <- element1

colnames(pval_adj1) <- element1
row.names(pval_adj1) <- element1

#compute correlation
for(i in 1:(N-1)){
  
  for(j in (i+1):N){
    
    x <- as.numeric(df1[element1[i],])
    y <- as.numeric(df1[element1[j],])
    
    res1 <- corr.test(x, y, use = "pairwise", method = "pearson", adjust = "fdr")
    
    corr1[i,j] = res1$r
    corr1[j,i] =  corr1[i,j] 
    
    pval1[i,j] = res1$p
    pval1[j,i] =  pval1[i,j]
    
    pval_adj1[i,j] = res1$p.adj
    pval_adj1[j,i] =  pval_adj1[i,j]
    
  }
  
}

#network building
ut1 <- upper.tri(corr1)

source1 <- rownames(corr1)[row(corr1)[ut1]]
target1 <- rownames(corr1)[col(corr1)[ut1]]

net1 <- data.frame(
  source = source1,
  target = target1,
  cor  = corr1[ut1],
  pvalue = pval1[ut1],
  pval_adj = pval_adj1[ut1]
)

##############################
# write.table(net, paste0(path, "/res_DGCA/case12_corr_by_hand.txt"), sep = "\t", row.names = F, col.names = T, quote = F)
# write.table(net1, paste0(path, "/res_DGCA/control12_corr_by_hand.txt"), sep = "\t", row.names = F, col.names = T, quote = F)

##############################
#merging

# net <- read.table(paste0(path, "/res_DGCA/case12_corr_by_hand.txt"), sep = "\t", quote = "", header = T)
# net1 <- read.table(paste0(path, "/res_DGCA/control12_corr_by_hand.txt"), sep = "\t", quote = "", header = T)

df_res <- merge(net, net1, by = c("source","target"), all = T)

##############################
#calculate correlation classes
r1 <- df_res$cor.x
r2 <- df_res$cor.y

r1[is.na(r1)] <- 0
r2[is.na(r2)] <- 0

p1 <- df_res$pval_adj.x
p2 <- df_res$pval_adj.y

p1[is.na(p1)] <- 1
p2[is.na(p2)] <- 1

significance_level <- 0.05

df_res$Class <- with(df_res, get_correlation_class(r1, p1, r2, p2, significance_level))
df_res <- subset(df_res, Class != "0/0" ) 

##############################
#compute dz
n1 <- 4  
n2 <- 4  

df_res[, c("dz", "pval_dz")] <- t(mapply(compute_dz, df_res$cor.x, df_res$cor.y, MoreArgs = list(n1 = n1, n2 = n2)))

df_res$pval_dz_adj <- p.adjust(df_res$pval_dz, method = "fdr") #adjust dz pvals

##############################
write.table(df_res, paste0(path, "/res_DGCA/df_res_combined_corr_by_hand.txt"), sep = "\t", 
            row.names = F, col.names = T, quote = F)





