compute_dz <- function(r1, r2, n1, n2) {
  
  if (!is.na(r1) && !is.na(r2) && abs(r1) < 1 && abs(r2) < 1) {
    
    #Fisher Z-transformation
    z1 <- atanh(r1)
    z2 <- atanh(r2)
    
    #dz
    dz <- (z1 - z2) / sqrt((1 / (n1 - 3)) + (1 / (n2 - 3))) #for pearson is 1, spearman 1.06
    
    #two-tailed p-value
    pval_dz <- 2 * (1 - pnorm(abs(dz)))
    
    return(c(dz, pval_dz))
  } else {
    return(c(NA, NA))  
  }
}
