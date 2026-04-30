get_correlation_class <- function(r1, p1, r2, p2, significance_level) {
  cor_class_case <- ifelse(p1 >= significance_level, "0", ifelse(r1 > 0, "+", ifelse(r1 < 0, "-", "0")))
  cor_class_control <- ifelse(p2 >= significance_level, "0", ifelse(r2 > 0, "+", ifelse(r2 < 0, "-", "0")))
  paste(cor_class_case, cor_class_control, sep="/")
  
}