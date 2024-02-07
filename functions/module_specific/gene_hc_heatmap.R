# Replace 0 for log transform
replace_0 = function(m){
  m_min = min(m[m > 0])
  m[m == 0] = m_min / 10
  m
}

# LRT-significant gene-sample heatmap
gene_hc_count = function(dds){
  if(nrow(dds)*ncol(dds) == 0) stop("No data")
  rlog(counts(dds)) |> 
    heatmaply(showticklabels = c(T, F), main = "Regularized log count", column_text_angle = 90)
}

gene_hc_norm = function(dds){
  if(nrow(dds)*ncol(dds) == 0) stop("No data")
  counts(dds, normalized = T) %>% replace_0 %>% log |> 
    heatmaply(showticklabels = c(T, F), main = "Log normarized count", column_text_angle = 90)
}

gene_hc_tpm = function(dds){
  if(nrow(dds)*ncol(dds) == 0) stop("No data")
  .tm[rownames(dds), colnames(dds)] %>% replace_0 %>% log |> 
    heatmaply(showticklabels = c(T, F), main = "Log TPM", column_text_angle = 90)
}