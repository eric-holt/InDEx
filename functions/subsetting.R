# Significant features
get_dt_sig = function(dt_res, a, lfc){
  tryCatch({
    dt_res[padj < a & abs(log2FoldChange) >= lfc]
  }, error = function(e) NULL)
}