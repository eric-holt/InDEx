# Get DESeq2 contrast results from dds
get_res = function(dds){
  tryCatch({
    pairs = cond_pairs_from_dds(dds)
    names = sapply(pairs, pair_to_contrast)
    dds_wald = nbinomWaldTest(dds)
    pairs |> lapply(function(p){
      results(dds_wald, c("condition", p[1], p[2]))
    }) |> setNames(names)
  }, error = function(e) {
    warning("get_res: ", e)
    NULL
  })
}

# data table of the results with gene name, sorted by padj
dt_result = function(res){
  tryCatch({
    dt = as.data.table(res)
    dt[, feature_id := rownames(res)]
    dt = unique(.dt_count[, .(feature_id, gene_id, gene_name)])[dt, on = "feature_id"]
    setorder(dt, pvalue)
    dt
  }, error = function(e) {
    warning("dt_result: ", e)
    NULL
  })
}

# combined data tables for visualization
dt_all_results = function(res){
  tryCatch({
    dt_all = 1:length(res) |> lapply(function(i){
      dt = res[[i]] |> dt_result()
      dt[, label := names(res)[i]]
      dt
    }) |> rbindlist()
    dt_all$label = factor(dt_all$label, levels = names(res))
    dt_all
  }, error = function(e) {
    warning("dt_all_results: ", e)
    NULL
  })
}

# LRT significant features
get_dt_lrt = function(dds, alpha){
  tryCatch({
    dds_lrt = nbinomLRT(dds, reduced = ~ 1)
    dt_lrt = results(dds_lrt) %>% na.omit %>% dt_result
    dt_lrt[, label := "LRT"]
    dt_lrt[padj < alpha]
  }, error = function(e) {
    warning("get_dt_lrt: ", e)
    NULL
  })
}