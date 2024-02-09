# Filter genes by minimum threshold in all replicates in any condition
filter_by_count = function(dt, threshold){
  dt_long = dt |> dplyr::select(feature_id, all_of(.samples)) |> 
    melt(id.vars = "feature_id", variable.name = "sample")
  dt_long[, condition := condition_from_sample(sample)]
  dt_long[, keep := all(value >= threshold), by = .(feature_id, condition)]
  dt_long[, .(keep = any(keep)), by = .(feature_id)][keep == T, feature_id]
}

# Filter genes by coefficient of variation percentile
filter_by_cv = function(dt, percentile){
  cutoff = quantile(dt$cv, percentile / 100)
  dt[cv >= cutoff, feature_id]
}

# Exclude genes by type, returning corresponding features
exclude_gene = function(features, indices){
  genes = feature_to_gene(features)
  for(index in indices){
    genes = genes |> setdiff(.genes_by_type[[index]])
  }
  gene_to_feature(genes)
}

