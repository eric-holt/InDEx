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

# Temp variable updates for gene type choice dynamic UI----
gene_types_choices = function(filtered_genes){
  1:length(.gene_types) |> sapply(function(i) {
    num_genes = sum(filtered_genes %in% .genes_by_type[[i]])
    sprintf("%s (%d)", .gene_types[i], num_genes)
  }) |> unname()
}

update_gt_choices = function(filtered_genes){
  .temp$gene_types_choices <<- gene_types_choices(filtered_genes)
}

update_gt_selected = function(gene_types){
  index = selected_index(gene_types)
  .temp$gene_types_selected <<- .temp$gene_types_choices[index]
}

update_gene_types = function(filtered_features){
  cat("Updating gene types...\n")
  filtered_genes = feature_to_gene(filtered_features)
  if(is.null(.temp$gene_types_choices)) update_gt_choices(filtered_genes)
  index = selected_index(.temp$gene_types_selected)
  update_gt_choices(filtered_genes)
  update_gt_selected(index)
}

selected_index = function(gene_types){
  which(.temp$gene_types_choices %in% gene_types)
}
#----

