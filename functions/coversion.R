# Gene <-> feature conversions, using .dt_count
gene_to_feature = function(genes){
  .dt_count[gene_id %in% genes, feature_id]
}

feature_to_gene = function(features){
  unique(.dt_count[feature_id %in% features, gene_id])
}
