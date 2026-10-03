# Hyperlink gene IDs to Ensembl or RefSeq
hyperlink_gene_name = function(dt){
  copy(dt)[, gene_name := Map(function(id, text) {
    hyperlink(id_to_url(id), text)
  }, gene_id, gene_name)]
}

# Hyperlink GO term IDs to amiGO
hyperlink_go_term = function(dt){
  copy(dt)[, Description := Map(function(id, text){
    hyperlink(id_to_url(id), text)
  }, ID, Description)]
}

# Datatables to display under the "Count matrix" tab group
dt_display = function(dt, samples, features, digits = 2){
  dt = dt[feature_id %in% features, .SD, .SDcols = c("gene_id", "gene_name", samples)]
  dt |> hyperlink_gene_name() |> dplyr::select(gene_name, all_of(samples)) |>
    datatable(escape = F, rownames = F, filter = "top",
              selection = "none",
              options = list(search = list(regex = T, smart = T))) |> 
    formatRound(2:(length(samples) + 1), digits = digits)
}
