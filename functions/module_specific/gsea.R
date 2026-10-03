# GO enrichment
get_enrich_go = function(dt, universe, p = .05, q = .2, minGSSize = 10, maxGSSize = 500){
  db = id_to_db(dt$gene_id[1]) |> toupper()
  cat("Detected DB:", db, "\n")
  get_enrich_go = function(dt_l){
    c("BP", "CC", "MF") |> lapply(function(ont){
      enrichGO(unique(na.omit(dt_l$gene_id)), .org, db, ont, p, "BH",
               universe, q, minGSSize, maxGSSize)
    }) |> setNames(c("BP", "CC", "MF"))
  }

  labels = levels(dt[, label]) 
  labels |> lapply(function(l) {
      dt[label == l]
    }) |> setNames(labels) |> lapply(get_enrich_go)
}

# Top n-term data.table
get_dt_top_n_go = function(enrich, n = 10, sort = "p-value", p = .05, q =.2){
  if(is.null(enrich)) return(NULL)
  dt = as.data.table(enrich)[pvalue < p & qvalue < q]
  if(!nrow(dt)) return()
  if("BgRatio" %in% names(dt)){ # from enrichGO()
    dt[, setSize := as.numeric((str_match(BgRatio, "(\\d+)/"))[, 2])]
    dt[, GeneRatio := GeneRatio |> sapply(function(x)eval(parse(text = x)))]
  } else{ # from gseGO()
    dt[, Count := sapply(str_split(core_enrichment, "/"), length)]
    dt[, GeneRatio := Count / setSize]
    dt[, sign := ifelse(NES > 0, "activated", "suppressed")]
  }
  if(sort == "p-value") setorder(dt, pvalue, -Count, setSize)
  if(sort == "gene count") setorder(dt, -Count, setSize, pvalue)
  dt = head(dt, n)
}


# Gene Set Enrichment Analysis
get_gsea_go = function(dt, p = .05, minGSSize = 10, maxGSSize = 500){
  db = id_to_db(dt$gene_id[1]) |> toupper()
  cat("Detected DB:", db, "\n")
  # Using signed -log(p) as the ranking criterion
  gene_list = (-log10(dt$pvalue) * sign(dt$log2FoldChange)) |> setNames(dt$gene_id) |> sort(T)
  c("BP", "CC", "MF") |> lapply(function(ont){
    gseGO(gene_list, ont, .org, db, 1, minGSSize, maxGSSize, 0, p)
  }) |> setNames(c("BP", "CC", "MF"))
}

get_all_gsea = function(dt, p = .05, minGSSize = 10, maxGSSize = 500){
  labels = levels(dt$label)
  labels |> 
    lapply(function(l){dt[label == l]}) |> 
    setNames(labels) |> 
    lapply(get_gsea_go, p, minGSSize, maxGSSize)
}
