# Process GTF
get_gtf = function(path){
  cat("Processing GTF...\n")
  gtf = as.data.table(rtracklayer::import(path, format = "gtf"))
  if(length(unique(gtf[, type])) > 1){ # Not simplified
    gtf = gtf[type =="gene"]
  }
  if("gene_name" %in% colnames(gtf)){ # Ensembl or simplified
    gtf = gtf[, .(gene_id, gene_name, seqnames, gene_biotype, source)]
  } else if("gene" %in% colnames(gtf)){ # RefSeq
    gtf = gtf[, .(gene_id, gene_name = gene, seqnames, gene_biotype, source)]
  }
  gtf[is.na(gene_name), gene_name := gene_id]
  cat("Done processing GTF\n")
  gtf
}

# Calculate coefficient of variation
calc_cv = function(matrix){
  apply(matrix, 1, function(row) sd(row)/mean(row))
}

# Count/TPM matrix into data table with additional info
matrix_to_dt = function(matrix, gtf){
  cv = calc_cv(matrix)
  dt = data.table(feature_id = rownames(matrix), matrix)
  suppressWarnings(add_positions(dt))
  
  dt = merge.data.table(dt, gtf, by = "gene_id", all.x = T, all.y = F)
  dt[, cv := cv]
  dt
}

# Add genomic positions to data table by parsing the feature ID
add_positions = function(dt){
  f = parse_feature_id(dt$feature_id)
  dt[, gene_id := f$gene_id]
  dt[, start := f$start]
  dt[, end := f$end]
}

# Make all project directories
make_all_dir = function(project){
  list(dir_cache, dir_gg, dir_plotly, dir_export, dir_import) |> lapply(function(dir){
    dir.create(dir(project), recursive = T, showWarnings = F)
  })
}

# Save the imported data
import_data = function(data, name, project){
  path = here(dir_import(project), paste0(name, ".rds"))
  saveRDS(data, path)
  cat("\tSaved data", name, "\n")
}

# Save the project metadata
save_project_metadata = function(project = .project){
  path = here(dir_project(project), "metadata.rds")
  if(check_path(path)){
    saveRDS(.metadata, path)
  }
}

# Read the project metadata
read_project_metadata = function(project = .project){
  path = here(dir_project(project), "metadata.rds")
  if(check_path(path)){
    .metadata <<- readRDS(path)
  } else {
    .metadata <<- list(Name = project, GTF = "", Count = "", TPM = "", org.db = "org.Mm.eg.db")
  }
}