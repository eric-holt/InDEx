# Extract conditions from sample names
# format: "name_condition"
#   "S01_HH", "S02_ctrl"
condition_from_sample = function(sample){
  cond = str_match(sample, "_(.+)$")[, 2]
  factor(cond, levels = unique(cond))
}

# Parse feature ID to extract elments
# valid formats:
#   "gene1" 
#   "gene2-1000:+200@1:30000-40000"
parse_feature_id = function(id){
  m = str_match(id, "^(.+?)(([+-]\\d+):([+-]\\d+)@(.+?):(\\d+)-(\\d+))?$")
  list(gene_id = m[, 2], 
       tss_dist_5 = as.numeric(m[, 4]),
       tss_dist_3 = as.numeric(m[, 5]),
       chr = m[, 6],
       start = as.numeric(m[, 7]),
       end = as.numeric(m[, 8]))
}

# Detect database from gene/term ID format
id_to_db = function(id) {
  if (str_starts(id, "ENS")) {
    return("Ensembl")
  } else if (str_starts(id, "GO:")) {
    return("GO")
  } else {
    return("RefSeq")
  }
}

