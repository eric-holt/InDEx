# Generate URL to look up gene/term ID
id_to_url = function(id){
  db = id_to_db(id)
  if(db == "RefSeq"){
    sprintf("https://www.ncbi.nlm.nih.gov/gene/?term=%s", id)
  } else if (db == "Ensembl"){
    sprintf("https://www.ensembl.org/id/%s", id)
  } else if (db == "GO"){
    sprintf("https://amigo.geneontology.org/amigo/term/%s", id)
  }
}

# Generate a hyperlink tag from URL and link text
hyperlink = function(url, text){
  if(is.null(url)){
    warning("No URL. Not generating hyperlink")
    return(text)
  }
  sprintf('<a href="%s" target="_blank">%s</a>', url, text)
}


# Generate condition pairs (length-2 vectors) from dds object
cond_pairs_from_dds = function(dds){
  combn(levels(dds$condition), 2) %>% split(rep(1:ncol(.), each = 2))
}

# Generate contrast names from condition levels
# "condition1_condition2": meaning condition1 / condition2
pair_to_contrast = function(pair){
  paste0(pair[1], "_", pair[2])
}

# Retrieve conditions from a contrast
contrast_to_conditions = function(contrast){
  str_split_1(contrast, "_")
}

