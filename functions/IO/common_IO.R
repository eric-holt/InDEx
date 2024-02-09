# Check if a directory exists and warn if not
check_path = function(path){
  if(length(path) != 1){
    cat("check_path: Path is not length 1\n")
    return(F)
  }
  
  if(dir.exists(path) || file.exists(path)){
    return(T)
  } else {
    cat(sprintf("Path '%s' does not exist\n", relative(path)))
    return(F)
  }
}

# Read matrix flexibly with rownames
read_matrix = function(filepath){
  m = fread(filepath)
  row_names = m[[1]]
  m = as.matrix(m[, -1])
  rownames(m) = row_names
  m
}