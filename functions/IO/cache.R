read_cache = function(name){
  path = here(dir_cache(), paste0(name, ".rds"))
  if (file.exists(path)){
    data = readRDS(path) 
    cat("\tLoaded cached data", name, "\n")
  }
  else {
    data = NULL
    cat("\tCache", name, "has not been saved\n")
  }
  .cached[[name]] = data
  data
}

read_all_cache = function(){
  list.files(dir_cache(), ".rds$") |> str_remove(".rds$") |> lapply(read_cache)
  invisible()
}

write_cache = function(data, name){
  .cached[[name]] = data
  dir.create(dir_cache(), recursive = T, showWarnings = F)
  path = here(dir_cache(), paste0(name, ".rds"))
  saveRDS(data, path)
  cat("Saved cached data", name, "\n")
}

# Convenience functions---- 
# Pass object as a reactive to evaluate only when being saved.
cashed_data = function(name){
  .cached[[name]]
}

is_cached = function(name){
  !is.null(cashed_data(name))
}

cashed_data_identity = function(name){
  .cached[[paste0(name, "_last_data")]]
}

record_data_identity = function(data_identity, name){
  write_cache(data_identity, paste0(name, "_last_data"))
}

is_data_updated = function(data_identity, name){
  !identical(data_identity, cashed_data_identity(name))
}

is_new = function(data_identity, name){
  !is_cached(name) || is_data_updated(data_identity, name)
}

update_cache = function(reactive_object, data_identity, name){
  record_data_identity(data_identity, name)
  write_cache(reactive_object(), name)
}

auto_update_cache = function(reactive_object, data_identity, name){
  if(is_new(data_identity, name)) update_cache(reactive_object, data_identity, name)
}

auto_cache = function(reactive_object, data_identity, name){
  auto_update_cache(reactive_object, data_identity, name)
  cashed_data(name)
}