# List to store the cache objects
.cache = list()

# Reactive values to store the last cache time
.cache_time = reactiveValues()

# Set the cache time for a specific cache file
set_cache_time = function(name, time){
  .cache_time[[name]] <<- time
}

# Reset the cache time for all cache files in the cache directory
initialize_all_cache_time = function(){
  list.files(.dir_cache(), full.names = T) |>
    lapply(function(path){
      name = str_remove(basename(path), ".rds")
      time = as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
      set_cache_time(name, time)
    })
} 

# Cache object to store the file path, current data, identity data, and last cache time
new_cache_obj = function(name){
  list(
    data = NULL,
    identity = NULL
  )
}

# Shortcut for the cache file path
cache_file = function(name){
  here(.dir_cache(), paste0(name, ".rds"))
}

# Shortcut for the cache identity file path
cache_identity_file = function(name){
  here(.dir_cache_identity(), paste0(name, ".rds"))
}

# Add a new cache object to the list
add_cache = function(name){
  if(!name %in% names(.cache)){
    .cache[[name]] <<- new_cache_obj(name)
    set_cache_time(name, as.POSIXct("1970-01-01 00:00:00", tz = "UTC"))
    cat(sprintf("Added cache object '%s'\n", name))
  }
}

# Add all caches in the cache directory to the list
add_all_cache = function(){
  files = list.files(.dir_cache(), full.names = T)
  names = str_remove(basename(files), ".rds")
  names |> lapply(add_cache)
  invisible()
}

# Read cached data if the file has been updated since the last time it was read
read_cache = function(name){
  # Create the cache object if it doesn't exist
  add_cache(name)
  
  path = cache_file(name)
  id_path = cache_identity_file(name)
  if (!file.exists(path)){
    cat(sprintf("Cache '%s' has not been saved\n", name))
  } else {   # Read the data if the file exists and has been updated
    time = file.mtime(path)
    if (time > .cache_time[[name]]){
      data = readRDS(path)
      .cache[[name]]$data <<- data
      if(!file.exists(id_path)){
        id_data = NULL
      } else {
        id_data = readRDS(id_path)
      }
      .cache[[name]]$identity <<- id_data
      isolate(set_cache_time(name, time))
      cat(sprintf("Loaded cache file '%s'\n", name))
    }
  }
  # Return the data
  .cache[[name]]
}

# Write data to the cache, updating the cache object
write_cache = function(data_reactive, name, id_data = data_reactive()){
  # Create the cache object if it doesn't exist
  add_cache(name)
  
  # Do not write if the identity data is the same as the last time
  if(identical(id_data, .cache[[name]]$identity)){
    cat(sprintf("Identity data for cache '%s' is the same; not writing\n", name))
  } else {
    # If the identity data is different, write it to file and update the cache object
    path = cache_file(name)
    data = data_reactive()
    saveRDS(data, path)
    saveRDS(id_data, cache_identity_file(name))
    .cache[[name]]$data <<- data
    .cache[[name]]$identity <<- id_data
    set_cache_time(name, file.mtime(path))
    cat(sprintf("Updated cache file '%s'\n", name))
  }
  
  .cache[[name]]
}

# Clear the cache
clear_cache = function(name){
  # Remove the cache object
  if(name %in% names(.cache)){
    .cache[[name]] <<- NULL
    cat(sprintf("Removed cache object '%s'\n", name))
  }
  
  # Remove the file
  path = cache_file(name)
  if(file.exists(path)){
    file.remove(path)
    cat(sprintf("Removed cache file '%s'\n", .relative(path)))
  }
}

# Clear all caches
clear_all_cache = function(){
  names(.cache) |> lapply(clear_cache)
  invisible()
}

# Reset internal cache data
reset_cache = function(){
  .cache <<- list()
  add_all_cache()
  initialize_all_cache_time()
}

# Shortcut to get the identity data from the cache
cache_identity = function(name){
  reactive({
    .cache_time[[name]]
    .cache[[name]]$identity
  })
}

# Shortcut to get the cache item as a reactive
cache = function(name){
  reactive({
    read_cache(name)
  }) |> bindEvent(.cache_time[[name]])
}
