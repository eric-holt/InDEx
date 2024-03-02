# List to store the cache objects
.cache = list()

# Cache object to store the file path, current data, identity data, and last cache time
new_cache_obj = function(name){
  reactiveValues(
    path = cache_file(name),
    id_path = cache_identity_file(name),
    data = NULL,
    id_data = NULL,
    last_cache_time = as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
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
  .cache[[name]] <<- new_cache_obj(name)
  cat(sprintf("Added cache object '%s'\n", name))
}

# Read cached data if the file has been updated since the last time it was read
read_cache = function(name){
  # Create the cache object if it doesn't exist
  if(!exists(name, .cache)){
    add_cache(name)
  }
  
  # Read the data if the file exists and has been updated
  path = .cache[[name]]$path
  id_path = .cache[[name]]$id_path
  time = file.mtime(path)
  
  if (!file.exists(path)){
    cat(sprintf("Cache '%s' has not been saved\n", name))
  } else if (time > .cache[[name]]$last_cache_time){
    data = readRDS(path)
    .cache[[name]]$data <<- data
    .cache[[name]]$last_cache_time <<- time
    cat(sprintf("Loaded cache '%s' from file\n", name))
  }
  
  if (!file.exists(id_path)){
    cat(sprintf("Identity data for cache '%s' has not been saved\n", name))
  } else if (time > .cache[[name]]$last_cache_time){
    id_data = readRDS(id_path)
    .cache[[name]]$id_data <<- id_data
    cat(sprintf("Loaded identity data for cache '%s' from file\n", name))
  }
  
  # Return the data
  .cache[[name]]$data
}

# Write data to the cache, updating the cache object
write_cache = function(data_reactive, name, id_data = NULL){
  # Create the cache object if it doesn't exist
  if(!exists(name, .cache)){
    add_cache(name)
  }
  
  # Skip the identity check if no identity data is provided
  if(!is.null(id_data)){
    # Do not write if the identity data is the same as the last time
    if(identical(id_data, .cache[[name]]$id_data)){
      cat(sprintf("Identity data for cache '%s' is the same; not writing\n", name))
      return()
    } else {
      # If the identity data is different, write it to file and update the cache object
      saveRDS(id_data, .cache[[name]]$id_path)
      .cache[[name]]$id_data <<- id_data
      cat(sprintf("Updated identity data for cache '%s' to file\n", name))
    }
  } 
  
  # If the data is different, write it to file and update the cache object
  data = data_reactive()
  path = .cache[[name]]$path
  saveRDS(data, path)
  .cache[[name]]$data <<- data
  .cache[[name]]$last_cache_time <<- file.mtime(path)
  cat(sprintf("Updated cache '%s' to file\n", name))
  data
}

# Clear the cache
clear_cache = function(name){
  # Remove the cache object
  if(exists(name, .cache)){
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

# Shortcut to get the identity data from the cache
cache_identity = function(name){
  .cache[[name]]$id_data
}

# Automatically read or write the cache based on the identity data
cache = function(data_reactive, name, id_data = NULL){
  # Read the cache if the identity is the same
  if(!is.null(id_data) && identical(id_data, cache_identity(name))){
    read_cache(name)
  } else {
    # Otherwise, write the new data to the cache and return it
    write_cache(data_reactive, name, id_data)
  }
}

