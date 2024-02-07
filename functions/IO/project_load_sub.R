# Get ready to load another project
queue_project_load = function(next_project){
  .next_project <<- next_project
  .project_load_flag(T)
  cat(sprintf("Project '%s' set to be loaded\n", .next_project))
}

# Finish up project loading
complete_project_load = function(){
  .project_load_flag(F)
  .next_project <<- NULL
  .project_load_complete(.project)
  cat(sprintf("Done loading project '%s'\n", .project))
}

# Alias for loading state, used to prevent reactives from triggering
project_being_loaded = function(){
  .project_load_flag()
}

# For project-specific static data
# Assign to global variables .name and global reactives g$name
assign_global = function(data, name){
  assign(paste0(".", name), data, envir = .GlobalEnv)
  .g[[name]] = data
  cat("\tAssigned global variable", name, "\n")
}

# Read imported static data
read_data = function(name){
  path = here(dir_import(), paste0(name, ".rds"))
  if (check_path(path)){
    data = readRDS(path) 
    cat("\tLoaded data", name, "\n")
  }
  else {
    data = NULL
    cat("\t", name, "has not been saved; assigned NULL\n")
  }
  assign_global(data, name)
  data
}

# Read all imported static data upon project loading
read_all_data =function(){
  if(check_path(dir_import())){
    list.files(dir_import(), ".rds$") |> str_remove(".rds$") |> lapply(read_data)
    invisible()
  }
}

