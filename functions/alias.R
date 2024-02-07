projects = function(){
  c(list.dirs(here(dir_user(), "project"), full.names = F, recursive = F))
}

# Current user directory
dir_user = function(user = .user){
  here("user", user)
}

# Current project directory
dir_project = function(project = .project, user = .user){
  here(dir_user(user), "project", project)
}

# Subdirectories under the project directory
dir_cache = function(project = .project, user = .user){
  here(dir_project(project, user), "cache")
}

dir_gg = function(project = .project, user = .user){
  here(dir_project(project, user), "ggplot")
}

dir_plotly = function(project = .project, user = .user){
  here(dir_project(project, user), "plotly")
}

dir_export = function(project = .project, user = .user){
  here(dir_project(project, user), "exported_reactives")
}

dir_import = function(project = .project, user = .user){
  here(dir_project(project, user), "imported_data")
}