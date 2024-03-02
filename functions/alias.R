.projects = function(){
  c(list.dirs(here(.dir_user(), "project"), full.names = F, recursive = F))
}

# Current user directory
.dir_user = function(user = .user){
  here("user", user)
}

# Current project directory
.dir_project = function(project = .project, user = .user){
  here(.dir_user(user), "project", project)
}

# Subdirectories under the project directory
.dir_cache = function(project = .project, user = .user){
  here(.dir_project(project, user), "cache")
}

.dir_export_gg = function(project = .project, user = .user){
  here(.dir_project(project, user), "exported_ggplot")
}

.dir_export_pl = function(project = .project, user = .user){
  here(.dir_project(project, user), "exported_plotly")
}

.dir_export_re = function(project = .project, user = .user){
  here(.dir_project(project, user), "exported_data")
}

.dir_import = function(project = .project, user = .user){
  here(.dir_project(project, user), "imported_data")
}

# Show the relative path
.relative = function(path, reference = .dir_user()){
  path |> str_remove(reference)
}