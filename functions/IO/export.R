# Store ggplot and plotly objects in global reactive values
store_plots = function(gg_object, id){
  if("gg_plot" %in% class(gg_object)){
    .gg[[id]] <<- gg_object
    .pl[[id]] <<- ggplotly(.gg[[id]])
  }
}

# Export ggplot objects in the global reactive values----
export_gg = function(id){
  if(check_path(dir_export_gg())){
    saveRDS(.gg[[id]], here(dir_export_gg(), paste0(id, ".rds")))
    cat(sprintf("Exported ggplot '%s'\n", id))
  }
}

export_all_gg = function(){
  if(check_path(dir_export_gg())){
    for(id in names(.gg)){
      export_gg(id)
    }
  }
}

# Export plotly objects in the global reactive values----
export_pl = function(id){
  if(check_path(dir_export_pl())){
    saveRDS(.pl[[id]], here(dir_export_pl(), paste0(id, ".rds")))
    cat(sprintf("Exported plotly '%s'\n", id))
  }
}

export_all_pl = function(){
  if(check_path(dir_export_pl())){
    for(id in names(.pl)){
      export_pl(id)
    }
  }
}

# Export data objects in the global reactive values----
export_data = function(id){
  if(check_path(dir_export_re())){
    saveRDS(.re[[id]], here(dir_export_re(), paste0(id, ".rds")))
    cat(sprintf("Exported reactive value '%s'\n", id))
  }
}

export_all_data = function(){
  if(check_path(dir_export_re())){
    for(id in names(.re)){
      export_data(id)
    }
  }
}
