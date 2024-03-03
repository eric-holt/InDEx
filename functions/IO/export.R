# Lists to store ggplot, plotly and data objects for export
.export_gg_list = list()
.export_plotly_list = list()
.export_data_list = list()

# Reset the export lists
init_export = function(){
  .export_gg_list <<- list()
  .export_plotly_list <<- list()
  .export_data_list <<- list()
}

# Store ggplot and Plotly objects for export and return Plotly
store_plots = function(gg_or_pl, id, pl_func = ggplotly){
  if("ggplot" %in% class(gg_or_pl)){
    .export_gg_list[[id]] <<- gg_or_pl
    .export_plotly_list[[id]] <<- pl_func(.export_gg_list[[id]])
    cat(sprintf("Stored ggplot and plotly for '%s' in export list\n", id))
  } else if ("plotly" %in% class(gg_or_pl)){
    .export_plotly_list[[id]] <<- gg_or_pl
    cat(sprintf("Stored plotly for '%s' in export list\n", id))
  } else {
    cat(sprintf("store_plots: 'ggplot' or 'plotly' not among the object classes: %s", paste(class(gg_or_pl), collapse = ", ")))
  }
  .export_plotly_list[[id]]
}

# Set data to be exported and return it
set_to_export = function(data, id){
  .export_data_list[[id]] <<- data
  cat(sprintf("Stored data for '%s' in export list\n", id))
  data
}

# Export ggplot objects in the global reactive values----
export_gg = function(id){
  if(check_path(.dir_export_gg())){
    saveRDS(.export_gg_list[[id]], here(.dir_export_gg(), paste0(id, ".rds")))
    cat(sprintf("Exported ggplot '%s'\n", id))
  }
}

export_all_gg = function(){
  if(check_path(.dir_export_gg())){
    for(id in names(.export_gg_list)){
      export_gg(id)
    }
  }
}

# Export plotly objects in the global reactive values----
export_pl = function(id){
  if(check_path(.dir_export_pl())){
    saveRDS(.export_plotly_list[[id]], here(.dir_export_pl(), paste0(id, ".rds")))
    cat(sprintf("Exported plotly '%s'\n", id))
  }
}

export_all_pl = function(){
  if(check_path(.dir_export_pl())){
    for(id in names(.export_plotly_list)){
      export_pl(id)
    }
  }
}

# Export data objects in the global list----
export_data = function(id){
  if(check_path(.dir_export_data())){
    saveRDS(.export_data_list[[id]], here(.dir_export_data(), paste0(id, ".rds")))
    cat(sprintf("Exported data '%s'\n", id))
  }
}

export_all_data = function(){
  if(check_path(.dir_export_data())){
    for(id in names(.export_data_list)){
      export_data(id)
    }
  }
}
