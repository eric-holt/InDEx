load_project = function(input){
  cat(sprintf("Loading project '%s'...\n", .next_project))
  updateTabsetPanel(inputId = "tbs_main", selected = "Home")
  
  # Reset all global reactive data values to NULL
  names(.g) |> lapply(function(name) {.g[[name]] = NULL})
  names(.r) |> lapply(function(name) {.r[[name]] = NULL})
  names(.re) |> lapply(function(name) {.re[[name]] = NULL})
  names(.cached) |> lapply(function(name) {.cached[[name]] = NULL})
  
  # Update project name
  assign_global(.next_project, "project")
  save_current_project()
  
  make_all_dir() # Just in case they have been deleted
  
  read_project_metadata()
  read_all_data()
  read_all_cache()
  read_input_state()
  
  org.Mm.eg.db |> assign_global("org")

  if(exists(".dds")){
    colnames(.dds) |> assign_global("samples")
    
    condition_from_sample(.samples) |> assign_global("conditions")
    
    brewer.pal(length(levels(.conditions)), "Set1") |> setNames(levels(.conditions)) |> assign_global("cond_colors")
    
    combn(levels(.conditions), 2) |> apply(2, pair_to_contrast) |> assign_global("contrasts")
    
    brewer.pal(length(.contrasts), "Set1") |> setNames(.contrasts) |> assign_global("cont_colors")
  }
  
  if(exists(".gtf")){
    genes_by_type(.gtf) |> assign_global("genes_by_type")
  }
  
  cat("\tRestoring input states...\n")
  restore_all_inputs(input)
  
  complete_project_load()
}
