# Load a project
load_project = function(input){
  cat(sprintf("Loading project '%s'...\n", .next_project))
  updateTabsetPanel(inputId = "tbs_main", selected = "Home")
  
  # Reset all export lists
  init_export()

  # Update project name
  .project <<- NULL
  .next_project |> assign_global("project")
  save_current_project()
  
  make_all_dir() # Just in case they have been deleted
  
  read_project_metadata()
  read_all_data()
  read_input_state()
  
  # Reset cache to prevent data from previous project from being used
  reset_cache()
  
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
