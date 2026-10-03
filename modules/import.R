import_ui = function(ns = identity, id = "import"){
  id = ns(id)
  ns = NS(id)
  
  set_button_tooltips = function(){
    tags$head(
      tags$script(HTML(sprintf("
      $(document).ready(function() {
        $('#%s').attr('title', 'Add new project');
        $('#%s').attr('title', 'Edit this project');
        $('#%s').attr('title', 'Delete this project');
      });
    ", ns("btn_add"), ns("btn_edit"), ns("btn_del")))))
  }
  
  set_button_positions = function(){
    tags$head(tags$style(HTML(sprintf("
      #%s, #%s, #%s {
      position: relative;
      top: 24px;
      left: -28px;
    ", ns("btn_add"), ns("btn_edit"), ns("btn_del")))))
  }
  
  tagList(
    if (debugging) debug_ui(ns),
    set_button_tooltips(),
    set_button_positions(),
    wellPanel(
      fixedRow(
        column(7, selectInput(ns("sel_project"), "Change project", choices = .projects(), selected = .project)),
        column(5, actionGroupButtons(ns(c("btn_add", "btn_edit", "btn_del")), list(icon("plus"), icon("edit"), icon("trash"))))))
  )
}

import_server = function(id = "import"){
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    if (debugging) debug_server(environment())
    
    adding_new = reactiveVal(F)
    file_null_error = list()
    project_name_error = T
    
    # Dialogs
    show_add_edit_dialog = function(name = ""){
      showModal(modalDialog(
        fluidRow(
          column(6,
                 if(adding_new()) h4("Create a new project")
                 else h4(sprintf("Update project %s", name)),
                 uiOutput(ns("proj_name_err")),
                 textInput(ns("project_name"), "Project name"),
                 uiOutput(ns("gtf_err")),
                 fileInput(ns("file_gtf"), "GTF", accept = c(".gtf", ".gtf.gz"), placeholder = "")),
          column(6,
                 uiOutput(ns("counts_err")),
                 fileInput(ns("file_counts"), "Count matrix", placeholder = ""),
                 uiOutput(ns("tpm_err")),
                 fileInput(ns("file_tpm"), "TPM matrix", placeholder = ""))),
        footer = tagList(
          modalButton("Cancel"),
          if(adding_new()) actionButton(ns("btn_create"), "Create")
          else actionButton(ns("btn_update"), "Update"))))
      if(!adding_new()) updateTextInput(inputId = "project_name", value = name)
    }
    
    proj_name_err = function(name){
      name = trimws(name)
      special = "[\\\\/:*?\"<>|]"
      project_name_error <<- T
      if(name == "")
        caution("Project name is required")
      else if(name %in% .projects() && adding_new())
        caution(sprintf("Project '%s' already exists", name))
      else if(grepl(special, name))
        caution(sprintf("Special characters '%s' not allowed", special))
      else{
        project_name_error <<- F
        ok()
      }
    }
    
    file_err = function(file, type = ""){
      file_null_error[[type]] <<- T
      if(is.null(file)){
        if(adding_new()){
          caution(sprintf("%s file is required", type))
        } else {
          span(sprintf("No file is selected"))
        }
      }
      else{
        file_null_error[[type]] <<- F
        ok(sprintf("%s is selected", file$name))
      }
    }
    
    show_del_dialog = function(name){
      showModal(modalDialog(
        h4(sprintf("Delete project %s?", name)),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("btn_del_confirm"), "Delete"))))
    }
    
    update_project_list = function(selected_project){
      updateSelectInput(inputId = "sel_project", choices = .projects(), selected = selected_project)
    }
    
    # Button actions----
    # Create ----
    create_project = function(pn, fg, fc, ft){
      if (!project_name_error & !any(unlist(file_null_error))){
        removeModal()
        project = trimws(pn)
        update_project(project, fg, fc, ft)
        cat("Created project", project, "\n")
      }
    }
    
    # Edit ----
    edit_project = function(pn, fg, fc, ft){
      removeModal()
      project = trimws(pn)
      if(project != .project){
        file.rename(.dir_project(.project), .dir_project(project))
        cat("Renamed project", .project, "to", project, "\n")
      }
      update_project(project, fg, fc, ft)
      cat("Updated project", project, "\n")
    }
    
    # Update project (follows create or edit)----
    update_project = function(project, fg, fc, ft){
      make_all_dir(project)
      update_project_list(project)
      
      if(!any(is.null(c(fg, fc, ft)))){
        reset_cache()
      }
      
      import_project_data = function(data, name){
        import_data(data, name, project)
      }
      
      .metadata$Name <<- project
      
      # GTF
      if(!is.null(fg)){
        gtf = get_gtf(fg$datapath)
        import_project_data(gtf, "gtf")
        .metadata$GTF <<- fg$name
      } else gtf = .gtf
      
      # Counts
      if(!is.null(fc)){
        cat("Processing DDS...\n")
        cm = read_matrix(fc$datapath) |> round()
        samples = colnames(cm)
        conditions = condition_from_sample(samples)
        colData = data.frame(condition = conditions)
        design = formula(~ condition)
        dds = DESeqDataSetFromMatrix(cm, colData, design) |> 
          estimateSizeFactors() |>
          estimateDispersions()
        dds = dds[rowSums(counts(dds)) > 0, ]
        dds |> import_project_data("dds")
        
        cat("Processing count data.tables...\n")
        dt_count = dds |> counts() |> matrix_to_dt(gtf) 
        dt_count |> import_project_data("dt_count")
        dt_rlog = dds |> counts() |> rlog() |> matrix_to_dt(gtf)
        dt_rlog |> import_project_data("dt_rlog")
        dt_norm = dds |> counts(normalized = T) |> matrix_to_dt(gtf)
        dt_norm |> import_project_data("dt_norm")
        .metadata$Count <<- fc$name
      } else dds = .dds
      
      if(!is.null(ft)){
        cat("Processing TPM data.table...\n")
        tm = read_matrix(ft$datapath)
        tm |> import_project_data("tm")
        dt_tpm = tm[rownames(dds), colnames(dds)] |> matrix_to_dt(gtf)
        dt_tpm |> import_project_data("dt_tpm")
        .metadata$TPM <<- ft$name
      }
      
      save_project_metadata(project)
      updateSelectInput(inputId = ns("project_name"), choices = .projects(), selected = project)
      # queue_project_load(project)
    }
    
    # Delete ----
    delete_project = function(name){
      removeModal()
      unlink(.dir_project(name), recursive = T)
      update_project_list("")
      cat("Deleted project", name, "\n")
    }
    
    # Inputs
    pn = reactive(input$project_name)
    pr = reactive(input$sel_project)
    fg = reactive(input$file_gtf)
    fc = reactive(input$file_counts)
    ft = reactive(input$file_tpm)
    
    # When project is changed, load the project
    observe({
      req(pr() != .project)
      queue_project_load(pr())
    })

    # Create
    observe({
      adding_new(T)
      show_add_edit_dialog()
    }) |> bindEvent(input$btn_add)

    observe(create_project(pn(), fg(), fc(), ft())) |> bindEvent(input$btn_create)
    output$proj_name_err = renderUI(proj_name_err(pn()))
    output$gtf_err = renderUI(file_err(fg(), "GTF"))
    output$counts_err = renderUI(file_err(fc(), "Count matrix"))
    output$tpm_err = renderUI(file_err(ft(), "TPM matrix"))
    output$file_gtf_selected = renderUI(file_selected(fg()))
    output$file_counts_selected = renderUI(file_selected(fc()))
    output$file_tpm_selected = renderUI(file_selected(ft()))
    
    # Edit
    observe({
      adding_new(F)
      show_add_edit_dialog(pr())
    }) |> bindEvent(input$btn_edit)

    observe(edit_project(pn(), fg(), fc(), ft())) |> bindEvent(input$btn_update)
    
    # Delete
    observe(show_del_dialog(pr())) |> bindEvent(input$btn_del)

    observe(delete_project(pr())) |> bindEvent(input$btn_del_confirm)
  })
}