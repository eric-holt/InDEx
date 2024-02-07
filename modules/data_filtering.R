data_ui = function(ns = identity, id = "data"){
  id = ns(id)
  ns = NS(id)
  
  tagList(
    if (debugging) debug_ui(ns),
    uiOutput(ns("UI"))
  )
}

data_server = function(id = "data") {
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    if (debugging) debug_server(environment())
    
    # UI
    output$UI = renderUI({
      if(!exists(".dt_count")){
        return(caution("No data to filter"))
      }
      tagList(
        fixedRow(
          column(3,
                 wellPanel(
                   uiOutput(ns("ui_samples")),
                   span(title = "Count or TPM cutoff for all samples in at least one condition",
                        h5("Group concensus threshold")),
                   fixedRow(
                     column(4, 
                            span(title = "Minimum raw count",
                                 observedNumericInput(ns("num_min_count"), "raw", 0, 0))),
                     column(4, 
                            span(title = "Minimum DESeq-normalized count",
                                 observedNumericInput(ns("num_min_norm"), "norm.", 0, 0))),
                     if(exists(".dt_tpm")) column(4,
                                                span(title = "Minimum TPM",
                                                     observedNumericInput(ns("num_min_tpm"), "TPM", 0, 0)))),
                   span(title = "Percentile cutoff for coefficient of variation (SD/mean). Bottom X percent will be removed",
                        h5("CV percentile threshold")),
                   fixedRow(
                     column(4, 
                            span(title = "Minimum CV percentile in regularized log count",
                                 observedNumericInput(ns("num_rlog_cv"), "rlog", 0, 0, 99))),
                     column(4,
                            span(title = "Minimum CV percentile in normalized count",
                                 observedNumericInput(ns("num_norm_cv"), "norm.", 0, 0, 99))),
                     if(exists(".dt_tpm")) column(4,
                                                span(title = "Minimum CV percentile in TPM",
                                                     observedNumericInput(ns("num_tpm_cv"), "TPM", 0, 0, 99)))),
                   span(title = "Gene types to exclude",
                        uiOutput(ns("ui_gene_types")))
                 )),
          column(9,
                 tabsetPanel(
                   tabPanel("Raw count", DTOutput(ns("dt_count"))),
                   tabPanel("Normalized count", DTOutput(ns("dt_norm"))),
                   if(exists(".dt_tpm")) tabPanel("TPM", DTOutput(ns("dt_tpm"))))))
      )
    }) |> bindEvent(.project_load_complete())
    
    # Gene type choice dynamic UI----
    output$ui_gene_types = renderUI({
      req(filtered(), !project_being_loaded())
      update_gene_types(filtered())
      checkboxGroupInput(ns("cbg_gene_types"), "Exclude gene types",
                         choices = .temp$gene_types_choices,
                         selected = .temp$gene_types_selected)
    })
    
    observe({
      update_gt_selected(gt())
    })
    
    # Samples dynamic UI
    output$ui_samples = renderUI({
      req(!project_being_loaded())
      observedCheckboxGroupInput(ns("cbg_samples"), "Samples", .samples, .samples)
    })
    
    # Inputs----
    sp = reactive({
      input$cbg_samples
      }) |> debounce(2000)
    observe_input(ns("cbg_samples"), sp)
    
    mc = reactive(as.numeric(input$num_min_count))
    observe_input(ns("num_min_count"), mc)
    
    mn = reactive(as.numeric(input$num_min_norm))
    observe_input(ns("num_min_norm"), mn)
    
    mt = reactive(as.numeric(input$num_min_tpm))
    observe_input(ns("num_min_tpm"), mt)
    
    cr = reactive(as.numeric(input$num_rlog_cv))
    observe_input(ns("num_rlog_cv"), cr)
    
    cn = reactive(as.numeric(input$num_norm_cv))
    observe_input(ns("num_norm_cv"), cn)
    
    ct = reactive(as.numeric(input$num_tpm_cv))
    observe_input(ns("num_tpm_cv"), ct)
    
    gt = reactive(input$cbg_gene_types) |> debounce(2000)
    observe_input(ns("cbg_gene_types"), gt)
    
    i_gt = reactiveVal()
    observe({
      req(gt(), .temp$gene_types_choices, .temp$gene_types_selected)
      which(.temp$gene_types_choices %in% .temp$gene_types_selected) %>% i_gt
    })
    
    # Feature filters
    filtered = reactiveVal()
    observe({
      req(.g$dt_count, !project_being_loaded())
      f = filter_by_count(.g$dt_count, mc()) |>
        intersect(filter_by_count(.dt_norm, mn())) |>
        intersect(filter_by_cv(.dt_rlog, cr())) |>
        intersect(filter_by_cv(.dt_norm, cn()))
      if(exists(".dt_tpm")){
        f = f |>
          intersect(filter_by_count(.dt_tpm, mt())) |>
          intersect(filter_by_cv(.dt_tpm, ct()))
      }
      f %>% filtered
    })
    
    included = reactiveVal()
    observe({
      req(filtered(), !project_being_loaded())
      exclude_gene(filtered(), i_gt()) %>% included
    })
    
    # Count/TPM matrix display
    output$dt_count = renderDT({
      req(.g$dt_count, all(sp() %in% .samples), !project_being_loaded())
      dt_display(.g$dt_count, sp(), included(), 0)
    })
    output$dt_norm = renderDT({
      req(.g$dt_norm, all(sp() %in% .samples), !project_being_loaded())
      dt_display(.g$dt_norm, sp(), included())
    })
    
    output$dt_tpm = renderDT({
      req(.g$dt_tpm, all(sp() %in% .samples), !project_being_loaded())
      dt_display(.g$dt_tpm, sp(), included())
    })
    
    # Subset DESeq Data Set with the selected filter conditions
    # dds = reactive({
    #   req(sp())
    #   .g$dds[included(), sp()]
    # })
    
    dds = reactiveVal()
    observe({
      req(!project_being_loaded())
      tryCatch({.g$dds[included(), sp()]}, error = function(e) NULL) %>% dds
    })

    # return(dds())
    return(dds)
  })
}