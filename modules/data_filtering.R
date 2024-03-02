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
                   observedCheckboxGroupInput(ns("cbg_samples"), "Samples", .samples, .samples),
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
                        fixedRow(
                          column(8, observedCheckboxGroupInput(ns("cbg_gene_types"), "Exclude gene types", .gene_types, NULL)),
                          column(4, uiOutput(ns("ui_gene_count")))
                          )
                        )
                 )),
          column(9,
                 tabsetPanel(
                   tabPanel("Raw count", DTOutput(ns("dt_count"))),
                   tabPanel("Normalized count", DTOutput(ns("dt_norm"))),
                   if(exists(".dt_tpm")) tabPanel("TPM", DTOutput(ns("dt_tpm"))))))
      )
    }) |> bindEvent(.project_load_complete())
    
    # Gene count UI responds to filtered()----
    output$ui_gene_count = renderUI({
      req(filtered(), !project_being_loaded())
      cat("Rendering gene counts...\n")
      filtered_genes = feature_to_gene(filtered())
      num_genes = sapply(1:length(.gene_types), function(i) {
        sum(filtered_genes %in% .genes_by_type[[i]])
      })
      
      HTML(paste(sapply(c("N", num_genes), function(n) {
        sprintf("<label><span>(%s)</span></label>", n)
      }), collapse = "<br>"))
    })
    
    # Inputs----
    sp = reactive({
      input$cbg_samples
      }) |> debounce(2000)
    observe_input(ns("cbg_samples"), sp)
    
    mc = reactive(as.numeric(input$num_min_count)) |> debounce(2000)
    observe_input(ns("num_min_count"), mc)
    
    mn = reactive(as.numeric(input$num_min_norm)) |> debounce(2000)
    observe_input(ns("num_min_norm"), mn)
    
    mt = reactive(as.numeric(input$num_min_tpm)) |> debounce(2000)
    observe_input(ns("num_min_tpm"), mt)
    
    cr = reactive(as.numeric(input$num_rlog_cv)) |> debounce(2000)
    observe_input(ns("num_rlog_cv"), cr)
    
    cn = reactive(as.numeric(input$num_norm_cv)) |> debounce(2000)
    observe_input(ns("num_norm_cv"), cn)
    
    ct = reactive(as.numeric(input$num_tpm_cv)) |> debounce(2000)
    observe_input(ns("num_tpm_cv"), ct)
    
    gt = reactive(input$cbg_gene_types) |> debounce(2000)
    observe_input(ns("cbg_gene_types"), gt)

    # Feature filters
    # by count/TPM
    filtered = reactive({
      req(.g$dt_count, !project_being_loaded(), mc(), mn(), mt(), cr(), cn(), ct())
      f = filter_by_count(.g$dt_count, mc()) |>
        intersect(filter_by_count(.dt_norm, mn())) |>
        intersect(filter_by_cv(.dt_rlog, cr())) |>
        intersect(filter_by_cv(.dt_norm, cn()))
      if(exists(".dt_tpm")){
        f = f |>
          intersect(filter_by_count(.dt_tpm, mt())) |>
          intersect(filter_by_cv(.dt_tpm, ct()))
      }
      f
    })
    
    # by gene type
    included = reactive({
      req(filtered(), !project_being_loaded(), gt())
      exclude_gene(filtered(), gt())
    })
    
    observe({
      req(included(), !project_being_loaded())
      write_cache(included, "included", c(sp(), mc(), mn(), mt(), cr(), cn(), ct(), gt()))
    })
    
    observe({
      req(sp(), !project_being_loaded())
      sp()
      write_cache(sp, "samples")
    })
    
    # Count/TPM matrix display
    output$dt_count = renderDT({
      samples = read_cache("samples")
      req(.g$dt_count, all(samples %in% .samples), !project_being_loaded())
      dt_display(.g$dt_count, samples, read_cache("included"), 0)
    })
    
    output$dt_norm = renderDT({
      samples = read_cache("samples")
      req(.g$dt_norm, all(samples %in% .samples), !project_being_loaded())
      dt_display(.g$dt_norm, samples, read_cache("included"))
    })
    
    output$dt_tpm = renderDT({
      samples = read_cache("samples")
      req(.g$dt_tpm, all(samples %in% .samples), !project_being_loaded())
      dt_display(.g$dt_tpm, samples, read_cache("included"))
    })

  })
}