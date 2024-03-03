gsea_ui = function(ns = identity, id = "gsea"){
  ns = NS(ns(id))
  tagList(
    if (debugging) debug_ui(ns),
    uiOutput(ns("UI"))
    
  )
}

gsea_server = function(dt_enrich, genes, id = "gsea") {
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    if (debugging) debug_server(environment())
    
    # Cached DESeq2 result
    dt_res = reactive({
      .cache_time$dt_res
      read_cache("dt_res")
    })
    
    # UI
    output$UI = renderUI({
      if(is.null(dt_enrich()) && is.null(dt_res())){
        return(caution("No data"))
      }
      tagList(
        fixedRow(column(7, wellPanel(
          fixedRow(
            column(4, 
                   actionButton(ns("btn_cp"), HTML("Run clusterProfiler<br>(may take a few minutes)"), icon("rotate-right")),
                   uiOutput(ns("data_identity"))
            ),
            column(4, observedNumericInput(ns("num_minGSSize"), "Min. gene set size", 10, 0)),
            column(4, observedNumericInput(ns("num_maxGSSize"), "Max. gene set size", 500, 0))))),
          column(2, actionButton(ns("btn_reset"), "Reset input")),
          ),
        fixedRow(column(9, wellPanel(
          fixedRow(
            column(3, observedNumericInput(ns("num_n"), "Terms to display", 10, 5, 100)),
            column(3, observedRadioButtons(ns("rbn_sort"), "Sort by", c("p-value", "gene count"), "p-value", inline = T)),
            column(3, observedNumericInput(ns("num_pvalueCutoff"), "p-value cutoff", 1, 0, 1)),
            column(3, observedNumericInput(ns("num_qvalueCutoff"), "q-value cutoff", .5, 0, 1)))))),
        tabsetPanel(
          tabPanel("Enriched GO terms", go_panels_ui(ns, "enrich_go")),
          tabPanel("GSEA (GO)", go_panels_ui(ns, "gsea_go")),
        )
      )
    })
    
    # Inputs
    mings = reactive(input$num_minGSSize)
    observe_input(ns("num_minGSSize"), mings)

    maxgs = reactive(input$num_maxGSSize)
    observe_input(ns("num_maxGSSize"), maxgs)
    
    sort = reactive(input$rbn_sort)
    observe_input(ns("rbn_sort"), sort)
    
    p = reactive(input$num_pvalueCutoff)
    observe_input(ns("num_pvalueCutoff"), p)
    
    q = reactive(input$num_qvalueCutoff)
    observe_input(ns("num_qvalueCutoff"), q)
    
    n = reactive(input$num_n)
    observe_input(ns("num_n"), n)
    
    # Data identity
    data_identity = reactive({
      req(dt_res())
      dt_res()[, .(baseMean, pvalue, padj, log2FoldChange)] |> na.omit() |> colSums() |> unname() |> c(cache_identity("outliers"))
    })
    
    # Indicator for data change, must respond to data_identity and cache time changes
    output$data_identity = renderUI({
      req(dt_res(), data_identity())
      .cache_time$enrich_go
      .cache_time$gsea_go
      if(!identical(data_identity(), cache_identity("enrich_go")) || !identical(data_identity(), cache_identity("gsea_go"))) {
        caution("Data changed")
      } else {
        span("Data not changed", icon("check"), style = "color: green;")
      }
    })

    # GO term enrichment result 
    enrich_go_ = reactive({
      req(dt_enrich())
      if(!is.null(dt_enrich()) && nrow(dt_enrich()) > 0){
        cat("Computing GO enrichment...\n")
        get_enrich_go(dt_enrich(), genes(), 1, 1, mings(), maxgs())
      } else {
        NULL
      }
    })
    
    # GSEA result with GO terms
    gsea_go_ = reactive({
      req(dt_res())
      cat("Computing GO GSEA...\n")
      get_all_gsea(dt_res(), 1, mings(), maxgs())
    })
    
    # Use the cache for visualization
    enrich_go = reactive({
      .cache_time$enrich_go
      read_cache("enrich_go")
    })
    
    gsea_go = reactive({
      .cache_time$gsea_go
      read_cache("gsea_go")
    })
    
    # Cache the results upon button click
    observe({
      req(dt_res(), dt_enrich())
      write_cache(enrich_go_, "enrich_go", data_identity()) |> set_to_export("enrich_go")
      write_cache(gsea_go_, "gsea_go", data_identity()) |> set_to_export("gsea_go")
    }) |> bindEvent(input$btn_cp)
    
    # Update the GO panels; give the reactives
    go_panels_server(enrich_go, p, q, n, sort, "enrich_go")
    
    go_panels_server(gsea_go, p, q, n, sort, "gsea_go")

    # Reset inputs upon button click
    observe({
      updateNumericInput(inputId = "num_n", value = 10)
      updateNumericInput(inputId = "num_pvalueCutoff", value = 1)
      updateNumericInput(inputId = "num_qvalueCutoff", value = .5)
      updateNumericInput(inputId = "num_minGSSize", value = 10)
      updateNumericInput(inputId = "num_maxGSSize", value = 500)
      updateRadioButtons(inputId = "rbn_sort", selected = "p-value")
      cat("GO inputs reset\n")
    }) |> bindEvent(input$btn_reset)
  })
}
