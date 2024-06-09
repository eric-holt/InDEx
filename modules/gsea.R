gsea_ui = function(ns = identity, id = "gsea"){
  ns = NS(ns(id))
  tagList(
    if (debugging) debug_ui(ns),
    uiOutput(ns("UI"))
  )
}

gsea_server = function(id = "gsea") {
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    if (debugging) debug_server(environment())
    
    # Cached data
    data = cache("dt_res")
    
    # UI
    output$UI = renderUI({
      if(is.null(data()) || is.null(data()$data)){
        return(caution("No data"))
      }
      tagList(
        fixedRow(column(7, wellPanel(
          fixedRow(
            column(4, 
                   actionButton(ns("btn_cp"), HTML("Run gseGO()<br>(may take a few minutes)"), icon("rotate-right")),
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
        go_panels_ui(ns, "gsea_go")
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

    # GSEA data identity; sort, p, q, n are not part of the data
    identity = reactive({
      req(data(), mings(), maxgs())
      c(data()$identity, mings(), maxgs())
    })
    
    # Indicator for data change, must respond to data_identity and cache time changes
    output$data_identity = renderUI({
      req(identity())
      .cache_time[["gsea_go"]]
      if(!identical(identity(), cache_identity("gsea_go"))) {
        caution("Data changed")
      } else {
        span("Data not changed", icon("check"), style = "color: green;")
      }
    })
    
    # GSEA result with GO terms
    gsea_go_ = reactive({
      req(data())
      if(nrow(data()$data)){
        cat("Computing GO GSEA...\n")
        get_all_gsea(data()$data, 1, mings(), maxgs())
      } else {
        NULL
      }
    })
    
    # Cache the results upon button click
    observe({
      write_cache(gsea_go_, "gsea_go", identity())$data
    }) |> bindEvent(input$btn_cp)
    
    # Use the cache for visualization
    gsea_go = cache("gsea_go")

    # Update the GSEA panel; give the reactives
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
