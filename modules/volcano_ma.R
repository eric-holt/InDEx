volcano_ma_ui = function(ns = identity, id = "vol_ma"){
  id = ns(id)
  ns = NS(id)
  tagList(
    if (debugging) debug_ui(ns),
    uiOutput(ns("UI"))
  )
}

volcano_ma_server = function(alpha, lfc, selected_sig, id = "vol_ma"){
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    if (debugging) debug_server(environment())

    # Cached data
    dt_res = cache("dt_res")
    dt = reactive({
      req(dt_res())
      dt_res()$data
    })
    
    sg = reactive({
      tryCatch({
        selected_sig()
      }, error = function(e) character())
    })
    
    # UI
    output$UI = renderUI({
      if(is.null(dt())){
        return(caution("No data to plot"))
      }
      tagList(
        h4("Volcano plot" ),
        plotlyOutput(ns("volcano")),
        
        h4("MA plot"),
        plotlyOutput(ns("ma"), height = 200),
        
        h4("Raw p-value histogram"),
        
        fixedRow(column(2, wellPanel(observedNumericInput(ns("num_bins"), "bins", ss(ns("num_bins")) %||% 50, 10, 250, 10)))),
        plotlyOutput(ns("p_hist"), height = 200)
      )
    })
    
    # Input
    bins = reactive(input$num_bins)
    observe_input(ns("num_bins"), bins)
    
    # Volcano plot
    output$volcano = renderPlotly({
      cat("Rendering the volcano plot...\n")
      store_plots(gg_volcano(dt(), alpha(), lfc(), sg()), "volcano", plotly_volcano) |> suppressWarnings()
    })
    
    # MA plot
    output$ma = renderPlotly({
      cat("Rendering the MA plot...\n")
      store_plots(gg_MA(dt(), alpha(), lfc(), sg()), "MA", plotly_MA) |> suppressWarnings()
    })
    
    # p-value histogram
    output$p_hist = renderPlotly({
      cat("Rendering the p-value histogram...\n")
      store_plots(gg_p_hist(dt(), bins()), "p_hist", plotly_p_hist) |> suppressWarnings()
    })
  })
}
