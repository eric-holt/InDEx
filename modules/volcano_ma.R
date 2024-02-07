volcano_ma_ui = function(ns = identity, id = "vol_ma"){
  id = ns(id)
  ns = NS(id)
  tagList(
    if (debugging) debug_ui(ns),
    uiOutput(ns("UI"))
  )
}

volcano_ma_server = function(dt_res, alpha, lfc, sg, id = "vol_ma"){
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    if (debugging) debug_server(environment())
    
    output$UI = renderUI({
      if(is.null(dt_res())){
        return(caution("No data to plot"))
      }
      tagList(
        actionButton(ns("btn_export"), "Export plots"),
        
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
      plotly_volcano(dt_res(), alpha(), lfc(), sg()) %>% suppressWarnings
    })
    
    # MA plot
    output$ma = renderPlotly({
      cat("Rendering the MA plot...\n")
      plotly_MA(dt_res(), alpha(), lfc(), sg()) %>% suppressWarnings
    })
    
    # p-value histogram
    output$p_hist = renderPlotly({
      cat("Rendering the p-value histogram...\n")
      plotly_pval_hist(dt_res(), bins()) %>% suppressWarnings
    })
    
    # Save ggplot
    observe({
      save = function(name, plt, ...){
        filename = paste0(name, ".png")
        ggsave(here("projects", .project, "plots", filename), plt, ...)
        cat("Saved", filename, "\n")
      } 
      plt_vol = gg_volcano(dt_res(), alpha(), lfc(), sg()) %>% suppressWarnings
      plt_ma = gg_MA(dt_res(), alpha(), lfc(), sg()) %>% suppressWarnings
      plt_hist = gg_pval_hist(dt_res(), bins()) %>% suppressWarnings
      save("volcano_plot", plt_vol, height = 4)
      save("MA_plot", plt_ma, height = 4)
      save("p_histogram", plt_hist, height = 4)
      save("volcano_MA_p", plt_vol/plt_ma/plt_hist)
    }) |> bindEvent(input$btn_export)
  })
}