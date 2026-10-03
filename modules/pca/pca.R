pca_ui = function(ns = identity, id = "pca"){
  id = ns(id)
  ns = NS(id)
  tagList(
    if (debugging) debug_ui(ns),
    uiOutput(ns("UI"))
  )
}

pca_server = function(data, id = "pca") {
  moduleServer(id, function(input, output, session) {
    if (debugging) debug_server(environment())
    ns = session$ns
    
    # UI
    output$UI = renderUI({
      if(is.null(data()) || !nrow(data()$data)){
        caution("No data")
      } else{
        tagList(
          plotlyOutput(ns("scat"), 500, 300),
          plotlyOutput(ns("all_pc"), 40 * n() + 80, 300),
          plotlyOutput(ns("hc_heatmap"), 30 * n() + 150, 30 * (n() - 1) + 150)
        )
      }
    })
    
    # PCA result list updater
    pca_ = reactive({
      req(data(), data()$data, nrow(data()$data) > 0)
      cat("Computing PCs...\n")
      get_pca(data()$data)
    })
    
    # Data identity for checking the cache need
    pca_identity = reactive({
      req(data())
      c(nrow(data()), colnames(data()))
    })
    
    # Cache PCA result upon data change
    observe({
      # req(data())
      write_cache(pca_, ns("pca"), data()$identity)
    })
    
    # Use cached PCA result for visualization
    pca = cache(ns("pca"))

    # Number of PCs for plot scaling
    n = reactive({
      req(pca())
      length(pca()$data$explained_var)
    })
    
    # PCA plot
    output$scat = renderPlotly({
      cat("Rendering PCA plot...\n")
      store_plots(gg_pca(pca()$data), paste0(id, "_scat"), plotly_pca) |> suppressWarnings()
    })
    
    # All-PC plot
    output$all_pc = renderPlotly({
      cat("Rendering all-PC plot...\n")
      store_plots(gg_all_pc(pca()$data), paste0(id, "_all_pc"), plotly_all_pc) |> suppressWarnings()
    })
    
    # PCA hierarchical clustering heatmap
    output$hc_heatmap = renderPlotly({
      cat("Rendering PCA HC heatmap...\n")
      store_plots(pca_hc_heatmap(pca()$data), paste0(id, "_hc_heatmap")) |> suppressWarnings()
    })
  })
}
