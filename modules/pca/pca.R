pca_ui = function(ns = identity, id = "pca"){
  id = ns(id)
  ns = NS(id)
  tagList(
    if (debugging) debug_ui(ns),
    uiOutput(ns("UI"))
  )
}

pca_server = function(dds_pca, id = "pca") {
  moduleServer(id, function(input, output, session) {
    if (debugging) debug_server(environment())
    ns = session$ns
    
    # UI
    output$UI = renderUI({
      if(is.null(dds_pca()) || !nrow(dds_pca())){
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
      req(dds_pca())
      cat("Computing PCs...\n")
      get_pca(dds_pca()) |> set_to_export(ns("pca"))
    })
    
    # Data identity for checking the cache need
    pca_identity = reactive({
      req(dds_pca())
      c(nrow(dds_pca()), colnames(dds_pca()))
    })
    
    # Cache PCA result upon data change
    observe({
      req(pca_identity())
      write_cache(pca, ns("pca"), pca_identity())
    })
    
    # Use cached PCA result for visualization
    pca = reactive({
      req(.cache_time[[ns("pca")]])
      read_cache(ns("pca"))
    })

    # Number of PCs for plot scaling
    n = reactive({
      req(pca())
      length(pca()$explained_var)
    })
    
    # PCA plot
    output$scat = renderPlotly({
      cat("Rendering PCA plot...\n")
      store_plots(suppressWarnings(gg_pca(pca())), paste0(id, "_scat"), plotly_pca)
    })
    
    # All-PC plot
    output$all_pc = renderPlotly({
      cat("Rendering all-PC plot...\n")
      store_plots(suppressWarnings(gg_all_pc(pca())), paste0(id, "_all_pc"), plotly_all_pc)
    })
    
    # PCA hierarchical clustering heatmap
    output$hc_heatmap = renderPlotly({
      cat("Rendering PCA HC heatmap...\n")
      store_plots(suppressWarnings(pca_hc_heatmap(pca())), paste0(id, "_hc_heatmap"))
    })
  })
}
