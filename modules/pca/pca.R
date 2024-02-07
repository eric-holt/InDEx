pca_ui = function(ns = identity, id = "pca"){
  id = ns(id)
  ns = NS(id)
  tagList(
    if (debugging) debug_ui(ns),
    # tags$h4(label),
    uiOutput(ns("UI"))
  )
}

pca_server = function(dds, id = "pca") {
  moduleServer(id, function(input, output, session) {
    if (debugging) debug_server(environment())
    ns = session$ns
    
    output$UI = renderUI({
      if(is.null(dds()) || !nrow(dds())){
        caution("No data")
      } else{
        uiOutput(ns("plots"))
      }
    })
    
    # PCA result list
    pca = reactive({
      req(dds())
      get_pca(dds())
    })
    
    # Data identity for checking the cache need
    data_identity = reactive({
      req(dds())
      c(nrow(dds()), colnames(dds()))
    })
    
    # Number of PCs for plot scaling
    n = reactive({
      req(pca())
      length(pca()$explained_var)
    })
    
    # All plots in one UI for data caching and lazy reaction
    output$plots = renderUI({
      req(pca(), data_identity())
      auto_cache(pca, data_identity(), id)
      isolate({
        .re$pca <<- pca()
        store_plots(suppressWarnings(ggplot_pca(.re$pca)), paste0(id, "_scat"))
        store_plots(suppressWarnings(ggplot_all_pc(.re$pca)), paste0(id, "_all_pc"))
        store_plots(suppressWarnings(pca_hc_heatmap(.re$pca)), paste0(id, "_hc_heatmap"))
      })
      
      tagList(
        plotlyOutput(ns("scat"), 500, 300),
        plotlyOutput(ns("all_pc"), 40 * n() + 80, 300),
        plotlyOutput(ns("hc_heatmap"), 30 * n() + 150, 30 * (n() - 1) + 150)
      )
    })
    
    output$scat = renderPlotly({
        cat("Rendering PCA plot...\n")
        .pl[[paste0(id, "_scat")]]
      # }
    })
    
    output$all_pc = renderPlotly({
      cat("Rendering all-PC plot...\n")
      .pl[[paste0(id, "_all_pc")]]
      
    })
    
    output$hc_heatmap = renderPlotly({
      cat("Rendering PCA HC heatmap...\n")
      .pl[[paste0(id, "_hc_heatmap")]]
    })
  })
}

