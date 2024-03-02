pca_ui = function(ns = identity, id = "pca"){
  id = ns(id)
  ns = NS(id)
  tagList(
    if (debugging) debug_ui(ns),
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
      cat("Computing PCs...\n")
      get_pca(dds())
    })
    
    # Data identity for checking the cache need
    pca_identity = reactive({
      req(dds())
      c(nrow(dds()), colnames(dds()))
    })
    
    observe({
      req(pca_identity())
      write_cache(pca, ns("pca"), pca_identity())
      .re$pca <<- pca()
    })
    
    # Number of PCs for plot scaling
    n = reactive({
      length(.re$pca$explained_var)
    })
    
    # All plots in one UI for data caching and lazy reaction
    output$plots = renderUI({
      req(pca_identity())
      tagList(
        plotlyOutput(ns("scat"), 500, 300),
        plotlyOutput(ns("all_pc"), 40 * n() + 80, 300),
        plotlyOutput(ns("hc_heatmap"), 30 * n() + 150, 30 * (n() - 1) + 150)
      )
    })
    
    output$scat = renderPlotly({
      cat("Rendering PCA plot...\n")
      store_plots(suppressWarnings(gg_pca(read_cache(ns("pca")))), paste0(id, "_scat"), plotly_pca)
      
      .pl[[paste0(id, "_scat")]]
      # }
    })
    
    output$all_pc = renderPlotly({
      cat("Rendering all-PC plot...\n")
      store_plots(suppressWarnings(gg_all_pc(read_cache(ns("pca")))), paste0(id, "_all_pc"), plotly_all_pc)
      .pl[[paste0(id, "_all_pc")]]
      
    })
    
    output$hc_heatmap = renderPlotly({
      cat("Rendering PCA HC heatmap...\n")
      store_plots(suppressWarnings(pca_hc_heatmap(read_cache(ns("pca")))), paste0(id, "_hc_heatmap"))
      .pl[[paste0(id, "_hc_heatmap")]]
    })
  })
}

