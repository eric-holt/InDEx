pca_panel_ui = function(ns = identity, id = "pca_panel"){
  id = ns(id)
  ns = NS(id)
  tagList(
    if (debugging) debug_ui(ns),
    uiOutput(ns("UI"))
  )
}

pca_panel_server = function(dds_all, dds_lrt, id = "pca_panel") {
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    if (debugging) debug_server(environment())
    
    output$UI = renderUI({
      if(is.null(dds_all()) && is.null(dds_lrt())){
        return(caution("No data"))
      }
      tagList(
        actionButton(ns("btn_export"), "Export plots"),
        pca_ui(ns, "All included genes", "pca_all"),
        # pca_ui(ns, "LRT-significant genes", "pca_lrt"),
        # hc_ui(ns)
      )
    })
    
    plots_all = pca_server(dds_all, "pca_all")
    # plots_lrt = pca_server(dds_lrt, "pca_lrt")
    # hc_server(dds_lrt, "hc")
    
    observe({
      plt_list = plots_all
      # plt_list = plots_all |> append(plots_lrt)
      
      names(plt_list) |> lapply(function(name){
        plt = plt_list[[name]]
        if(is.ggplot(plt())){
          dir = here("projects", .project, "plots", "PCA")
          path = here(dir, paste0(name, ".png"))
          ggsave(path, plt())
          cat("Saved", path, "\n")
        }
      })
    }) |> bindEvent(input$btn_export)
  })
}
