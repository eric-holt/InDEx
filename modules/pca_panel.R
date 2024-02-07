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
      tabsetPanel(
        tabPanel("All included features", pca_ui(ns, "pca_all")),
        tabPanel("LRT-significant features", pca_ui(ns, "pca_lrt")),
        tabPanel("LRT-significant feature hierarchical clustering", hc_ui(ns))
      )
    })
    
    pca_server(dds_all, "pca_all")
    pca_server(dds_lrt, "pca_lrt")
    hc_server(dds_lrt, "hc")
  })
}
