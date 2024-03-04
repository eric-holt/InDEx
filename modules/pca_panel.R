pca_panel_ui = function(ns = identity, id = "pca_panel"){
  id = ns(id)
  ns = NS(id)
  tagList(
    if (debugging) debug_ui(ns),
    uiOutput(ns("UI"))
  )
}

pca_panel_server = function(data_all, data_lrt, id = "pca_panel") {
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    if (debugging) debug_server(environment())
    
    output$UI = renderUI({
      if(is.null(data_all()) && is.null(data_lrt())){
        return(caution("No data"))
      }
      tabsetPanel(
        tabPanel("All included features", pca_ui(ns, "pca_all")),
        tabPanel("LRT-significant features", pca_ui(ns, "pca_lrt")),
        tabPanel("LRT-significant feature hierarchical clustering", hc_ui(ns))
      )
    })
    
    pca_server(data_all, "pca_all")
    pca_server(data_lrt, "pca_lrt")
    hc_server(data_lrt, "hc")
  })
}
