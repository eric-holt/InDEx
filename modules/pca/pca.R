pca_ui = function(ns = identity, label = "", id = "pca"){
  id = ns(id)
  ns = NS(id)
  tagList(
    if (debugging) debug_ui(ns),
    tags$h4(label),
    fixedRow(column(5, plotlyOutput(ns("scat_all"), 488, 300)),
             column(4, plotlyOutput(ns("pc_all"), 390, 300)),
             column(3, plotlyOutput(ns("heatmap_all"), 400, 300)))
  )
}

# pca_ui = function(ns = identity, label = "", id = ns("pca")){
#   ns = NS(id)
#   tagList(
#     if (debugging) debug_ui(ns),
#     tags$h4(label),
#     fixedRow(column(5, plotlyOutput(ns("scat_all"), 488, 300)),
#              column(4, plotlyOutput(ns("pc_all"), 390, 300)),
#              column(3, plotlyOutput(ns("heatmap_all"), 400, 300)))
#   )
# }

pca_server = function(dds, id = "pca") {
  moduleServer(id, function(input, output, session) {
    if (debugging) debug_server(environment())
    
    object = reactive({
      req(dds())
      get_pca(dds())
    })
    
    data_identity = reactive({
      req(dds())
      c(nrow(dds()), colnames(dds()))
    })
    
    pca = reactive({
      req(data_identity())
      auto_cache(object, data_identity(), id)
    })
    
    output$scat_all = renderPlotly({
      plotly_pca(pca()) %>% suppressWarnings
    })
    output$pc_all = renderPlotly({
      plotly_all_pc(pca()) %>% suppressWarnings
    })
    output$heatmap_all = renderPlotly({
      pca_hc_heatmap(pca())
    })
    
    return(
      list(reactive(ggplot_pca(pca())),
           reactive(ggplot_all_pc(pca()))) |> 
        setNames(paste(id, c("scat", "bar")))
    )
  })
}

