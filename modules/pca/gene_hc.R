hc_ui = function(ns = identity, id = "hc"){
  id = ns(id)
  ns = NS(id)
  tagList(
    if (debugging) debug_ui(ns),
    uiOutput(ns("UI"))
  )
}

hc_server = function(data, id = "hc") {
  moduleServer(id, function(input, output, session) {
    if (debugging) debug_server(environment())
    
    output$UI = renderUI({
      if(is.null(data()) || !nrow(data()$data)){
        caution("No data")
      } else{
        tagList(
          fixedRow(
            column(6, 
                   plotlyOutput(ns("count"), 500, 500)),
            column(5, 
                   plotlyOutput(ns("norm"), 500, 500))
          ),
          fixedRow(
            column(3),
            column(5,
                   plotlyOutput(ns("tpm"), 500, 500))
          )
        )
      }
    })
    
    output$count = renderPlotly({
      store_plots(lrt_hc_count(data()$data), "LRT_hclust_count") |> suppressWarnings()
    })
    
    output$norm = renderPlotly({
      store_plots(lrt_hc_norm(data()$data), "LRT_hclust_norm") |> suppressWarnings()
    })
    
    output$tpm = renderPlotly({
      store_plots(lrt_hc_tpm(data()$data), "LRT_hclust_TPM") |> suppressWarnings()
    })
  })
}
