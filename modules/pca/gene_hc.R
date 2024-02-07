hc_ui = function(ns = identity, id = "hc"){
  id = ns(id)
  ns = NS(id)
  tagList(
    if (debugging) debug_ui(ns),
    tags$h4("LRT-significant gene hierarchical clustering"),
    fixedRow(column(4, plotlyOutput(ns("count"), 390, 400)),
             column(4, plotlyOutput(ns("norm"), 390, 400)),
             column(4, plotlyOutput(ns("tpm"), 390, 400)))
  )
}

hc_server = function(dds_sig, id = "hc") {
  moduleServer(id, function(input, output, session) {
    if (debugging) debug_server(environment())
    output$count = renderPlotly(gene_hc_count(dds_sig()))
    output$norm = renderPlotly(gene_hc_norm(dds_sig()))
    output$tpm = renderPlotly(gene_hc_tpm(dds_sig()))
  })
}

