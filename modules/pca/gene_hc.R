hc_ui = function(ns = identity, id = "hc"){
  id = ns(id)
  ns = NS(id)
  tagList(
    if (debugging) debug_ui(ns),
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

hc_server = function(dds_sig, id = "hc") {
  moduleServer(id, function(input, output, session) {
    if (debugging) debug_server(environment())
    output$count = renderPlotly(gene_hc_count(dds_sig()))
    output$norm = renderPlotly(gene_hc_norm(dds_sig()))
    output$tpm = renderPlotly(gene_hc_tpm(dds_sig()))
  })
}

