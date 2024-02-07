gs_plot_ui = function(ns = identity, id = "gse_plot"){
  ns = NS(ns(id))
  tagList(
    if (debugging) debug_ui(ns),
    uiOutput(ns("ui_term_plots"))
  )
}

gs_plot_server = function(dt, id) {
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    if (debugging) debug_server(environment())
    
    output$ui_term_plots = renderUI({
      req(dt())
      h = max(38 * nrow(dt()) + 20, 300)
      fixedRow(
        column(6, plotlyOutput(ns("plotly"), height = h)),
        column(6, DTOutput(ns("DT"))))
    })
    

    output$plotly = renderPlotly({
      cat("Rendering", id, "dot plot...\n")
      req(dt())
      dotplot(dt())
    })
    
    output$DT = renderDT({
      cat("Rendering", id, "datatable...\n")
      req(dt())
      d = dt() %>% hyperlink_go_term
      d[, .(Description, qvalue, setSize)] |>
        datatable(escape = F, rownames = F, selection = "none",
                  options = list(search = list(regex = T, smart = T))) |> 
        formatSignif(2, digits = 3)
    })
    
    plt = reactive(gg_dotplot(dt()))
    return(plt)
  })
}
