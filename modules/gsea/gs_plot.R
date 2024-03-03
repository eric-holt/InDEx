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
    
    # UI
    output$ui_term_plots = renderUI({
      req(dt())
      h = max(38 * nrow(dt()) + 20, 300)
      fixedRow(
        column(6, plotlyOutput(ns("plotly"), height = h)),
        column(6, DTOutput(ns("DT"))))
    })
    
    # Plot
    output$plotly = renderPlotly({
      req(dt())
      cat("Rendering", id, "dot plot...\n")
      plot_id = str_split_1(ns(id), "-")[2:3] |> paste(collapse = "_")
      store_plots(gg_dotplot(dt()), plot_id, plotly_dotplot) |> suppressWarnings()
    })
    
    output$DT = renderDT({
      req(dt())
      cat("Rendering", id, "datatable...\n")
      d = dt() %>% hyperlink_go_term
      d[, .(Description, qvalue, setSize)] |>
        datatable(escape = F, rownames = F, selection = "none",
                  options = list(search = list(regex = T, smart = T))) |> 
        formatSignif(2, digits = 3)
    })
  })
}
