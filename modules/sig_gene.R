sig_ui = function(ns = identity, id = "sig"){
  id = ns(id)
  ns = NS(id)
  tagList(
    if (debugging) debug_ui(ns),
    uiOutput(ns("UI"))
  )
}

sig_server = function(id = "sig") {
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    if (debugging) debug_server(environment())
    
    # Cached data
    data = cache("dt_sig")
    d = reactive({
      req(data())
      data()$data
    })
    
    # UI
    output$UI = renderUI({
      if(is.null(data()) || is.null(d())){
        return(caution("No data"))
      } else if(!nrow(d())){
        return(caution("No significant feature"))
      }

      tabs = unique(d()$label) |> lapply(function(l){
        n = nrow(d()[label == l])
        tabPanel(sprintf("%s (%d)", l, n), 
                 DTOutput(ns(l)))
      })

      tagList(
        h4("Significantly differential features"),
        wellPanel(
          h5("Select features to emphasize in plots"),
          do.call(tabsetPanel, tabs)
          ),
      )
    })
    
    # Render function for each label
    render_sig = function(dt, l){
      cat("Rendering", as.character(l), "sig datatable...\n")
      dt[label == l] |> hyperlink_gene_name() |> 
        dplyr::select(gene_name, baseMean, log2FoldChange, padj) |>
        datatable(escape = F, rownames = F,
                  options = list(search = list(regex = T, smart = T))) |>
        formatSignif(2:4, digits = 3)
    }

    # Render each label, responding to the reactive data
    observe({
      req(d(), nrow(d()) > 0)
      unique(d()$label) |> lapply(function(l){
        output[[l]] = renderDT(render_sig(d(), l))
      })
    })

    # Features selected in the DT tables, used for emphasis in plots
    selected = reactive({
      req(d())
      unique(d()$label) |> lapply(function(l){
        dt = d()[label == l]
        idx = input[[paste0(l, "_rows_selected")]]
        dt[idx, feature_id]
      }) %>% unlist %>% unique
    })
    
    return(selected)
  })
}
