sig_ui = function(ns = identity, id = "sig"){
  id = ns(id)
  ns = NS(id)
  tagList(
    if (debugging) debug_ui(ns),
    uiOutput(ns("UI"))
  )
}

sig_server = function(dt_sig, dt_lrt_sig, id = "sig") {
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    if (debugging) debug_server(environment())
    
    # Check if the data are provided
    no_data = reactive({
      is.null(dt_sig()) || is.null(dt_lrt_sig())
    })

    # Combine Wald and LRT results
    dt = reactive({
      req(!no_data())
      
      rbind(dt_sig(), dt_lrt_sig()) |> set_to_export("dt_sig")
    })
    
    # UI
    output$UI = renderUI({
      if(no_data()){
        return(caution("No data"))
      } else if(nrow(dt()) == 0){
        return(caution("No significant feature"))
      }

      tabs = unique(dt()$label) |> lapply(function(l){
        n = nrow(dt()[label == l])
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
      req(dt(), nrow(dt()))
      unique(dt()$label) |> lapply(function(l){
        output[[l]] = renderDT(render_sig(dt(), l))
      })
    })

    # Features selected in the DT tables, used for emphasis in plots
    selected = reactive({
      req(dt_sig())
      unique(dt_sig()$label) |> lapply(function(l){
        dt = dt_sig()[label == l]
        idx = input[[paste0(l, "_rows_selected")]]
        dt[idx, feature_id]
      }) %>% unlist %>% unique
    }) |> debounce(1000)
    
    return(selected)
  })
}
