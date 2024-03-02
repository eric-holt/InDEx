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
    
    no_data = reactive({
      is.null(dt_sig()) || is.null(dt_lrt_sig())
    })

    dt = reactive({
      req(!no_data(), ncol(dt_sig()) == ncol(dt_lrt_sig()))
      rbind(dt_sig(), dt_lrt_sig())
    })
    
    no_sig = reactiveVal(T)
    observe({
      req(dt())
      no_sig(nrow(dt()) == 0)
    })
    
    output$UI = renderUI({
      if(no_data()){
        return(caution("No data"))
      } else if(no_sig()){
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
    

    ui_sig = function(){
      
      
    }
    
    output$ui_sig = renderUI(ui_sig())
    
    render_sig = function(dt, l){
      cat("Rendering", as.character(l), "sig datatable...\n")
      dt[label == l] |> hyperlink_gene_name() |> 
        dplyr::select(gene_name, baseMean, log2FoldChange, padj) |>
        datatable(escape = F, rownames = F,
                  options = list(search = list(regex = T, smart = T))) |>
        formatSignif(2:4, digits = 3)
    }
    
    observe({
      req(dt(), nrow(dt()))
      unique(dt()$label) |> lapply(function(l){
        output[[l]] = renderDT(render_sig(dt(), l))
      })
    })

    selected = reactiveVal()
    observe({
      req(dt_sig())
      unique(dt_sig()$label) |> lapply(function(l){
        dt = dt_sig()[label == l]
        idx = input[[paste0(l, "_rows_selected")]]
        dt[idx, feature_id]
      }) %>% unlist %>% unique %>% selected
    }) |> debounce(1000)
    
    return(selected)
  })
}

