go_panels_ui = function(ns = identity, id = "go_panels"){
  ns = NS(ns(id))
  tagList(
    if (debugging) debug_ui(ns),
    uiOutput(ns("ui_go_panels"))
  )
}

go_panels_server = function(data, p, q, n, sort, id = "go_panels") {
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    if (debugging) debug_server(environment())
    
    # Shorthand for data
    d = reactive({
      req(data())
      data()$data
    })
    
    # UI
    ui_go_panels = function(){
      if(is.null(data()) || is.null(d()))
        return(caution("No data has been cached"))
      ui_ont = function(ont){
        names(d()) |> lapply(function(l){
          tabPanel(l, gs_plot_ui(ns, paste(l, ont, sep = "_")))
        }) %>% do.call(tabsetPanel, .)
      }
      tabsetPanel(
        tabPanel("Biological Process", ui_ont("BP")),
        tabPanel("Cellular Component", ui_ont("CC")),
        tabPanel("Molecular Function", ui_ont("MF")), 
        id = ns("ontology")
      )
    }
    
    output$ui_go_panels = renderUI(ui_go_panels())
    
    # Data for plots including top n terms
    dt_top_n = reactive({
      req(d(), n(), sort(), p(), q())
      names(d()) |> lapply(function(l){
        names(d()[[l]]) |> lapply(function(ont){
          dt = get_dt_top_n_go(d()[[l]][[ont]], n(), sort(), p(), q())
          if(is.null(dt)) return()
          dt[, `:=`(label = l, ontology = ont)]
        }) %>% rbindlist
      }) %>% rbindlist
    })
    
    # Plot for each contrast and ontology
    observe({
      req(dt_top_n())
      unique(dt_top_n()$label) |> lapply(function(label){
        unique(dt_top_n()$ontology) |> lapply(function(ontology){
          local({
            l = label
            ont = ontology
            gs_plot_server(
              reactive({
                req(dt_top_n())
                dt_top_n()[label == l & ontology == ont]
              }), 
              paste(l, ont, sep = "_"))
          })
        })
      })
    })
  })
}

