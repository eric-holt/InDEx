go_panels_ui = function(ns = identity, id = "go_panels"){
  ns = NS(ns(id))
  tagList(
    if (debugging) debug_ui(ns),
    uiOutput(ns("ui_go_panels"))
  )
}

go_panels_server = function(data_list, p, q, n, sort, id = "go_panels") {
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    if (debugging) debug_server(environment())
    
    ui_go_panels = function(){
      ui_ont = function(ont){
        names(data_list) |> lapply(function(l){
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
    
    dt_top_n = reactiveVal()
    observe({
      req(data_list, n(), sort(), p(), q())
      names(data_list) |> lapply(function(l){
        names(data_list[[l]]) |> lapply(function(ont){
          dt = get_dt_top_n_go(data_list[[l]][[ont]], n(), sort(), p(), q())
          if(is.null(dt)) return()
          dt[, `:=`(label = l, ontology = ont)]
        }) %>% rbindlist
      }) %>% rbindlist %>% dt_top_n
    }) |> debounce(1000)
    
    plt_list = reactiveVal()
    observe({
      req(dt_top_n())
      labels = unique(dt_top_n()$label)
      labels |> lapply(function(label){
        ontologies = unique(dt_top_n()$ontology)
        ontologies |> lapply(function(ontology){
          local({
            l = label
            ont = ontology
            gs_plot_server(
              reactive(dt_top_n()[label == l & ontology == ont]), 
              paste(l, ont, sep = "_"))
          })
        }) |> setNames(ontologies)
      }) |> setNames(labels) %>% plt_list
    })

    return(reactive(plt_list()))
  })
}

