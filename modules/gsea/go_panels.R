go_panels_ui = function(ns = identity, id){
  ns = NS(ns(id))
  tagList(
    if (debugging) debug_ui(ns),
    uiOutput(ns("ui_go_panels"))
  )
}

go_panels_server = function(data, p, q, n, sort, id) {
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
      
      # Recursively create tabsetPanel
      make_tbs = function(list_obj, name = ""){
        if(is.list(list_obj) && !"data.frame" %in% class(list_obj)){
          names(list_obj) |> lapply(function(n){
            name = paste(name, n, sep = "_") |> str_remove("^_")
            label =
              if(n == "BP") "Biological Process" else
                if(n == "CC") "Cellular Component" else
                  if(n == "MF") "Molecular Function" else
                    n
            tabPanel(label, make_tbs(list_obj[[n]], name))
          }) %>% do.call(tabsetPanel, .)
        } else {
          tabPanel(name, gs_plot_ui(ns, name))
        }
      }
      
      make_tbs(d())
    }
    
    output$ui_go_panels = renderUI(ui_go_panels())
    
    # Plot for each contrast and ontology
    observe({
      req(d(), n(), sort(), p(), q())
      
      # Recursively create server
      make_server = function(list_obj, name = ""){
        if(is.list(list_obj) && !"data.frame" %in% class(list_obj)){
          names(list_obj) |> lapply(function(n){
            name = paste(name, n, sep = "_") |> str_remove("^_")
            dt = list_obj[[n]]
            make_server(list_obj[[n]], name)
          })
        } else {
          dt = reactive({
            get_dt_top_n_go(list_obj, n(), sort(), p(), q())
          })
          gs_plot_server(dt, name)
        }
      }
      make_server(d())
    })
  })
}

