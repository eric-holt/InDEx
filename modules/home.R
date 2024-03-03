home_ui = function(ns = identity, id = "home"){
  id = ns(id)
  ns = NS(id)
  
  tagList(
    if (debugging) debug_ui(ns),
    h1(HTML("<center>InDEx</center>")),
    h5(HTML("<center><b>In</b>teractive <b>D</b>ifferential <b>Ex</b>plorer</center>")),
    import_ui(ns),
    uiOutput(ns("meta"))
  )
}

home_server = function(id = "home") {
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    if (debugging) debug_server(environment())
    
    # Import
    import_server()
    
    output$meta = renderUI({
      meta = list(h3("Project metadata"))
      for(name in names(.metadata)){
        meta[[length(meta) + 1]] = h5(paste0(name, ":"))
        meta[[length(meta) + 1]] = h4(.metadata[[name]])
        meta[[length(meta) + 1]] = span(HTML("<br>"))
      }
      do.call(tagList, meta)
    }) |> bindEvent(.project_load_complete())
  })
}