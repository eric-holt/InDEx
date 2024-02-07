home_ui = function(ns = identity, id = "home"){
  id = ns(id)
  ns = NS(id)
  
  tagList(
    if (debugging) debug_ui(ns),
    h1(HTML("<center>InDEx</center>")),
    h4(HTML("<center><b>In</b>teractive <b>D</b>ifferential <b>Ex</b>plorer</center>")),
    
    import_ui(ns)
  )
}

home_server = function(id = "home") {
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    if (debugging) debug_server(environment())
    
    # Import
    import_server()
 })
}