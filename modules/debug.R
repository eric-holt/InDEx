debug_ui = function(ns = identity, id = "debug"){
  id = ns(id)
  ns = NS(id)
  uiOutput(ns)
  actionLink(ns("lnk"), "Debug", style = "float: right; z-index: 1000; position: relative;")
}

debug_server = function(env, id = "debug"){
  moduleServer(id, function(input, output, session){
    ns = session$ns
    loc = str_remove(ns(""), "-?debug-$")
    modal = function(){
      modalDialog(
        sidebarLayout(
          sidebarPanel(
            textAreaInput(ns("code"), paste("Debug code in:", loc), rows = 10, resize = "vertical")
          ),
          mainPanel(
            verbatimTextOutput(ns("out"), T)
          )
        )
      )
    }
    
    show_debug_window = function(){
      cat("Debugging ID:", loc, "\n")
      showModal(modal())
    }
    
    observe(show_debug_window()) |> bindEvent(input$lnk)
    
    output$out = renderPrint(
      tryCatch({
        eval(parse(text = input$code), envir = env)
      }, error = function(e) {
        paste("Error:", e$message)
      })
    )
  })
}
