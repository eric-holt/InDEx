four_way_ui = function(ns = identity, id = "4way"){
  id = ns(id)
  ns = NS(id)
  tagList(
    if (debugging) debug_ui(ns),
    uiOutput(ns("UI"))
  )
}

four_way_server = function(dt_res, a, lfc, selected_sig, id = "4way"){
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    
    ui_axis = function(ns, cont){
      tagList(
        column(4,
               observedSelectInput(ns("sel_x"), "X-axis", cont, cont[1])),
        column(4,
               observedSelectInput(ns("sel_y"), "Y-axis", cont, cont[2]))
      )
    }
    
    if (debugging) debug_server(environment())
    
    output$UI = renderUI({
      if(is.null(dt_res())){
        caution("No data to plot")
      } else{
        tagList(
          fixedRow(
            column(8,
                   wellPanel(
                     fixedRow(
                       uiOutput(ns("ui_axis")),
                       column(4,
                              observedNumericInput(ns("num_conf"), "Confidence level", 0.95, 0, 1))),
                     fixedRow(
                       column(6,
                              checkboxInput(ns("chk_interval"), "Show intervals")),
                       column(6,
                              checkboxInput(ns("chk_lf_se"), "Show LFC standard errors"))
                     )),
                   uiOutput(ns("out"))),
            column(4,
                   h4("Outliers"),
                   uiOutput(ns("ui_outlier"))))        )
      }
    })
    
    output$out = renderUI({
      req(x(), y())
      if(x() == y()) caution("Cannot compare the same contrast")
      else plotlyOutput(ns("plot"), 720, 720)
    })
    
    output$ui_axis = renderUI({
      cat("Rendering the contrast axis UI...\n")
      ui_axis(ns, .g$contrasts)
    })
    
    # Inputs
    x = reactive(to_underscore(input$sel_x))
    observe_input(ns("sel_x"), x)
    
    y = reactive(to_underscore(input$sel_y))
    observe_input(ns("sel_y"), y)
    
    conf = reactive(input$num_conf)
    observe_input(ns("num_conf"), conf)
    
    show_int = reactive(input$chk_interval)
    observe_input(ns("chk_interval"), show_int)

    show_se = reactive(input$chk_lf_se)
    observe_input(ns("chk_lf_se"), show_se)
    
    # 4-way plot (x vs y) dataset
    dt = reactiveVal()
    observe({
      req(dt_res(), x(), y(), a(), lfc(), x() != y())
      cat(sprintf("Creating the %s vs %s dataset...\n", x(), y()))
      dt_4way(dt_res(), x(), y(), a(), lfc()) %>% dt
    }) |> debounce(1000)
    
    # Data points outside the prediction interval
    dt_outlier = reactiveVal()
    observe({
      req(dt(), conf())
      pred_outliers(dt(), isolate(x()), isolate(y()), conf()) %>% suppressWarnings %>% dt_outlier
      cat(sprintf("%d outliers found\n", nrow(dt_outlier())))
    }) |> debounce(1000)
    
    # Plot
    output$plot = renderPlotly({
      req(dt(), selected(), conf())
      cat("Rendering the 4-way plot...\n")
      store_plots(suppressWarnings(gg_4way(dt(), isolate(x()), isolate(y()), isolate(a()), isolate(lfc()), selected(), conf(), show_int(), show_se())), "_4_way", plotly_4way)
      .pl[["_4_way"]]
    })
    
    # Outlier DataTable
    xy = reactiveVal()
    observe({
      req(dt_outlier())
      levels(dt_outlier()$label)[1] %>% xy
    })
    yx = reactiveVal()
    observe({
      req(dt_outlier())
      levels(dt_outlier()$label)[2] %>% yx
    })
    dt_xy = reactiveVal()
    observe({
      req(dt_outlier())
      dt_outlier()[label == xy()] %>% dt_xy
    })
    dt_yx = reactiveVal()
    observe({
      req(dt_outlier())
      dt_outlier()[label == yx()] %>% dt_yx
    })
    
    output$ui_outlier = renderUI({
      req(xy(), yx())
      cat("Creating the outlier tabset UI...\n")
      tabsetPanel(
        tabPanel(xy(), DTOutput(ns("DT_xy"))),
        if(!is.na(yx())) tabPanel(yx(), DTOutput(ns("DT_yx")))
      )
    })
    
    DT_out = function(dt){
      dt |> hyperlink_gene_name() |> dplyr::select(gene_name) |> 
        datatable(escape = F, rownames = F,
                  options = list(search = list(regex = T, smart = T))) 
    }
    output$DT_xy = renderDT({
      req(dt_xy())
      cat("Rendering the xy outlier datatable...\n")
      DT_out(dt_xy())
    })
    output$DT_yx = renderDT({
      req(dt_yx())
      cat("Rendering the yx outlier datatable...\n")
      DT_out(dt_yx())
    })
    
    # Selected features
    selected = reactiveVal()
    observe({
      req(dt_xy(), dt_yx())
      cat("Updating selected features...\n")
      unique(c(selected_sig(), 
               dt_xy()[input$DT_xy_rows_selected, feature_id],
               dt_yx()[input$DT_yx_rows_selected, feature_id])) %>% selected
    }) |> debounce(1000)
    
    # Output may be used for GO
    out = reactiveVal()
    observe({
      tryCatch(dt_outlier()[, .(feature_id, gene_id, gene_name, label)], 
      error = function(e) NULL) %>% out 
    })
    
    return(out)
  })
}

