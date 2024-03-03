four_way_ui = function(ns = identity, id = "4way"){
  id = ns(id)
  ns = NS(id)
  tagList(
    if (debugging) debug_ui(ns),
    uiOutput(ns("UI"))
  )
}

four_way_server = function(a, lfc, selected_sig, id = "4way"){
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
    
    # Cached DESeq2 result
    dt_res = reactive({
      .cache_time$dt_res
      read_cache("dt_res")
    })
    
    # UI
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
                       column(4,
                              checkboxInput(ns("chk_interval"), "Show intervals")),
                       column(4,
                              checkboxInput(ns("chk_lf_se"), "Show LFC standard errors")),
                       column(4,
                              radioButtons(ns("rbn_p_lfc"), "Metric", c("Signed log(p)", "LFC"), "Signed log(p)"))
                     )),
                   uiOutput(ns("out"))),
            column(4,
                   h4("Outliers"),
                   uiOutput(ns("ui_outlier"))))        )
      }
    })
    
    # Plot output UI
    output$out = renderUI({
      req(x(), y())
      if(x() == y()) caution("Cannot compare the same contrast")
      else plotlyOutput(ns("plot"), 720, 720)
    })
    
    # Axis UI
    output$ui_axis = renderUI({
      cat("Rendering the contrast axis UI...\n")
      .project_load_complete()
      ui_axis(ns, .contrasts)
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
    
    metric = reactive(input$rbn_p_lfc)
    observe_input(ns("rbn_p_lfc"), metric)
    
    # Disable the LFC standard errors when the metric is not LFC
    observe({
      req(metric())
      if(metric() == "LFC") enable("chk_lf_se")
      else disable("chk_lf_se")
    })
    
    # 4-way plot (x vs y) dataset
    dt = reactive({
      req(x(), y(), a(), lfc(), x() != y(), metric())
      cat(sprintf("Creating the %s vs %s dataset...\n", x(), y()))
      dt_4way(dt_res(), x(), y(), a(), lfc(), metric()) |> set_to_export("dt_4way")
    }) |> debounce(1000)
    
    # Data points outside the prediction interval
    dt_outlier = reactive({
      req(dt(), x(), y(), conf())
      ol = pred_outliers(dt(), x(), y(), conf()) %>% suppressWarnings
      cat(sprintf("%d outliers found\n", nrow(ol)))
      ol
    })

    # Plot; isolate the reactive values used for data generation to avoid double rendering
    output$plot = renderPlotly({
      req(dt(), selected(), x(), y(), conf())
      cat("Rendering the 4-way plot...\n")
      store_plots(gg_4way(dt(), isolate(x()), isolate(y()), isolate(a()), isolate(lfc()), selected(), conf(), show_int(), show_se(), metric()), "_4way", plotly_4way) |> suppressWarnings()
    })
    
    # Outlier DataTable
    xy = reactive({
      req(dt_outlier())
      levels(dt_outlier()$label)[1]
    })
    yx = reactive({
      req(dt_outlier())
      levels(dt_outlier()$label)[2]
    })
    dt_xy = reactive({
      req(dt_outlier())
      dt_outlier()[label == xy()]
    })
    dt_yx = reactive({
      req(dt_outlier())
      dt_outlier()[label == yx()]
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
    
    # Selected features for emphasizing points in the plot
    selected = reactive({
      req(dt_xy(), dt_yx())
      cat("Updating selected features...\n")
      unique(c(selected_sig(), 
               dt_xy()[input$DT_xy_rows_selected, feature_id],
               dt_yx()[input$DT_yx_rows_selected, feature_id]))
    }) |> debounce(1000)
    
    # Output may be used for GO
    out = reactive({
      req(dt_outlier())
      tryCatch({
        dt_outlier()[, .(feature_id, gene_id, gene_name, label)] |> set_to_export("outliers")
        }, error = function(e) NULL) 
    })
    
    # Cache the outliers when inputs change
    observe({
      req(x(), y(), a(), lfc(), conf())
      write_cache(out, "outliers", c(x(), y(), a(), lfc(), conf()))
    })
  })
}
