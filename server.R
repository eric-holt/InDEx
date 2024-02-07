server = function(input, output, session){
  if (debugging) debug_server(environment())
  
  # Auto-save the state
  state_save_ready = reactive({trigger_state_save()}) |> debounce(5000)
  observe(if(state_save_ready()) save_input_state())
  
  # Load project when flagged
  observe(if(.project_load_flag()) load_project(input))
  
  # Show the current project
  output$project_name = renderUI({
    tagList(
      h5(HTML("<center>Current project:</center>")),
      h3(HTML(sprintf("<center>%s</center>", .project)))
    )
  }) |> bindEvent(.project_load_flag())
  
  # Export all `re` reactive values
  # observe({
  #   names(.re) |> lapply(export_reactives)
  # }) |> bindEvent(input$btn_export)
  
  # Export reactive objects
  observe({
    if("data" %in% input$cbg_export) export_all_data()
    if("ggplot" %in% input$cbg_export) export_all_gg()
    if("plotly" %in% input$cbg_export) export_all_pl()
  }) |> bindEvent(input$btn_export) |> throttle(5000)

  # Inputs
  a = reactive(as.numeric(input$num_alpha))
  observe_input("num_alpha", a)
  
  lrt_a = reactive(as.numeric(input$num_lrt_alpha))
  observe_input("num_lrt_alpha", lrt_a)

  lfc = reactive(as.numeric(input$num_lfc))
  observe_input("num_lfc", lfc)
  
  only_sig = reactive(input$chk_only_sig)
  observe_input("chk_only_sig", only_sig)
  
  go_pred = reactive(input$chk_go_pred)
  observe_input("chk_go_pred", go_pred)
  
  selected_tab = reactive(input$tbs_main)
  observe_input("tbs_main", selected_tab)
  
  .r = reactiveValues()
  
  # Home
  home_server()
  
  # Data filtering
  filtered_data = data_server()
  
  observe({
    req(filtered_data(), .dt_count)
    cat("Updating filtered features...\n")
    .r$dds = filtered_data()
    .re$genes <<- unique(.dt_count[feature_id %in% rownames(filtered_data()), gene_id])
  })

  observe({
    req(.r$dds, lrt_a())
    cat("Updating dt_lrt_sig...\n")
    .re$dt_lrt_sig <<- get_dt_lrt(.r$dds, lrt_a())
  })
  
  observe({
    req(.r$dds, .re$dt_lrt_sig)
    cat("Updating dds_lrt_sig...\n")
    .r$dds_lrt_sig = .r$dds[.re$dt_lrt_sig[, feature_id], ]
  })

  observe({
    req(.r$dds_lrt_sig)
    cat("Updating res_lrt...\n")
    .r$res_lrt = get_res(.r$dds_lrt_sig)
  })
  
  observe({
    req(.r$dds)
    cat("Updating res_all...\n")
    .r$res_all = get_res(.r$dds)
  })
  
  observe({
    req(.r$res_lrt)
    cat("Updating dt_res_lrt...\n")
    .re$dt_res_lrt <<- dt_all_results(.r$res_lrt)
  })
  
  observe({
    req(.r$res_all)
    cat("Updating dt_res_all...\n")
    .re$dt_res_all <<- dt_all_results(.r$res_all)
  })
  
  observe({
    if(only_sig()){
      req(.re$dt_res_lrt)
      cat("Updating dt_res with dt_res_lrt...\n")
      .r$dt_res = .re$dt_res_lrt 
    }
    else{
      req(.re$dt_res_all)
      cat("Updating dt_res with dt_res_all...\n")
      .r$dt_res = .re$dt_res_all 
    }
  })
  
  # PCA
  pca_panel_server(reactive(.r$dds), reactive(.r$dds_lrt_sig))

  # Significant genes
  observe({
    req(.r$dt_res)
    .re$dt_sig <<- get_dt_sig(.r$dt_res, a(), lfc())
  }) |> debounce(1000)

  selected = sig_server(reactive(.re$dt_sig), reactive(.re$dt_lrt_sig))

  # Plots
  dt_outlier = four_way_server(reactive(.r$dt_res), a, lfc, selected)
  observe({
    .re$dt_outlier = dt_outlier()
  })
  volcano_ma_server(reactive(.r$dt_res), a, lfc, selected)

  # clusterProfiler
  dt_go = reactive({
    if(go_pred()) .re$dt_outlier
    else .re$dt_sig
  })
  gsea_server(dt_go, reactive(.re$genes), reactive(.re$dt_res_all))
  
}
