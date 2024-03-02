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
      h4(HTML(sprintf("<center>%s</center>", .project)))
    )
  }) |> bindEvent(.project_load_flag())
  
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
  
  # only_sig = reactive(input$chk_only_sig)
  # observe_input("chk_only_sig", only_sig)
  
  go_pred = reactive(input$chk_go_pred)
  observe_input("chk_go_pred", go_pred)
  
  # Set LRT α to the same value as Wald
  observe({
    req(input$chk_same_as_wald)
    updateNumericInput(session, "num_lrt_alpha", value = input$num_alpha)
  })

  # Disable LRT α input when set to be the same as Wald
  observe({
    req(!is.null(input$chk_same_as_wald))
    if (input$chk_same_as_wald) {
      disable("num_lrt_alpha")
    } else {
      enable("num_lrt_alpha")
    }
  })
  
  # selected_tab = reactive(input$tbs_main)
  # observe_input("tbs_main", selected_tab)
  
  # Home
  home_server()
  
  # Data filtering
  # filtered_data = data_server()
  data_server()
  
  
  dds = reactive({
    features = read_cache("included")
    samples = read_cache("samples")
    req(features, samples, .dt_count)
    cat("Updating filtered features...\n")
    .re$genes <<- unique(.dt_count[feature_id %in% features, gene_id])
    .dds[features, samples]
  })
  
  # observe({
  #   features = read_cache("included")
  #   samples = read_cache("samples")
  #   req(features, samples, .dt_count)
  #   cat("Updating filtered features...\n")
  #   .r$dds = .dds[features, samples]
  #   .re$genes <<- unique(.dt_count[feature_id %in% features, gene_id])
  # })
  
  # observe({
  #   req(filtered_data(), .dt_count)
  #   cat("Updating filtered features...\n")
  #   .r$dds = filtered_data()
  #   .re$genes <<- unique(.dt_count[feature_id %in% rownames(filtered_data()), gene_id])
  # })
  
  dt_lrt_sig = reactive({
    req(dds(), lrt_a())
    cat("Updating dt_lrt_sig...\n")
    .re$dt_lrt_sig <<- get_dt_lrt(dds(), lrt_a())
    .re$dt_lrt_sig
  })
  
  # observe({
  #   req(.r$dds, lrt_a())
  #   cat("Updating dt_lrt_sig...\n")
  #   .re$dt_lrt_sig <<- get_dt_lrt(.r$dds, lrt_a())
  # })
  
  dds_lrt_sig = reactive({
    req(dds(), dt_lrt_sig())
    cat("Updating dds_lrt_sig...\n")
    dds()[dt_lrt_sig()$feature_id, ]
  })
  
  # observe({
  #   req(.r$dds, .re$dt_lrt_sig)
  #   cat("Updating dds_lrt_sig...\n")
  #   .r$dds_lrt_sig = .r$dds[.re$dt_lrt_sig[, feature_id], ]
  # })
  
  # observe({
  #   req(.r$dds_lrt_sig)
  #   cat("Updating res_lrt...\n")
  #   .r$res_lrt = get_res(.r$dds_lrt_sig)
  # })
  
  res = reactive({
    req(dds())
    cat("Updating DESeq2 results...\n")
    get_res(dds())
  })
  
  # observe({
  #   req(.r$dds)
  #   cat("Updating DESeq2 results...\n")
  #   .r$res = get_res(.r$dds)
  # })
  
  # observe({
  #   req(.r$res_lrt)
  #   cat("Updating dt_res_lrt...\n")
  #   .re$dt_res_lrt <<- dt_all_results(.r$res_lrt)
  # })
  
  dt_res = reactive({
    req(res())
    cat("Updating dt_res...\n")
    dt_all_results(res())
  })
  
  observe({
    req(dt_res())
    write_cache(dt_res, "dt_res", c(cache_identity("included"), read_cache("samples")))
    .re$dt_res <<- dt_res()
  })
  
  # observe({
  #   req(.r$res)
  #   cat("Updating dt_res_all...\n")
  #   .re$dt_res <<- dt_all_results(.r$res)
  # })
  
  # observe({
  #   # if(only_sig()){
  #   #   req(.re$dt_res_lrt)
  #   #   cat("Updating dt_res with dt_res_lrt...\n")
  #   #   .r$dt_res = .re$dt_res_lrt 
  #   # }
  #   # else{
  #     req(.re$dt_res_all)
  #     cat("Updating dt_res with dt_res_all...\n")
  #     .r$dt_res = .re$dt_res_all 
  #   # }
  # })
  
  # PCA
  pca_panel_server(dds, dds_lrt_sig)
  
  # Significant 
  dt_sig = reactive({
    req(dt_res(), a())
    cat("Updating dt_sig...\n")
    .re$dt_sig <<- get_dt_sig(dt_res(), a())
    .re$dt_sig
  })
  
  # observe({
  #   req(.r$dt_res)
  #   .re$dt_sig <<- get_dt_sig(.r$dt_res, a(), lfc())
  # }) |> debounce(1000)
  
  selected = sig_server(dt_sig, dt_lrt_sig)
  
  # Plots
  four_way_server(a, lfc, selected)
   
  volcano_ma_server(a, lfc, selected)
  
  # clusterProfiler
  dt_go = reactive({
    if(go_pred()) read_cache("outliers")
    else dt_sig()
  })
  gsea_server(dt_go, .re$genes, read_cache("dt_res"))
  
}
