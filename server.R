server = function(input, output, session){
  if (debugging) debug_server(environment())
  
  # Auto-save the state
  state_save_ready = reactive({trigger_state_save()}) |> debounce(5000)
  observe(if(state_save_ready()) save_input_state())
  
  # Load project when flagged
  observe({
    if(.project_load_flag()) load_project(input)
  })
  
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
  
  # Home
  home_server()
  
  # Data filtering
  data_server()
  
  # Read cache when the cache time is updated
  included = reactive({
    .cache_time$included
    read_cache("included")
  })
  
  samples = reactive({
    .cache_time$samples
    read_cache("samples")
  })
  
  genes = reactive({
    .cache_time$genes
    read_cache("genes")
  })
  
  # DESeq2 chain to update the result data.table
  dds = reactive({
    req(included(), samples())
    cat("Subsetting DESeqDataSet...\n")
    .dds[included(), samples()]
  })
  
  res = reactive({
    req(dds())
    cat("Updating DESeq2 results...\n")
    get_res(dds())
  })
  
  dt_res = reactive({
    req(res())
    cat("Updating data.table for DESeq2 results...\n")
    dt_all_results(res()) |> set_to_export("dt_res")
  })
  
  # Save the result data.table to cache when the filter is updated
  observe({
    req(dds_identity())
    write_cache(dt_res, "dt_res", dds_identity())
  })
  
  # Significant features, responding to filter and α input changes
  dt_sig = reactive({
    req(dt_res(), a())
    cat("Updating data.table for significant features...\n")
    get_dt_sig(dt_res(), a())
  })
  
  dt_lrt_sig = reactive({
    req(dds(), lrt_a())
    cat("Updating data.table for LRT-significant features...\n")
    get_dt_lrt(dds(), lrt_a())
  })
  
  # Subset of DESeqDataSet for LRT-significant features, used for PCA
  dds_lrt_sig = reactive({
    req(dds(), dt_lrt_sig())
    cat("Subsetting DESeqDataSet for LRT-significant features...\n")
    dds()[dt_lrt_sig()$feature_id, ]
  })
  
  # PCA
  pca_panel_server(dds, dds_lrt_sig)
  
  
  selected = sig_server(dt_sig, dt_lrt_sig)
  
  # Plots
  four_way_server(a, lfc, selected)
   
  volcano_ma_server(a, lfc, selected)
  
  outliers = reactive({
    req(.chache_time$outliers)
    read_cache("outliers")
  })
  
  # clusterProfiler
  dt_go = reactive(if(go_pred()) outliers else dt_sig)
  gsea_server(dt_go(), genes)
}
