server = function(input, output, session){
  # Load project when flagged
  observe({
    if(.project_load_flag()) load_project(input)
  })
  
  # While loading a project, do nothing
  observe({
    req(!project_being_loaded())
    
    if (debugging) debug_server(environment())
    
    # Auto-save the state
    state_save_ready = reactive({trigger_state_save()}) |> debounce(5000)
    observe(if(state_save_ready()) save_input_state())
    
    # Show the current project
    output$project_name = renderUI({
      tagList(
        h5(HTML("<center>Current project:</center>")),
        h4(HTML(sprintf("<center>%s</center>", .project)))
      )
    }) |> bindEvent(.project_load_flag())
    
    # Export reactive objects upon button click
    observe({
      if("data" %in% input$cbg_export) export_all_data()
      if("ggplot" %in% input$cbg_export) export_all_gg()
      if("plotly" %in% input$cbg_export) export_all_pl()
    }) |> bindEvent(input$btn_export) |> throttle(5000)
    
    # Clear cache upon button click
    observe({
      clear_all_cache()
    }) |> bindEvent(input$btn_clr_cache)
    
    # Inputs
    a = reactive(as.numeric(input$num_alpha))
    observe_input("num_alpha", a)
    
    lrt_a = reactive(as.numeric(input$num_lrt_alpha))
    observe_input("num_lrt_alpha", lrt_a)
    
    lfc = reactive(as.numeric(input$num_lfc))
    observe_input("num_lfc", lfc)
    
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

    # Data processing chain----
    # Cached data from data filtering
    included = cache("included")
    samples = cache("samples")
    
    # DESeq2 chain to update the result data.table
    dds = reactive({
      req(included(), samples(), included()$data, samples()$data, length(included()$data) > 0, length(samples()$data) > 0)
      cat("Subsetting DESeqDataSet...\n")
      .dds[included()$data, samples()$data]
    })
    
    res = reactive({
      req(dds())
      cat("Updating DESeq2 results...\n")
      get_res(dds())
    })
    
    dt_res_ = reactive({
      req(res())
      cat("Updating data.table for DESeq2 results...\n")
      dt_all_results(res()) |> set_to_export("dt_res")
    })
    
    # Save the result data.table to cache when the filter is updated
    observe({
      req(included())
      write_cache(dt_res_, "dt_res", c(included()$identity, samples()$identity))
    })
    
    # Use the cache for the downstream analysis
    dt_res = cache("dt_res")
    
    # Significant features, responding to filter and α input changes
    # LRT-significant features
    dt_lrt_sig = reactive({
      req(dds(), lrt_a())
      cat("Updating data.table for LRT-significant features...\n")
      get_dt_lrt(dds(), lrt_a())
    })
    
    # Wald-significant features
    dt_sig_ = reactive({
      req(dt_res(), a())
      cat("Updating data.table for significant features...\n")
      sig = get_dt_sig(dt_res()$data, a(), lfc())
      
      # Add LRT-significant features if not empty
      if(!is.null(dt_lrt_sig()) && nrow(dt_lrt_sig())){
        sig = rbind(sig, dt_lrt_sig())
      }
      sig
    })
    
    sig_identity = reactive({
      req(dt_res(), a(), lfc())
      id = c(dt_res()$identity, a(), lfc())
      if(!is.null(dt_lrt_sig()) && nrow(dt_lrt_sig())){
        c(id, lrt_a())
      }
    })
    
    # Cache dt_sig when the thresholds are updated
    observe({
      # req(sig_identity())
      write_cache(dt_sig_, "dt_sig", sig_identity())$data |> set_to_export("dt_sig")
    })
 
    
    # Subset of DESeqDataSet for LRT-significant features, used for PCA
    dds_lrt_sig = reactive({
      req(dds(), dt_lrt_sig())
      cat("Subsetting DESeqDataSet for LRT-significant features...\n")
      dds()[dt_lrt_sig()$feature_id, ]
    })
    
    # PCA
    pca_data_all = reactive({
      req(dds())
      list(data = dds(), 
           identity = c(included()$identity, samples()$identity))
    })
    
    pca_data_lrt = reactive({
      req(dds_lrt_sig())
      list(data = dds_lrt_sig(), 
           identity = c(included()$identity, samples()$identity, lrt_a()))
    })
    
    # Sub modules
    # NB: Don't try to isolate modules with if(); it will break the reactivity
    home_server()
    data_server()
    pca_panel_server(pca_data_all, pca_data_lrt)
    selected = sig_server()
    four_way_server(a, lfc, selected)
    volcano_ma_server(a, lfc, selected)
    go_server()
    gsea_server()
  })
}
