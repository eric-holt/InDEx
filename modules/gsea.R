gsea_ui = function(ns = identity, id = "gsea"){
  ns = NS(ns(id))
  tagList(
    if (debugging) debug_ui(ns),
    uiOutput(ns("UI"))
    
  )
}

gsea_server = function(dt_enrich, genes, dt_res, id = "gsea") {
  moduleServer(id, function(input, output, session) {
    ns = session$ns
    if (debugging) debug_server(environment())
    
    output$UI = renderUI({
      if(is.null(dt_enrich()) && is.null(dt_res())){
        return(caution("No data"))
      }
      tagList(
        fixedRow(column(7, wellPanel(
          fixedRow(
            column(4, 
                   actionButton(ns("btn_cp"), HTML("Run clusterProfiler<br>(may take a few minutes)"), icon("rotate-right")),
                   uiOutput(ns("data_identity"))
            ),
            column(4, observedNumericInput(ns("num_minGSSize"), "Min. gene set size", 10, 0)),
            column(4, observedNumericInput(ns("num_maxGSSize"), "Max. gene set size", 500, 0))))),
          column(2, actionButton(ns("btn_reset"), "Reset input")),
          column(2, actionButton(ns("btn_export"), "Export plots"))),
        fixedRow(column(9, wellPanel(
          fixedRow(
            column(3, observedNumericInput(ns("num_n"), "Terms to display", 10, 5, 100)),
            column(3, observedRadioButtons(ns("rbn_sort"), "Sort by", c("p-value", "gene count"), "p-value", inline = T)),
            column(3, observedNumericInput(ns("num_pvalueCutoff"), "p-value cutoff", .05, 0, 1)),
            column(3, observedNumericInput(ns("num_qvalueCutoff"), "q-value cutoff", .2, 0, 1)))))),
        tabsetPanel(
          tabPanel("Enriched GO terms", go_panels_ui(ns, "enrich_go")),
          tabPanel("GSEA (GO)", go_panels_ui(ns, "gsea_go")),
        )
      )
    })
    
    # Inputs
    mings = reactive(input$num_minGSSize)
    observe_input(ns("num_minGSSize"), mings)

    maxgs = reactive(input$num_maxGSSize)
    observe_input(ns("num_maxGSSize"), maxgs)
    
    sort = reactive(input$rbn_sort)
    observe_input(ns("rbn_sort"), sort)
    
    p = reactive(input$num_pvalueCutoff)
    observe_input(ns("num_pvalueCutoff"), p)
    
    q = reactive(input$num_qvalueCutoff)
    observe_input(ns("num_qvalueCutoff"), q)
    
    n = reactive(input$num_n)
    observe_input(ns("num_n"), n)
    
    output$data_identity = renderUI({
      req(dt_res(), cashed_data_identity("gsea"))
      if(is_data_updated(dt_res()[, .(feature_id, log2FoldChange, label)], "gsea"))
        caution("Data changed")
        # span("Data changed", icon("circle-exclamation"), style = "color: red;")
      else
        span("Data not changed", icon("check"), style = "color: green;")
    })
    
    
    observe({
      req(dt_res())
      write_cache(dt_res()[, .(feature_id, log2FoldChange, label)], "gsea_last_data")
      if(!is.null(dt_enrich()) && nrow(dt_enrich()) > 0){
        cat("Computing GO enrichment...\n")
        .re$enrich_go = get_enrich_go(dt_enrich(), genes(), 1, 1, mings(), maxgs())
        write_cache(.re$enrich_go, "enrich_go")
        cat("Done Computing GO enrichment\n")
      } else {
        .re$enrich_go = NULL
        write_cache(.re$enrich_go, "enrich_go")
      }
      cat("Computing GO GSEA...\n")
      .re$gsea_go = get_all_gsea(dt_res(), 1, mings(), maxgs())
      write_cache(.re$gsea_go, "gsea_go")
      cat("Done Computing GO GSEA\n")
    }) |> bindEvent(input$btn_cp)
    
    enrich_go = reactive({
      .re$enrich_go
      cashed_data("enrich_go")
    })

    gsea_go = reactive({
      .re$gsea_go
      cashed_data("gsea_go")
    })
    
    plt_lists = list(
      enrich_go = go_panels_server(enrich_go, p, q, n, sort, "enrich_go"),
      gsea_go = go_panels_server(gsea_go, p, q, n, sort, "gsea_go")
    )

    observe({
      plt_list_names = names(plt_lists)
      dirs = here("projects", .project, "plots", plt_list_names) |> setNames(plt_list_names)
      plt_list_names |> lapply(function(list_name){
        plt_list = plt_lists[[list_name]]()
        names(plt_list) |> lapply(function(label){
          plots = plt_list[[label]]
          names(plots) |> lapply(function(ont){
            plt = plots[[ont]]()
            if(is.ggplot(plt)){
              dir = dirs[list_name]
              path = here(dir, paste0(ont, "_", label, ".png"))
              ggsave(path, plt)
              cat("Saved", path, "\n")
            }
          })
        })
      })
    }) |> bindEvent(input$btn_export)

    observe({
      updateNumericInput(inputId = "num_n", value = 10)
      updateNumericInput(inputId = "num_pvalueCutoff", value = .05)
      updateNumericInput(inputId = "num_qvalueCutoff", value = .2)
      updateNumericInput(inputId = "num_minGSSize", value = 10)
      updateNumericInput(inputId = "num_maxGSSize", value = 500)
      updateRadioButtons(inputId = "rbn_sort", selected = "p-value")
      cat("GO inputs reset\n")
    }) |> bindEvent(input$btn_reset)
  })
}

