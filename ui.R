source("preamble.R")

ui = fixedPage(
  useShinyjs(),
  tags$head(tags$style(HTML(css))),
  add_busy_spinner(color = "#808080", position = "top-left"),
  
  if (debugging) debug_ui(),
  fixedRow(
    column(5,
           uiOutput("project_name"),
           wellPanel(
             fixedRow(
               column(4,
                      downloadButton("export", HTML(" Export"), icon = icon("file-export")),
                      actionButton("btn_clr_cache", "Clear cache", icon("refresh"))),
               column(8,
                      checkboxGroupInput("cbg_export", NULL, c("data", "ggplot", "plotly"), c("data"), T))
             )
           )
    ),
    column(7,
           wellPanel(
             h5(HTML("<center>Significance cutoffs</center>")),
             fixedRow(
               column(4,
                      observedNumericInput("num_alpha", HTML("Wald p<sub>adj</sub>"), .05, 0, 1)),
               column(4,
                      observedNumericInput("num_lrt_alpha", HTML("LRT p<sub>adj</sub>"), .05, 0, 1),
                      observedCheckboxInput("chk_same_as_wald", "Same as Wald", T)
               ),
               column(4,
                      observedNumericInput("num_lfc", "Log2FC", .5, 0, 2)),
             )
           )
    )
  ),
  tabsetPanel(id = "tbs_main",
              tabPanel("Home", home_ui()),
              tabPanel("Filter", data_ui()),
              tabPanel("PCA", pca_panel_ui()),
              tabPanel("Significant", sig_ui()),
              tabPanel("4-way", four_way_ui()),
              tabPanel("Volcano/MA/p", volcano_ma_ui()),
              tabPanel("GO", go_ui()),
              tabPanel("GSEA", gsea_ui())
  )
)
