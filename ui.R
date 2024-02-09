source("preamble.R")


ui = fixedPage(
  useShinyjs(),
  tags$head(tags$style(HTML(css))),
  add_busy_spinner(color = "#808080", position = "top-left"),
  
  if (debugging) debug_ui(),
  fixedRow(
    # column(4, import_ui()),
    column(4,
           uiOutput("project_name"),
           wellPanel(
             fixedRow(
               column(4,
                      actionButton("btn_export", HTML(" Export"), icon("file-export"))),
               column(8,
                      checkboxGroupInput("cbg_export", NULL, c("data", "ggplot", "plotly"), c("data", "ggplot", "plotly"), T))
               )
           )
    ),
    column(8,
           wellPanel(
             h5(HTML("<center>Project parameters</center>")),
             fixedRow(
               column(2,
                      observedNumericInput("num_alpha", "Wald α", .05, 0, 1)),
               column(2,
                      observedNumericInput("num_lrt_alpha", "LRT α", .05, 0, 1)),
               column(2,
                      observedNumericInput("num_lfc", "Log2FC", .5, 0, 2)),
               column(6,
                      observedCheckboxInput("chk_only_sig", "Test LRT-significant genes only", F),
                      observedCheckboxInput("chk_go_pred", "Enrichment with prediction outliers", F))
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
              tabPanel("Functional", gsea_ui())
  )
)
