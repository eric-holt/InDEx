# Dot plot
gg_dotplot = function(dt){
  if(is.null(dt) || !nrow(dt)) return()
  dt[, Description := factor(Description, levels = Description)]
  plt = dt |>
    ggplot(aes(GeneRatio, Description, color = qvalue, size = setSize)) +
    geom_point() + 
    scale_y_discrete(labels = label_wrap_gen(40), limits = rev(levels(dt$Description))) +
    labs(x = "gene ratio", y = NULL, size = "term size") +
    scale_color_gradient(low = "blue", high = "red")
  if("NES" %in% names(dt)){ # from gseGO()
    plt = plt + facet_wrap(~ sign)
  }
  plt
}

plotly_dotplot = function(gg){
  if(is.null(gg)) stop("No data to plot")
  ggplotly(gg)
}
