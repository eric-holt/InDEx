# Dot plot
dotplot = function(dt){
  plt = gg_dotplot(dt)
  if(is.null(plt)) stop("No data to plot")
  ggplotly(plt)
}

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