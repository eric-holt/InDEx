# MA plot
gg_MA = function(dt, a, lfc, features){
  d = copy(dt)
  types = c("upregulated", "downregulated", "not significant")
  d[, type := types[3]]
  d[log2FoldChange >= lfc & padj < a]$type = types[1]
  d[log2FoldChange <= -lfc & padj < a]$type = types[2]
  d[, type := factor(type, levels = unique(types))]
  d[, selected := feature_id %in% features]
  colors = c("red", "blue", "gray40") |> setNames(types)
  
  plt = d |> ggplot(aes(log10(baseMean), log2FoldChange, color = type, text = gene_name)) +
    facet_wrap(~label, nrow = 1) +
    geom_hline(yintercept = lfc, color = "gray25", linewidth = .1) +
    geom_hline(yintercept = -lfc, color = "gray25", linewidth = .1) +
    geom_hline(yintercept = 0, linewidth = .1) +
    geom_point(data = d[type == types[3] & selected == F], alpha = .01) +
    geom_point(data = d[type %in% types[1:2] & selected == F], alpha = .5) +
    geom_point(data = d[selected == T], size = 4, alpha = .9) +
    labs(x = "log10 mean normalized counts", 
         y = "log2 fold change") +
    scale_color_manual(values = colors) +
    theme(legend.position = "none")
  plt
}

plotly_MA = function(gg){
  ggplotly(gg, tooltip = "text")
}