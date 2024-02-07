# MA plot
gg_MA = function(dt, a, lfc, features){
  types = c("upregulated", "downregulated", "not significant")
  dt[, type := types[3]]
  dt[log2FoldChange >= lfc & padj < a]$type = types[1]
  dt[log2FoldChange <= -lfc & padj < a]$type = types[2]
  dt[, type := factor(type, levels = unique(types))]
  dt[, selected := feature_id %in% features]
  colors = c("red", "blue", "gray40") |> setNames(types)
  
  plt = dt |> ggplot(aes(log10(baseMean), log2FoldChange, color = type, text = gene_name)) +
    facet_wrap(~label, nrow = 1) +
    geom_hline(yintercept = lfc, color = "gray25", linewidth = .1) +
    geom_hline(yintercept = -lfc, color = "gray25", linewidth = .1) +
    geom_hline(yintercept = 0, linewidth = .1) +
    geom_point(data = dt[type == types[3] & selected == F], alpha = .1) +
    geom_point(data = dt[type %in% types[1:2] & selected == F], alpha = .5) +
    geom_point(data = dt[selected == T], size = 4, alpha = .9) +
    labs(x = "log10 mean normalized counts", 
         y = "log2 fold change") +
    scale_color_manual(values = colors) +
    theme(legend.position = "none")
  plt
}

plotly_MA = function(dt, a, lfc, features){
  ggplotly(gg_MA(dt, a, lfc, features), tooltip = "text")
}