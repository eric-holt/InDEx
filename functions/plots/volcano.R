# Volcano plot
gg_volcano = function(dt, a, lfc, features){
  d = copy(dt)
  types = c("upregulated", "downregulated", "not significant")
  d[, type := types[3]]
  d[log2FoldChange >= lfc & padj < a]$type = types[1]
  d[log2FoldChange <= -lfc & padj < a]$type = types[2]
  d[, type := factor(type, levels = unique(types))]
  d[, selected := feature_id %in% features]
  colors = c("red", "blue", "gray40") |> setNames(types)
  
  plt = d |> ggplot(aes(log2FoldChange, -log10(padj), color = type, text = gene_name)) +
    facet_wrap(~ label, nrow = 1) +
    geom_vline(xintercept = lfc, color = "gray25", linewidth = .1) +
    geom_vline(xintercept = -lfc, color = "gray25", linewidth = .1) +
    geom_hline(yintercept = -log10(a), color = "gray25", linewidth = .1) +
    geom_vline(xintercept = 0, linewidth = .1) +
    geom_hline(yintercept = 0, linewidth = .1) +
    geom_point(data = d[type == types[3] & selected == F], alpha = .1) +
    geom_point(data = d[type %in% types[1:2] & selected == F], alpha = .5) +
    geom_point(data = d[selected == T], size = 4, alpha = .9) +
    labs(x = "log2 fold change", color = "") +
    scale_color_manual(values = colors) +
    theme(legend.position = "none")
  plt
}

plotly_volcano = function(gg){
  ggplotly(gg, tooltip = "text")
}