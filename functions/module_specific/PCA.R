# Get numerical values of PCA used by plotPCA()
get_pca = function(dds){
  tryCatch({
    pca = prcomp(t(assay(rlog(dds))))
    eigen = pca$sdev ^ 2
    explained = (eigen / sum(eigen)) |> setNames(colnames(pca$x))
    
    dt = as.data.table(pca$x)
    dt[, sample := rownames(pca$x)]
    dt[, condition := dds$condition]
    
    list(prcomp = pca, dt = dt, explained_var = explained)
  }, error = function(e) {
    warning("get_pca: ", e)
    NULL
  })
}

# PCA plot (PC 1 and 2)
gg_pca = function(pca, colors = .cond_colors){
  if(is.null(pca)) stop("No data")
  p = plot_2pc(pca$dt, pca$explained_var)
  if(!is_null(colors)) 
    p = p + scale_color_manual(values = colors)
  p
}

plot_2pc = function(dt, vars){
  dt |> 
    ggplot(aes(PC1, PC2, color = condition, text = sample)) +
    geom_point() +
    labs(x = sprintf("PC1 (%.1f%%)", vars[1] * 100),
         y = sprintf("PC2 (%.1f%%)", vars[2] * 100)) +
    theme(aspect.ratio = vars[2] / vars[1]) +
    guides(color = "none")
}

plotly_pca = function(gg){
  ggplotly(gg, tooltip = "text")
}

# Bar plot for all PCs from PCA
gg_all_pc = function(pca, colors = .cond_colors){
  if(is.null(pca)) stop("No data")
  dt = pca$dt
  vars = pca$explained_var
  p = plot_all_pc(dt, vars)
  if(!is_null(colors)) 
    p = p + scale_color_manual(values = colors)
  p
}

plot_all_pc = function(dt, vars){
  vars = vars[vars > .01] * 100
  n = length(vars)
  dt = dt[, .SD, .SDcols = c(names(vars), "sample", "condition")][, (1:n) := lapply(.SD, function(x) scale(x)), .SDcols = 1:n] |> melt(id.vars = c("sample", "condition"), variable.name = "PC", value.name = "score")
  dt[, PC := str_remove(PC, "PC")]
  dt[, PC := as.factor(parse_number(PC))]
  dt[, v := (score - min(score)) / (max(score) - min(score)) * vars[PC], by = PC]
  dt |>
    ggplot(aes(PC, v)) +
    geom_col(aes(text = sprintf("PC%d explains %.1f%% variance", PC, v)),
             data = data.table(PC = factor(1:n), v = vars), alpha = .25) |> suppressWarnings() + 
    geom_point(aes(color = condition, 
                   text = sprintf("%s: PC%d score %.2f", sample, PC, score))) |> suppressWarnings() +
    labs(y = "explained variance [%]\n(scaled PC score)")
}

plotly_all_pc = function(gg){
  ggplotly(gg, tooltip = "text")
}

# PCA hierarchical clustering heatmap
pca_hc_heatmap = function(pca){
  if(is.null(pca)) stop("No data")
  p = t(pca$prcomp$x)
  heatmaply(p[1:(nrow(p) - 1), ], dendrogram = "column", column_text_angle = 90)
}
