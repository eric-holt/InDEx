
# Get numerical values of PCA used by plotPCA()
get_pca = function(dds){
  tryCatch({
    pca = prcomp(t(assay(rlog(dds))))
    eigen = pca$sdev ^ 2
    explained = (eigen / sum(eigen)) |> setNames(colnames(pca$x))
    
    dt = as.data.table(pca$x)
    dt[, sample := rownames(pca$x)]
    dt[, id := str_sub(sample, 2, 2)]
    dt[, condition := dds$condition]
    
    list(prcomp = pca, dt = dt, explained_var = explained)
  }, error = function(e) {
    warning("get_pca: ", e)
    NULL
  })
}

# PCA plot (PC 1 and 2)
ggplot_pca = function(pca, colors = .cond_colors){
  if(is.null(pca)) stop("No data")
  vars = pca$explained_var * 100
  plt = pca$dt |> 
    ggplot(aes(PC1, PC2, color = condition, text = sample)) +
    geom_point() +
    coord_fixed() +
    labs(x = sprintf("PC1 (%.1f%%)", vars[1]),
         y = sprintf("PC2 (%.1f%%)", vars[2])) +
    theme(legend.position = "none")
  if(!is_null(colors)) 
    plt = plt + scale_color_manual(values = colors)
  plt
}

plotly_pca = function(pca, colors = .cond_colors){
  ggplotly(ggplot_pca(pca, colors), tooltip = "text")
}

# Bar plot for all PCs from PCA
ggplot_all_pc = function(pca, colors = .cond_colors){
  if(is.null(pca)) stop("No data")
  dt = pca$dt
  vars = pca$explained_var * 100
  vars = vars[vars > .1]
  n = length(vars)
  dt = dt |> dplyr::select(c(names(vars), sample, id, condition)) |> 
    gather("PC", "score", PC1:!!sym(paste0("PC", n))) |> 
    mutate(PC = as.factor(parse_number(PC)))
  setDT(dt)
  dt[, v := (score - min(score)) / (max(score) - min(score)) * vars[PC], by = PC]
  plt = dt |>
    ggplot(aes(PC, v)) +
    geom_col(aes(text = sprintf("PC%d explains %.1f%% variance", PC, v)),
             data = data.table(PC = factor(1:n), v = vars), alpha = .25) + 
    geom_point(aes(color = condition, 
                   text = sprintf("%s: PC%d score %.2f", sample, PC, score))) +
    labs(y = "explained variance [%]\n(scaled PC score)")
  if(!is_null(colors)) 
    plt = plt + scale_color_manual(values = colors)
  plt
}

plotly_all_pc = function(pca, colors = .cond_colors){
  ggplotly(ggplot_all_pc(pca, colors), tooltip = "text")
}


# PCA hierarchical clustering heatmap
pca_hc_heatmap = function(pca){
  if(is.null(pca)) stop("No data")
  p = t(pca$prcomp$x)
  heatmaply(p[1:(nrow(p) - 1), ], dendrogram = "column", column_text_angle = 90)
}