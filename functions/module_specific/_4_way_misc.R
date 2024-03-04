# Given two sets of condition pairs, non-overlaps are the other pair
third_pair = function(pair1, pair2){
  union(setdiff(pair1, pair2), setdiff(pair2, pair1))
}

third_contrast = function(ax, ay){
  pair_x = contrast_to_conditions(ax)
  pair_y = contrast_to_conditions(ay)
  pair_z = third_pair(pair_x, pair_y)
  pair_to_contrast(pair_z)
}

# Data for the plot
dt_4way = function(dt, ax, ay, a, lfc, metric){
  # Third contrast that is not compared
  az = third_contrast(ax, ay)
  
  dt = copy(dt)
  setnames(dt, c("log2FoldChange", "pvalue"), c("lfc", "p"))
  
  # Change column names based on the metric
  dt[, value := if (metric == "LFC") lfc else -log10(p) * sign(lfc)]
  
  vars = if (metric == "LFC") c("value", "lfcSE", "padj") else c("value", "padj")

  d = dt |>
    dcast(feature_id + gene_id + gene_name ~ label, value.var = vars)

  c("x", "y", "z") |> lapply(function(v){
    av = get(paste0("a", v))
    old = paste0(c("value_", "lfcSE_", "padj_"), av)
    new = paste0(v, c("", "_se", "_padj"))
    setnames(d, old, new, skip_absent = T)
  })
  d
}

# Prediction outliers
pred_outliers = function(dt, ax, ay, level){
  d = dt[, .(x, y, gene_id, gene_name, feature_id)]
  d = d[complete.cases(d)]
  model = lm(y ~ x, dt)
  pred = predict(model, interval = "prediction", level = level)
  d[, `:=`(l = pred[, "lwr"], u = pred[, "upr"])]
  d = d[y < l | y > u]
  labels = c(paste0(ax, ">", ay), paste0(ay, ">", ax))
  d[y < l, label := labels[1]]
  d[y > u, label := labels[2]]
  d[, label := factor(label, levels = unique(labels))]
  d
}

contrasts_to_4way_labels = function(...){
  types = list(...) |> sapply(function(contrast){
    sprintf("%s significant", to_slash(contrast))
  }) |> c("not significant")
}

# Pseudo namespace for the 4-way plot function
four_way = list(
  assign_types = function(dt, types){
    dt[, type := types[4]]
    dt[(x >= lfc | x <= -lfc) & px < a, type := types[1]]
    dt[(y >= lfc | y <= -lfc) & py < a, type := types[2]]
    dt[(z >= lfc | z <= -lfc) & pz < a, type := types[3]]
    dt[, type := factor(type, levels = unique(types))]
  }
)
