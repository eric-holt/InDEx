# Given two sets of condition pairs, non-overlaps are the other pair
third_pair = function(pair1, pair2){
  union(setdiff(pair1, pair2), setdiff(pair2, pair1))
}

third_contrast = function(dt, ax, ay){
  contrasts = dt$label |> unique()
  pair_x = contrast_to_conditions(ax)
  pair_y = contrast_to_conditions(ay)
  pair_z = third_pair(pair_x, pair_y)
  contrasts[contrasts %like% pair_z[1] & contrasts %like% pair_z[2]] |> as.character()
}

# Data for the plot
dt_4way = function(dt_res, ax, ay, a, lfc, metric){
  ax = as.character(ax)
  ay = as.character(ay)
  az = third_contrast(dt_res, ax, ay)
  
  dt = copy(dt_res)
  setnames(dt, c("log2FoldChange", "pvalue"), c("lfc", "p"))
  
  # Change column names based on the metric
  dt[, value := if (metric == "LFC") lfc else -log10(p) * sign(lfc)]
  
  vars = if (metric == "LFC") c("value", "lfcSE", "padj") else c("value", "padj")

  d = dt |>
    dcast(feature_id + gene_id + gene_name ~ label, value.var = vars)

  c(ax, ay, az) |> lapply(function(av){
    old = paste0(c("value_", "lfcSE_", "padj_"), av)
    new = paste0(av, c("", "_se", "_padj"))
    setnames(d, old, new, skip_absent = T)
  })
  
  d[, (sprintf("%s_significant", az)) := get(sprintf("%s_padj", az)) < a]
  
  attr(d, "third_contrast") = az
  d
}

# Prediction outliers
pred_outliers = function(dt4, ax, ay, level){
  if(ax == ay) return(NULL)
  dt = copy(dt4)
  d = dt[, .SD, .SDcols = c(ax, ay, "gene_id", "gene_name", "feature_id")]
  d = d[complete.cases(d)]
  setnames(d, c(ax, ay), c("x", "y"))
  model = lm(y ~ x, d)
  pred = predict(model, interval = "prediction", level = level)
  d[, `:=`(l = pred[, "lwr"], u = pred[, "upr"])]
  d = d[y < l | y > u]
  labels = c(paste0(ax, ">", ay), paste0(ay, ">", ax))
  d[y < l, label := labels[1]]
  d[y > u, label := labels[2]]
  d[, label := factor(label, levels = unique(labels))]
  d
}
