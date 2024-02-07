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
dt_4way = function(dt, ax, ay, a, lfc){
  az = third_contrast(ax, ay)
  d = dt |> 
    dplyr::rename(lfc = log2FoldChange, p = padj) |>
    dcast(feature_id + gene_id + gene_name ~ label, value.var = c("p", "lfc", "lfcSE"))
  d[, x := get(paste0("lfc_", ax))]
  d[, y := get(paste0("lfc_", ay))]
  d[, z := get(paste0("lfc_", az))]
  d[, x_se := get(paste0("lfcSE_", ax))]
  d[, y_se := get(paste0("lfcSE_", ay))]
  d[, z_se := get(paste0("lfcSE_", az))]
  d[, px := get(paste0("p_", ax))]
  d[, py := get(paste0("p_", ay))]
  d[, pz := get(paste0("p_", az))]
  d[, .(feature_id, gene_id, gene_name, x, y, z, x_se, y_se, z_se, px, py, pz)]
}

# Prediction outliers
pred_outliers = function(dt, ax, ay, level){
  model = lm(y ~ x, dt)
  pred = predict(model, interval = "prediction", level = level)
  dt[, `:=`(l = pred[, "lwr"], u = pred[, "upr"])]
  dt = dt[y < l | y > u]
  labels = c(paste0(ax, ">", ay), paste0(ay, ">", ax))
  dt[y - y_se < l, label := labels[1]]
  dt[y + y_se > u, label := labels[2]]
  dt[, label := factor(label, levels = unique(labels))]
  dt
}

contrasts_to_4way_labels = function(...){
  list(...) |> sapply(function(contrast){
    pair = contrast_to_conditions(contrast)
    sprintf("%s/%s significant", pair[1], pair[2])
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
