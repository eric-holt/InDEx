# Four-way plot----
# a.: axis, representing a contrast like "HH_HC"

# plot
gg_4way = function(dt4, ax, ay, a = .05, lfc = .5, features = NULL, level = .95, show_int = F, show_se = F, metric = "Signed log(p)", colors = .cont_colors, point_size = 1.5, na_alpha = .2){
  dt = copy(dt4)
  ax = as.character(ax)
  ay = as.character(ay)
  az = attr(dt, "third_contrast")
  
  dt[, selected := feature_id %in% features]

  # Linear model with confidence and prediction intervals
  d = dt[, .SD, .SDcols = c(ax, ay)] |> na.omit()
  model = lm(get(ay) ~ get(ax), d)
  dt_model = data.table(x = seq(min(d[[ax]]), max(d[[ax]]), length.out = 100)) |> setNames(ax)
  fit = predict(model, newdata = dt_model)
  conf = predict(model, newdata = dt_model, interval = "confidence", level = level)
  pred = predict(model, newdata = dt_model, interval = "prediction", level = level)
  dt_model[, `:=`(y = fit, 
                  conf_u = conf[, "upr"],
                  conf_l = conf[, "lwr"],
                  pred_u = pred[, "upr"],
                  pred_l = pred[, "lwr"])]
  setnames(dt_model, "y", ay)
  
  # Correlation
  ct = cor.test(dt[[ax]], dt[[ay]])
  r = ct$estimate
  
  zsig = sprintf("%s_significant", az)
  
  # Plot
  p = dt |>
    ggplot(aes(!!sym(ax), !!sym(ay), color = !!sym(zsig), alpha = !!sym(zsig))) +
    scale_color_manual(values = c("TRUE" = unname(colors[az]), "FALSE" = "gray20", "NA" = "gray20"), breaks = c(T, F, NA), labels = c("significant", "non-significant", "not applicable")) +
    scale_alpha_manual(values = c("TRUE" = .9, "FALSE" = na_alpha, "NA" = na_alpha), breaks = c(T, F, NA), labels = c("significant", "non-significant", "not applicable")) +
    scale_size_manual(values = c("FALSE" = point_size, "TRUE" = point_size * 2)) +
    guides(color = guide_legend(title = to_slash(az)), alpha = "none", size = "none") +
    coord_fixed() +
    theme(legend.position = "top") +
    geom_abline(intercept = 0, slope = 1, linetype = "dotdash", color = "grey25")

  
  if (metric == "LFC"){
    p = p +
      geom_vline(xintercept = lfc, color = "gray25", linewidth = .1) +
      geom_vline(xintercept = -lfc, color = "gray25", linewidth = .1) +
      geom_hline(yintercept = lfc, color = "gray25", linewidth = .1) +
      geom_hline(yintercept = -lfc, color = "gray25", linewidth = .1)
  }
  
  p = p +
    geom_vline(xintercept = 0, linewidth = .1) +
    geom_hline(yintercept = 0, linewidth = .1)
  
  # Show intervals
  if(show_int){
    p = p +
      geom_ribbon(aes(ymin = pred_l, ymax = pred_u, color = NULL, text = paste0(level * 100, "% prediction interval")), data = dt_model, fill = "green", alpha = .2) +
      geom_ribbon(aes(ymin = conf_l, ymax = conf_u, color = NULL, text = paste0(level * 100, "% confidence interval")), data = dt_model, fill = "blue", alpha = .2) +
      geom_line(aes(color = NULL, text = paste0("R = ", signif(ct$estimate, 3), " (p = ", signif(ct$p.value, 3), ")")), data = dt_model, linewidth = .2, color = "red")
  }
  
  # Show standard errors  
  if(show_se){
    p = p +
      geom_errorbar(aes(ymin = y - y_se, ymax = y + y_se), linewidth = .1) +
      geom_errorbarh(aes(xmin = x - x_se, xmax = x + x_se), linewidth = .1)
  }
  
  # Data points
  p = p +
    geom_point(aes(size = selected, 
                   text = sprintf("%s\npadj (%s): %.2e\npadj (%s): %.2e\npadj (%s): %.2e",
                                  gene_name, 
                                  ax, signif(get(sprintf("%s_padj", ax)), 3),
                                  ay, signif(get(sprintf("%s_padj", ay)), 3),
                                  az, signif(get(sprintf("%s_padj", az)), 3))))

  # Labels
  label_suffix = if (metric == "LFC") "log2 fold change" else "−log10(p) × sign(LFC)"
  p = p +
    labs(x = paste(to_slash(ax), label_suffix),
         y = paste(to_slash(ay), label_suffix))
  p
}

plotly_4way = function(gg){
  ggplotly(gg, tooltip = "text") |> 
    layout(legend = list(orientation = "h", x = .5, y = 1, xanchor = "center", yanchor = "bottom"))
}
