# Four-way plot----
# a.: axis, representing a contrast like "HH_HC"

# plot
gg_4way = function(dt, ax, ay, a, lfc, features, level, show_int, show_se, metric){
  az = third_contrast(ax, ay)
  
  # Assign types
  types = contrasts_to_4way_labels(ax, ay, az)
  dt[, type := types[4]]
  dt[x_padj < a, type := types[1]]
  dt[y_padj < a, type := types[2]]
  dt[z_padj < a, type := types[3]]
  dt[, type := factor(type, levels = types)]
  
  # Mark selected features
  dt[, selected := feature_id %in% features]
  
  # Set contrast colors defined upon project load
  colors = c(.cont_colors[ax],
             .cont_colors[ay],
             .cont_colors[az],
             "gray20") |> setNames(types)
  
  # Linear model with confidence and prediction intervals
  d = dt[, .(x, y)] |> na.omit()
  model = lm(y ~ x, d)
  dt_model = data.table(x = seq(min(d$x), max(d$x), length.out = 100))
  fit = predict(model, newdata = dt_model)
  conf = predict(model, newdata = dt_model, interval = "confidence", level = level)
  pred = predict(model, newdata = dt_model, interval = "prediction", level = level)
  dt_model[, `:=`(y = fit, 
                  conf_u = conf[, "upr"],
                  conf_l = conf[, "lwr"],
                  pred_u = pred[, "upr"],
                  pred_l = pred[, "lwr"])]
  
  # Correlation
  ct = cor.test(dt$x, dt$y)
  
  # Plot
  p = dt |> 
    ggplot(aes(x, y, color = type,
               text = sprintf("%s\npadj (%s): %.2e\npadj (%s): %.2e\npadj (%s): %.2e",
                              gene_name, 
                              ax, signif(x_padj, 3), 
                              ay, signif(y_padj, 3),
                              az, signif(z_padj, 3))))

  if (metric == "lfc"){
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
      geom_line(aes(color = NULL, text = paste0("R = ", signif(ct$estimate, 3), " (p = ", signif(ct$p.value, 3), ")")), data = dt_model, linewidth = .2)
  }
  
  # Show standard errors  
  if(show_se){
    p = p +
      geom_errorbar(aes(ymin = y - y_se, ymax = y + y_se), linewidth = .1) +
      geom_errorbarh(aes(xmin = x - x_se, xmax = x + x_se), linewidth = .1)
  }
  
  p = p +
    geom_point(data = dt[type == types[4] & selected == F], alpha = .25) +
    geom_point(data = dt[type %in% types[1:3] & selected == F], alpha = .75) +
    geom_point(data = dt[selected == T], size = 4, alpha = .9) +
    coord_fixed() +
    scale_color_manual(values = colors) +
    guides(size = "none", nrow = 2)+
    labs(alpha = "", color = "", size = "") +
    theme(legend.position = "top",
          legend.box = "horizontal")
  
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
