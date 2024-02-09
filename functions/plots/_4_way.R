# Four-way plot----
# a.: axis, representing a contrast like "HH_HC"

# plot
gg_4way = function(dt, ax, ay, a, lfc, features, level, show_int, show_se){
  az = third_contrast(ax, ay)
  
  # Assign types
  types = contrasts_to_4way_labels(ax, ay, az)
  dt[, type := types[4]]
  dt[(x >= lfc | x <= -lfc) & px < a, type := types[1]]
  dt[(y >= lfc | y <= -lfc) & py < a, type := types[2]]
  dt[(z >= lfc | z <= -lfc) & pz < a, type := types[3]]
  dt[, type := factor(type, levels = unique(types))]
  
  # Mark selected features
  dt[, selected := feature_id %in% features]
  
  # Set contrast colors
  colors = c(.cont_colors[ax],
             .cont_colors[ay],
             .cont_colors[az],
             "gray20") |> setNames(types)
  
  # Prediction interval
  model = lm(y ~ x, dt)
  dt_model = data.table(x = seq(min(dt$x), max(dt$x), length.out = 100))
  fit = predict(model, newdata = dt_model)
  conf = predict(model, newdata = dt_model, interval = "confidence", level = level)
  pred = predict(model, newdata = dt_model, interval = "prediction", level = level)
  dt_model[, `:=`(y = fit, 
                  conf_u = conf[, "upr"],
                  conf_l = conf[, "lwr"],
                  pred_u = pred[, "upr"],
                  pred_l = pred[, "lwr"])]
  
  # Regions of interest (usual matrix id order)
  # r = c("similar", "different", "opposite")
  # rect_colors = c("#aaff00", "#aa80ff", "#5500ff")
  # names(rect_colors) = r
  # dt_region = data.table(xmin = rep(c(-Inf, -lfc, lfc), each = 3),
  #                        xmax = rep(c(-lfc, lfc, Inf), each = 3),
  #                        ymin = rep(c(lfc, -lfc, -Inf), 3),
  #                        ymax = rep(c(Inf, lfc, -lfc), 3),
  #                        region = c(r[3], r[2], r[1], r[2], r[1], r[2], r[1], r[2], r[3]))
  
  
  # Correlation
  ct = cor.test(dt$x, dt$y)
  # Plot
  plt = dt |> 
    ggplot(aes(x, y, color = type,
               text = sprintf("%s\npadj (%s): %.2e\npadj (%s): %.2e\npadj (%s): %.2e",
                              gene_name, 
                              ax, signif(px, 3), 
                              ay, signif(py, 3),
                              az, signif(pz, 3))))
  # Draw regions
  # plt = plt + geom_rect(data = dt_region, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = region), alpha = 1/3, inherit.aes = F) +
  # scale_fill_manual(values = rect_colors) +
  
  plt = plt +
    geom_vline(xintercept = lfc, color = "gray25", linewidth = .1) +
    geom_vline(xintercept = -lfc, color = "gray25", linewidth = .1) +
    geom_hline(yintercept = lfc, color = "gray25", linewidth = .1) +
    geom_hline(yintercept = -lfc, color = "gray25", linewidth = .1) +
    geom_vline(xintercept = 0, linewidth = .1) +
    geom_hline(yintercept = 0, linewidth = .1)
  
  # Show intervals
  if(show_int){
    plt = plt +
      geom_ribbon(aes(ymin = pred_l, ymax = pred_u, color = NULL, text = paste0(level * 100, "% prediction interval")), data = dt_model, fill = "green", alpha = .2) +
      geom_ribbon(aes(ymin = conf_l, ymax = conf_u, color = NULL, text = paste0(level * 100, "% confidence interval")), data = dt_model, fill = "blue", alpha = .2) +
      geom_line(aes(color = NULL, text = paste0("R = ", signif(ct$estimate, 3), " (p = ", signif(ct$p.value, 3), ")")), data = dt_model, linewidth = .2)
  }
  
  # Show standard errors  
  if(show_se){
    plt = plt +
      geom_errorbar(aes(ymin = y - y_se, ymax = y + y_se), linewidth = .1) +
      geom_errorbarh(aes(xmin = x - x_se, xmax = x + x_se), linewidth = .1)
  }
  
  plt = plt +
    geom_point(data = dt[type == types[4] & selected == F], alpha = .25) +
    geom_point(data = dt[type %in% types[1:3] & selected == F], alpha = .75) +
    geom_point(data = dt[selected == T], size = 4, alpha = .9) +
    coord_fixed() +
    scale_color_manual(values = colors) +
    labs(x = paste(to_slash(ax), "log2 fold change"), 
         y = paste(to_slash(ay), "log2 fold change"), 
         alpha = "", color = "", size = "") +
    guides(size = "none", nrow = 2) +
    theme(legend.position = "top",
          legend.box = "horizontal")
  plt
}

plotly_4way = function(gg){
  ggplotly(gg, tooltip = "text") |> 
    layout(legend = list(orientation = "h", x = .5, y = 1, xanchor = "center", yanchor = "bottom"))
}
