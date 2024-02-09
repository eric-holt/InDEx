# P-value histogram
gg_p_hist = function(dt, bins = 50){
  dt |> ggplot(aes(pvalue)) + 
    geom_histogram(bins = bins) +
    facet_wrap(~ label, nrow = 1)
}

plotly_p_hist = function(gg, bins = 50){
  ggplotly(gg)
}