# P-value histogram
gg_pval_hist = function(dt, bins = 50){
  dt |> ggplot(aes(pvalue)) + 
    geom_histogram(bins = bins) +
    facet_wrap(~ label, nrow = 1)
}

plotly_pval_hist = function(dt, bins = 50){
  ggplotly(gg_pval_hist(dt, bins))
}