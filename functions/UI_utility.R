caution = function(html_text = ""){
  span(HTML(html_text), icon("circle-exclamation"), style = "color: red;")
}

ok = function(html_text = ""){
  span(HTML(html_text), icon("check"), style = "color: green;")
}
