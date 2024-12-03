plotSankey <- function(Nodes, Links) {

  if (add.prop) {
    if(all(table(Links$source) == table(Links$source)[1])) {
      nrow_per_times <- table(Links$source)[1]^2
      n_times <- nrow(Links)/nrow_per_times
      for (i in 1:n_times) {
        tab <- Links[((i-1)*nrow_per_times+1):((i-1)*nrow_per_times+nrow_per_times), ]
        vals <- as.numeric(by(tab$value, tab$target, sum))
        Nodes[Nodes$X %in% (unique(tab$target) + 1), "label"] <- paste0(
          Nodes[Nodes$X %in% (unique(tab$target) + 1), "label"], " (",
          formatC(vals/sum(vals)*100, digits = 1, format = 'f'), "%)")
        
        if (i == 1) {
          vals <- as.numeric(by(tab$value, tab$source, sum))
          Nodes[Nodes$X %in% (unique(tab$source) + 1), "label"] <- paste0(
            Nodes[Nodes$X %in% (unique(tab$source) + 1), "label"], " (",
            formatC(vals/sum(vals)*100, digits = 1, format = 'f'), "%)")
        }
      }
    } else {
      warning("Links arg does not have equal number of each source and function cannot automatically calculate proportions.")
    }
  }
  
  # Adding transparency to the links
  # Conversion to rgba character string
  Links$color <- apply(grDevices::col2rgb(Links$color), 2, function(x) {
    paste0("rgba(", x[1], ",", x[2], ",", x[3], ",0.4)")
  })
    
  fig <- plot_ly(
      type = "sankey",
      orientation = "h",
      alpha_stroke = 0.2,
      node = list(
        label = Nodes$label,
        color = Nodes$color,
        pad = 15,
        thickness = 20,
        line = list(color = "black", width = 0.5)
      ),
      link = list(
        source = Links$source,
        target = Links$target,
        value =  Links$value,
        color = Links$color
      )
    )
  fig <- fig %>% layout(font = list(size = 14, color = "black", weight = "bold"))
  fig
}
