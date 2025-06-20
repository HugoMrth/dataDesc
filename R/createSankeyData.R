createSankeyData <- function(data,
                                  states,
                                  timesColumns,
                                  autoNodesCoord = FALSE) {
  data <- as.data.frame(data)
  n_states <- length(states)
  n_times <- length(timesColumns)
  for (i in 1:n_times) {
    data[, timesColumns[i]] <- factor(data[, timesColumns[i]], levels = states)
  }
  
  nodesCols <- c("#AAC0AF", "#B28B84", "#1C4073", "#0f766e", "#653239", "#472C1B", "#5C2751")[1:n_states]
  linksCols <- c("#D0DCD3", "#D0B8B4", "#285CA4", "#17B5A7", "#964A54", "#76492D", "#8F3D7E")[1:n_states]
  
  
  vals <- c()
  for (i in 2:n_times) {
    for (j in 1:n_states) {
      vals <- c(vals, table(data[, timesColumns[i]][data[, timesColumns[i-1]] == states[j]]))
    }
  }
  
  dataSankey <- list(
    Nodes = data.frame(
      label = rep(states, n_times),
      color = rep(nodesCols, n_times)
    ),
    Links = data.frame(
      source = c(rep(1:(n_states*(n_times-1)), each = n_states)) - 1,
      target = as.vector(sapply(split((n_states+1):(n_states*n_times), rep(1:(n_times-1), each = n_states)), function(x) {rep(x, n_states)})) - 1,
      value = vals,
      color = rep(rep(linksCols, each = n_states), n_times-1)
    ))
  
  # Calculating Nodes Coordinates
  if (!autoNodesCoord) {
    if (length(timesColumns) == 2) {
      dataSankey$Nodes$x <- rep(c(0.0001, 0.98), each = n_states)
    } else {
      dataSankey$Nodes$x <- rep(c(0.0001, (1:(n_times-2))/(n_times-1), 0.98), each = n_states)
    }
    
    t1 <- dataSankey$Links %>%
      group_by(source) %>%
      summarise(n = sum(value))
    t2 <- dataSankey$Links %>%
      group_by(target) %>%
      summarise(n = sum(value))
    colnames(t2) <- colnames(t1)
    t3 <- rbind(t1, t2[!(t2$source %in% t1$source),]) %>%
      mutate(group = rep(1:(n_times), each = n_states))
    y <- c()
    for (i in 1:n_times) {
      for (j in 1:n_states) {
        if (j == 1) {
          y <- c(y, (t3$n[t3$group == i][1]/2)/sum(t3$n[t3$group == i]))
        } else {
          y <- c(y, sum(t3$n[t3$group == i][1:(j-1)])/sum(t3$n[t3$group == i]) +
                   (t3$n[t3$group == i][j]/2)/sum(t3$n[t3$group == i]))
        }
      }
    }
    dataSankey$Nodes$y <- y
  }
  return(dataSankey)
}
