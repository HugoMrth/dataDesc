describeTidy <- function(desc.object) {
  i <- 2
  n <- 0
  # Suppressing Oui/Non lines to shorten the table
  while (i < nrow(desc.object)) {
    # Oui/non double lines detection
    # making sure ther's nothing after
    if ((desc.object[i, 1] == "  Non" & desc.object[i+1, 1] == "  Oui" & substr(desc.object[i+2, 1], 1, 2) != "  ") |
      (desc.object[i, 1] == "  Oui" & desc.object[i+1, 1] == "  Non" & substr(desc.object[i+2, 1], 1, 2) != "  ") | 
      (desc.object[i, 1] == "  Yes" & desc.object[i+1, 1] == "  No" & substr(desc.object[i+2, 1], 1, 2) != "  ") |
      (desc.object[i, 1] == "  No" & desc.object[i+1, 1] == "  Yes" & substr(desc.object[i+2, 1], 1, 2) != "  ") ) {
      # replacing empty line with the data
      ifelse(desc.object[i, 1] == "  Oui" | desc.object[i, 1] == "  Yes",
             desc.object[i-1, 2:ncol(desc.object)] <- desc.object[i, 2:ncol(desc.object)],
             desc.object[i-1, 2:ncol(desc.object)] <- desc.object[i+1, 2:ncol(desc.object)])
      # suppressing the oui/non lines
      desc.object <- desc.object[-c(i, i+1),]
      n <- n + 1
    }
    i <- i + 1
  }
  message(paste(n, "Yes/No variables have been detected and", 2*n, "lines have been suppressed"))
  return(desc.object)
}
