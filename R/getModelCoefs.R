getModelCoefs <- function(
    model, # Une sortie de modèle avec un attribut 'coefficients'
    coefs_digits = 2,
    p_digits = 3,
    p_threshold = 0.001,
    merge_p = TRUE
    ) {
  
  coefficients <- summary(model)$coefficients[, "Estimate"]
  se <- summary(model)$coefficients[, "Std.err"]
  
  if (merge_p) {
    res <- data.frame(
      Variable = rownames(summary(model)$coefficients)[-1],
      Coefs = paste0(formatC(exp(coefficients)[-1], digits = coefs_digits, format = "f"), " [",     # OR
                     formatC(exp(coefficients - 1.96 * se)[-1], digits = coefs_digits, format = "f"), "-",      # IC inf
                     formatC(exp(coefficients + 1.96 * se)[-1], digits = coefs_digits, format = "f"), "], p ", # IC sup
                     ifelse(summary(model)$coefficients[-1, 4] < p_threshold,
                            paste("<", p_threshold),
                            paste("=", formatC(summary(model)$coefficients[-1, 4], digits = p_digits, format = "f"))))      # p val
    )
  } else {
    res <- data.frame(
      Variable = rownames(summary(model)$coefficients)[-1],
      Coefs = paste0(formatC(exp(coefficients)[-1], digits = coefs_digits, format = "f"), " [",     # OR
                     formatC(exp(coefficients - 1.96 * se)[-1], digits = coefs_digits, format = "f"), "-",      # IC inf
                     formatC(exp(coefficients + 1.96 * se)[-1], digits = coefs_digits, format = "f"), "]"),# IC sup
      p = ifelse(summary(model)$coefficients[-1, 4] < p_threshold,
                                 paste0("<", p_threshold),
                                 formatC(summary(model)$coefficients[-1, 4], digits = p_digits, format = "f"))    # p val
    )
  }
  res
}
