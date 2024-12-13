getModelCoefs <- function(
  model, # Une sortie de modèle avec un attribut 'coefficients'
  coefs_digits = 2,
  p_digits = 3,
  p_threshold = 0.001,
  merge_CI = TRUE,
  merge_p = TRUE
) {
  
  if(!merge_CI & merge_p) warning("Confidence intervals will only be unmerged if 'merge_p = FALSE'")
  
  coefficients <- summary(model)$coefficients[, 1]
  se <- summary(model)$coefficients[, 2]
  
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
    if (merge_CI) {
      res <- data.frame(
        Variable = rownames(summary(model)$coefficients)[-1],
        Coefs = paste0(formatC(exp(coefficients)[-1], digits = coefs_digits, format = "f"), " [",     # OR
                       formatC(exp(coefficients - 1.96 * se)[-1], digits = coefs_digits, format = "f"), "-",      # IC inf
                       formatC(exp(coefficients + 1.96 * se)[-1], digits = coefs_digits, format = "f"), "]"),# IC sup
        p = ifelse(summary(model)$coefficients[-1, 4] < p_threshold,
                   paste0("<", p_threshold),
                   formatC(summary(model)$coefficients[-1, 4], digits = p_digits, format = "f")))    # p val
    } else {
      res <- data.frame(
        Variable = rownames(summary(model)$coefficients)[-1],
        Coefs = formatC(exp(coefficients)[-1], digits = coefs_digits, format = "f"),    # OR
        CIlower = formatC(exp(coefficients - 1.96 * se)[-1], digits = coefs_digits, format = "f"),
        CIupper = formatC(exp(coefficients + 1.96 * se)[-1], digits = coefs_digits, format = "f"),
        p = ifelse(summary(model)$coefficients[-1, 4] < p_threshold,
                   paste0("<", p_threshold),
                   formatC(summary(model)$coefficients[-1, 4], digits = p_digits, format = "f"))    # p val
    )
    }
  }
  res
}
