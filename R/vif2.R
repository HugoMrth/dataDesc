vif2 <- function (mod, multinom = F) 
{
  if (multinom) {
    if (any(is.na(coef(mod)))) 
      stop("there are aliased coefficients in the model")
    v <- vcov(mod)
    assign <- attr(model.matrix(mod), "assign")
    if (colnames(coefficients(mod))[1] == "(Intercept)") {
      v <- v[-which(stringr::str_detect(colnames(v), "(Intercept)")), 
             -which(stringr::str_detect(rownames(v), "(Intercept)"))]
      assign <- assign[-1]
    } else {
      warning("No intercept: vifs may not be sensible.")
    }
    terms <- labels(terms(mod))
    n.terms <- length(terms)
    if (n.terms < 2) 
      stop("model contains fewer than 2 terms")
    result <- list()
    for (i in 1:length(unique(sub(":.*", "", colnames(v))))) {
      imin <- ((i-1)*(ncol(v)/length(unique(sub(":.*", "", colnames(v)))))+1)
      imax <- i*ncol(v)/length(unique(sub(":.*", "", colnames(v))))
      vtemp <- v[imin:imax, imin:imax]
      R <- cov2cor(vtemp)
      detR <- det(R)
      res <- matrix(0, n.terms, 3)
      rownames(res) <- terms
      colnames(res) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
      for (term in 1:n.terms) {
        subs <- which(assign == term)
        res[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs]))/detR
        res[term, 2] <- length(subs)
      }
      if (all(res[, 2] == 1)) {
        res <- res[, 1]
      } else {
        res[, 3] <- abs(res[, 1])^(1/(2 * res[, 2]))
      }
      result[[i]] <- res
    }
    names(result) <- unique(sub(":.*", "", colnames(v)))
  } else {
    if (any(is.na(coef(mod)))) 
      stop("there are aliased coefficients in the model")
    v <- vcov(mod)
    assign <- attr(model.matrix(mod), "assign")
    if (names(coefficients(mod)[1]) == "(Intercept)") {
      v <- v[-1, -1]
      assign <- assign[-1]
    }
    else warning("No intercept: vifs may not be sensible. Consider using multinom = TRUE if need be.")
    terms <- labels(terms(mod))
    n.terms <- length(terms)
    if (n.terms < 2) 
      stop("model contains fewer than 2 terms")
    R <- cov2cor(v)
    detR <- det(R)
    result <- matrix(0, n.terms, 3)
    rownames(result) <- terms
    colnames(result) <- c("GVIF", "Df", "GVIF^(1/(2*Df))")
    for (term in 1:n.terms) {
      subs <- which(assign == term)
      result[term, 1] <- det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, 
                                                                         -subs]))/detR
      result[term, 2] <- length(subs)
    }
    if (all(result[, 2] == 1)) 
      result <- result[, 1]
    else result[, 3] <- result[, 1]^(1/(2 * result[, 2]))
  }
  result
}
