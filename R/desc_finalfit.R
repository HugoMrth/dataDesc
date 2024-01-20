desc_finalfit <- function(DATA,
                          dependent,
                          explanatory_uni,
                          explanatory_multi,
                          label = NULL,
                          family = "binomial",
                          decimal = 2,
                          p.decimal = 3,
                          ratio.label = "OR",
                          langue = "fr",
                          merge.cols = FALSE) {
  if (langue == "fr") {
    suffix <- c(" (univarié)", " (multivarié)")
    col <- c("Univarié", "Multivarié")
    lab.IC <- "IC95%"
  } else {
    suffix <- c(" (univariable)", " (multivariable)")
    col <- c("Univariable", "Multivariable")
    lab.IC <- "CI95%"
  }

  # DATA <- DATA_mod
  # dependent <- "reprise.tardive"
  # explanatory_uni <- var_signif_REPRISE
  # explanatory_multi <- c("age.lors.du.trauma",  "score.igs.ii", "nombre.de.membre.ampute")

  FIFIT <- DATA %>%
    glmuni(dependent, explanatory_uni, family = family) %>%
    fit2df(exp = TRUE, estimate_name = ratio.label, estimate_suffix = suffix[1],
           digits = c(decimal, decimal, p.decimal), remove_intercept = FALSE) %>%
    dplyr::full_join(DATA %>%
                       glmmulti(dependent, explanatory_multi, family = family) %>%
                       fit2df(exp = TRUE, estimate_name = ratio.label, estimate_suffix = suffix[2],
                              digits = c(decimal, decimal, p.decimal, remove_intercept = FALSE))) %>%
    dplyr::mutate_at(.vars = 3, ~replace(., is.na(.), "-"))

  FIFIT$Variable <- rep(" ", nrow(FIFIT))

  if (!is.null(label)) {
    FIFIT$Variable[which(FIFIT$explanatory == "(Intercept)")] <- label
  } else {
    FIFIT$Variable[which(FIFIT$explanatory == "(Intercept)")] <- explanatory_uni
  }
  FIFIT[which(FIFIT$explanatory == "(Intercept)"), 2:3] <- "-"

  N <- 0
  for (i in 1:nrow(FIFIT)) {
    if(FIFIT$explanatory[i] == "(Intercept)") {
      N <- N + 1
      FIFIT$explanatory[i] <- ifelse(is.numeric(DATA[, explanatory_uni[N]]),
                                     "",
                                     levels(DATA[, explanatory_uni[N]])[1])
    } else {
      FIFIT$explanatory[i] <- str_replace(FIFIT$explanatory[i], explanatory_uni[N], "")
    }
  }

  FIFIT <- FIFIT[, c(ncol(FIFIT), 1:(ncol(FIFIT)-1))]
  colnames(FIFIT)[2] <- "Modalité"

  if (!merge.cols) {
    FIFIT <- rbind(c("", "", ratio.label, lab.IC, "p", ratio.label, lab.IC, "p"),
                   cbind(FIFIT[, 1:2],
                         str_split_fixed(FIFIT[, 3], " ", 3),
                         str_split_fixed(FIFIT[, 4], " ", 3)))
    FIFIT <- apply(FIFIT, 2, function(x) {
      str_replace_all(x, "\\(|,|\\)|p=", "")
    })

    colnames(FIFIT)[3:5] <- col[1]
    colnames(FIFIT)[6:8] <- col[2]
  }

  as.data.frame(FIFIT)
}
