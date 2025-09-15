run_raw_permutation_analysis <- function(
    df, weights, id_col = 1, group_col = 2,
    n_perm = 1000, seed = 123, categorical_var = NULL,
    survival_pairs = NULL, survival_weights = NULL,
    censor_value_means_censored = 1  # if the censor column uses 1=censored, 0=event
) {
  set.seed(seed)
  
  test_cols <- setdiff(seq_along(df), c(id_col, group_col))
  cn        <- names(df)
  group     <- factor(df[[group_col]])
  
  # remove survival time & censor from standard test cols
  surv_times  <- if (length(survival_pairs)) vapply(survival_pairs, `[[`, "", "time")   else character(0)
  surv_censor <- if (length(survival_pairs)) vapply(survival_pairs, `[[`, "", "censor") else character(0)
  test_cols   <- setdiff(test_cols, match(c(surv_times, surv_censor), cn))
  
  # weights for standard tests cols
  weights <- suppressWarnings(as.numeric(weights))
  stopifnot(length(weights) == length(test_cols))
  is_cat  <- cn[test_cols] %in% (categorical_var %||% character(0))
  
  # survival weights 
  if (!length(survival_pairs)) survival_pairs <- list()
  if (is.null(survival_weights)) survival_weights <- rep(1, length(survival_pairs))
  if (length(survival_weights) != length(survival_pairs))
    stop("Length of survival_weights must equal number of survival pairs.")
  if (any(!is.finite(survival_weights)))
    stop("All survival_weights must be finite.")
  
  
  per_var_stat <- function(col_idx, group_vec) {
    j <- test_cols[col_idx]
    v <- df[[j]]
    
    
    keep <- !(is.na(group_vec))
    if (!any(keep)) return(0)
    
    vv <- v[keep]
    g  <- droplevels(as.factor(group_vec[keep]))
    G  <- nlevels(g)

    if (is_cat[col_idx]) {
      # categorical variables
      fv <- droplevels(as.factor(vv))
      K  <- nlevels(fv)
      if (K < 2L) return(0)
      
      tab <- table(g, fv)
      # require non-empty rows/cols
      if (any(rowSums(tab) == 0) || any(colSums(tab) == 0)) return(0)
      
      chi2 <- suppressWarnings(as.numeric(chisq.test(tab, correct = FALSE)$statistic))
      if (!is.finite(chi2)) return(0)
      
      stat <- (chi2 / ((G - 1) * (K - 1)))
      if (!is.finite(stat)) 0 else stat
      
    } else {
      # continuous/binary variable branch
      fstat <- as.numeric(oneway.test(vv ~ g, var.equal = FALSE)$statistic)
      
      if (!is.finite(fstat)) return(0)
      
      stat <- (fstat / (G - 1))
      if (!is.finite(stat)) 0 else stat
    }
  }
  
  
  # Survival BrANCH
  surv_data <- lapply(survival_pairs, function(p) {
    time   <- df[[p$time]]
    censor <- df[[p$censor]]
    keep   <- complete.cases(time, censor, group)
    if (!any(keep)) return(NULL)
    t  <- as.numeric(time[keep])
    cz <- censor[keep]
    cz_event <- as.numeric(!(cz == censor_value_means_censored))
    list(keep = keep, t = t, event = cz_event)
  })
  
  per_surv_stat <- function(k, group_vec) {
    sd <- surv_data[[k]]
    if (is.null(sd)) return(0)
    g <- droplevels(as.factor(group_vec[sd$keep]))
    G <- nlevels(g)
    if (G < 2L) return(0)
    
    fit   <- survdiff(Surv(sd$t, sd$event) ~ g, rho = 0)
    chi2  <- unname(fit$chisq)
    if (!is.finite(chi2) || chi2 <= 0) return(0)
    
    stat <- (chi2 / (G - 1))
    if (!is.finite(stat)) 0 else stat
  }
  
  
  # OBSERVED
  t_sq_obs_std <- if (length(test_cols)) sapply(seq_along(test_cols), per_var_stat, group_vec = group) else numeric(0)
  t_sq_obs_sur <- if (length(surv_data))  vapply(seq_along(surv_data),  per_surv_stat, numeric(1), group_vec = group) else numeric(0)
  
  qc_obs <- sum((weights * t_sq_obs_std)) +
    sum((survival_weights * t_sq_obs_sur))
  
#PERMUTED
  n <- length(group)
  qc_null <- numeric(n_perm)
  for (i in seq_len(n_perm)) {
    g_perm <- sample(group, n, replace = FALSE)
    t_std  <- if (length(test_cols)) sapply(seq_along(test_cols), per_var_stat, group_vec = g_perm) else numeric(0)
    t_sur  <- if (length(surv_data))  vapply(seq_along(surv_data),  per_surv_stat, numeric(1), group_vec = g_perm) else numeric(0)
    qc_null[i] <- sum((weights * t_std)) + sum((survival_weights * t_sur))
  }
  
  qc_pval <- (sum(qc_null >= qc_obs)) / (n_perm)
  
  list(
    qc_null      = qc_null,
    qc_composite = qc_obs,
    qc_pvalue    = qc_pval
  )
}