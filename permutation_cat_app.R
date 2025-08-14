
## Permutation code for Raw data on App capable of handling categorical variables
run_raw_permutation_analysis <- function(df, weights, id_col = 1, group_col = 2, 
                                         n_perm = 1000, seed = 123, categorical_var = NULL) {
  set.seed(seed)
  
  
  test_cols <- setdiff(seq_along(df), c(id_col, group_col))
  rd_data   <- df[, test_cols, drop = FALSE]
  group     <- df[[group_col]]
  weights <- suppressWarnings(as.numeric(weights))
  cn <- names(df)
  is_cat <- cn[test_cols] %in% (categorical_var %||% character(0))
  
  stopifnot(length(weights) == ncol(rd_data))
  
  per_var_stat <- function(col_idx, group_vec) {
    j <- test_cols[col_idx]
    v <- df[[j]]
    
    if (is_cat[col_idx]) {
      # --- Categorical branch: explicit joint NA filter ---
      keep <- !(is.na(v) | is.na(group_vec)) ## this removes rows with an NA for any of the columns
      if (!any(keep)) return(0)
      
      gv <- group_vec[keep]
      fv <- droplevels(factor(v[keep]))  
      if (nlevels(fv) < 2L || length(unique(gv)) < 2L) return(0)
      
      tab <- table(gv, fv) 
      if (any(colSums(tab) == 0) || any(rowSums(tab) == 0)) return(0)
      
      K <- nlevels(fv)
      sqrt(as.numeric(suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)) / (K-1))
      
    } else {
      x <- v[group_vec == 1]
      y <- v[group_vec == 0]
      tval <- tryCatch(
        as.numeric(t.test(x, y, na.action = na.omit)$statistic),      ##t.test drop NAs within groups
        error = function(e) 0
      )
      if (is.na(tval)) 0 else tval
    }
  }
  
  
  # observed stats
  t_obs <- sapply(seq_along(test_cols), per_var_stat, group_vec = group)
  t_obs <- as.numeric(t_obs)
  qc_obs <- sum((weights * t_obs)^2)
  
  # permutation nulls
  qc_null <- numeric(n_perm)
  
  n <- length(group)
  i <- 1L
  repeat {
    sample_group <- sample(group, size = n, replace = FALSE)
    t_values <- sapply(seq_along(test_cols), per_var_stat, group_vec = sample_group)
    qc_null[i] <- sum((weights * t_values)^2)
    i <- i + 1L
    if (i > n_perm) break
  }
  

  qc_pval <- sum(qc_null >= qc_obs)/n_perm
  
  list(
    qc_null = qc_null,
    qc_composite = qc_obs,
    qc_pvalue = qc_pval
  )
}
