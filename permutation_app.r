run_raw_permutation_analysis <- function(df, weights, id_col = 1, group_col = 2, 
                                         n_perm = 10000, seed = 123, tail = "two") {
  set.seed(seed)
  
  test_cols <- setdiff(seq_along(df), c(id_col, group_col))
  rd_data <- df[, test_cols, drop = FALSE]
  group   <- df[[group_col]]
  
  stopifnot(length(weights) == ncol(rd_data))
  
  compute_weighted_t_stats <- function(data, group_vec) {
    sapply(test_cols, function(i) {
      x <- data[[i]][group_vec == 1]
      y <- data[[i]][group_vec == 0]
      t.test(x, y)$statistic
    })
  }
  
  compute_weighted_t_stats_cat <- function(data, group_vec) {
    sapply(categorical_var, function(i){
      x <- data[[i]][group_vec == 1]
      y <- data[[i]][group_vec == 0]
    })
  }
  
  t_obs <- compute_weighted_t_stats(df, group)
  sc_obs <- sum(weights * t_obs)
  qc_obs <- sum((weights * t_obs)^2)
  

  sc_null <- numeric(n_perm)
  qc_null <- numeric(n_perm)
  
  i<-1
  repeat{
    
    sample_group <- sample(group, replace = FALSE)
    
    t_values <- sapply(test_cols, function(j) {
      x <- df[[j]][sample_group == 1]
      y <- df[[j]][sample_group == 0]
      t.test(x, y, na.action = na.omit)$statistic
    })
    sc_null[i] <- sum(weights * t_values)
    qc_null[i] <- sum((weights * t_values)^2)
    
    i <- i + 1
    if(i > n_perm) break
  }
  
  sc_pval <- switch(tail,
                    "lower" = sum(sc_null <= sc_obs)/n_perm,
                    "two" =  sum(abs(sc_null) >= abs(sc_obs))/n_perm,
                    "upper" = sum(sc_null >= sc_obs)/n_perm)
  
  qc_pval <- sum(qc_null >= qc_obs)/n_perm
  
  list(
    sc_null = sc_null,
    qc_null = qc_null,
    sc_composite         = sc_obs,
    sc_pvalue            = sc_pval,
    qc_composite         = qc_obs,
    qc_pvalue            = qc_pval
  )
}
