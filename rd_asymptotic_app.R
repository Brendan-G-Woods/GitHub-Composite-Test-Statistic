##Raw data Asymptotic Null Distribution Function for App

run_rawdata_analysis <- function(df, weights, id_col = 1, group_col = 2, n_sim = 10^6, seed = 24253739, tail = "two") {
  set.seed(seed)
  
  test_cols <- setdiff(seq_along(df), c(id_col, group_col))
  rd_data <- df[, test_cols, drop = FALSE]
  grp <- df[[group_col]]
  
  test_stats <- sapply(test_cols, function(i) {
    x <- df[[i]][grp == 1]
    y <- df[[i]][grp == 0]
    t.test(x, y)$statistic
  })
  names(test_stats) <- names(df)[test_cols]
  
  
  
  stopifnot(length(test_stats) == ncol(rd_data))
  
  
  rd_cm <- cor(rd_data, use = "complete.obs")
  
  
  sc_composite <- sum(weights * test_stats)
  variance <- as.numeric(t(weights) %*% rd_cm %*% weights)
  sc_pvalue <- switch(tail,
                      "lower" = pnorm(sc_composite / sqrt(variance)),
                      "two" = 2*(1-pnorm(abs(sc_composite)/sqrt(variance))),
                      "upper" = 1 - pnorm(sc_composite / sqrt(variance)))# estimated from theoretical formula
  sc_null_distribution <- rnorm(n_sim, mean = 0, sd = sqrt(variance))
  
  qc_composite <- sum(weights*test_stats^2)
  eigen_decomp <- eigen(rd_cm)
  srootD <- diag(sqrt(eigen_decomp$values))
  Q <- eigen_decomp$vectors
  middle_mat <- srootD%*%t(Q)%*%diag(weights)%*%Q%*%srootD
  rnorm_mat <- matrix(rnorm(n_sim*length(test_stats)),ncol=length(test_stats))
  null_sims <- rowSums((rnorm_mat %*% middle_mat) * rnorm_mat)
  qc_null_distribution <- null_sims
  qc_pvalue <- sum(null_sims >= qc_composite)/n_sim

  
  list(
    sc_null_distribution = sc_null_distribution,
    qc_null_distribution = qc_null_distribution,
    correlation_matrix = rd_cm,
    sc_composite = sc_composite,
    sc_pvalue    = sc_pvalue,
    qc_composite = qc_composite,
    qc_pvalue    = qc_pvalue
  )
}

