# CM-TS_asymptotic_app.R

run_composite_analysis <- function(corr_matrix, test_stats, weights, n_sim = 1e6, seed = 123, tail = "two") {
  set.seed(seed)
  stopifnot(ncol(corr_matrix) == length(test_stats))
  
  sc_composite <- sum(weights * test_stats)
  variance <- as.numeric(t(weights) %*% corr_matrix %*% weights)
  sc_pvalue <- switch(tail,
                      "lower" = pnorm(sc_composite / sqrt(variance)),
                      "two" = 2*(1-pnorm(abs(sc_composite)/sqrt(variance))),
                      "upper" = 1 - pnorm(sc_composite / sqrt(variance)))# estimated from theoretical formula
  sc_null_distribution <- rnorm(n_sim, mean = 0, sd = sqrt(variance))

  sc_pvalue <- switch(tail, "lower" = sum(sc_null_distribution <= sc_composite)/n_sim,
                      "two" = sum(abs(sc_null_distribution) >= abs(sc_composite))/n_sim,
    "upper" = sum(sc_null_distribution >= sc_composite)/n_sim) 
  
  eigen_decomp <- eigen(corr_matrix)
  srootD <- diag(sqrt(eigen_decomp$values))
  Q <- eigen_decomp$vectors
  middle_mat <- srootD%*%t(Q)%*%diag(weights)%*%Q%*%srootD
  rnorm_mat <- matrix(rnorm(n_sim*length(test_stats)),ncol=length(test_stats))
  null_sims <- rowSums((rnorm_mat %*% middle_mat) * rnorm_mat)
  qc_null_distribution <- null_sims
  qc_composite <- sum(weights*test_stats^2)
  qc_pvalue <- sum(qc_null_distribution >= qc_composite)/n_sim
  
  qc_null_distribution <- as.numeric(qc_null_distribution)
  sc_null_distribution <- as.numeric(sc_null_distribution)
  
  list(
    sc_null_distribution = sc_null_distribution,
    qc_null_distribution = qc_null_distribution,
    sc_composite = sc_composite,
    sc_pvalue    = sc_pvalue,
    qc_composite = qc_composite,
    qc_pvalue    = qc_pvalue
  )

}
