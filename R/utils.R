#################################################################################
# orthonormalization: Gram-Schmidt Orthonormalization contained in "far" package
#################################################################################
# A set of unit vectors is returned in case of collinearlity.

orthonormalization <- function (u = NULL, basis = TRUE, norm = TRUE) {
  #    if (is.null(u))
  #        return(NULL)
  #    if (!(is.matrix(u)))
  #        u <- as.matrix(u)
  p <- nrow(u)
  n <- ncol(u)
  #    if (prod(abs(La.svd(u)$d) > 1e-08) == 0)
  #        stop("collinear vectors")
  #    if (p < n) {
  #        warning("too much vectors to orthogonalize.")
  #        u <- as.matrix(u[, 1:p])
  #        n <- p
  #    }
  #    if (basis & (p > n)) {
  #        base <- diag(p)
  #        coef.proj <- crossprod(u, base)/diag(crossprod(u))
  #        base2 <- base - u %*% matrix(coef.proj, nrow = n, ncol = p)
  #        norm.base2 <- diag(crossprod(base2))
  #        base <- as.matrix(base[, order(norm.base2) > n])
  #        u <- cbind(u, base)
  #        n <- p
  #    }
  if (prod(abs(La.svd(u)$d) > 1e-08) == 0) {		# added by wada
    warning("collinears vectors")			# added by wada
    v <- matrix(0, nrow=p, ncol=p)			# added by wada
    diag(v) <- 1					# added by wada
    return(v)					# added by wada
  }    						# added by wada
  v <- u
  if (n > 1) {
    for (i in 2:n) {
      coef.proj <- c(crossprod(u[, i], v[, 1:(i - 1)]))/diag(crossprod(v[,
                                                                         1:(i - 1)]))
      v[, i] <- u[, i] - matrix(v[, 1:(i - 1)], nrow = p) %*%
        matrix(coef.proj, nrow = i - 1)
    }
  }
  if (norm) {
    coef.proj <- 1/sqrt(diag(crossprod(v)))
    v <- t(t(v) * coef.proj)
  }
  return(v)
}
