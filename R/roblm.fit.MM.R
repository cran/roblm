"roblm.fit.MM" <-
function(x, y, control)
{
	x <- as.matrix(x)
	n <- nrow(x)
	p <- ncol(x)

	# find the initial (50% BP S) estimator and the
	# associated residual scale estimator
	initial.S <- roblm.S(x=x, y=y, control=control)

	# find the final (95% efficient M) estimator using
	# RWLS iterations
	final.MM <- roblm.MM(x=x, y=y, beta.initial = initial.S$coef, 
				scale = initial.S$scale, control=control)

	coef.initial <- initial.S$coef

	# If IRWLS does not converge, use the S estimator
	if( !final.MM$converged) {
		coef <- initial.S$coef
		cov.matrix <- matrix(initial.S$cov, p, p)
	} else {
		cov.matrix <- matrix(final.MM$cov, p, p)
		coef <- final.MM$coef
	}

    rank <- qr(x)$rank
	r1 <- 1:rank
	dn <- colnames(x)
	if (is.matrix(y)) {
	  coef[-r1, ] <- NA
	  dimnames(coef) <- list(dn, colnames(y))
	}
	else {
	  coef[-r1] <- NA
	  names(coef) <- dn
	}
	f <- x %*% as.matrix(coef)
	r <- as.matrix(y) - f
	z <- list(fitted.value=as.vector(f), residuals=as.vector(r), 
	rank=rank, degree.freedom=n-rank, coefficients=coef, 
	initial.coefficients = coef.initial, 
	scale=final.MM$scale, seed=final.MM$seed, cov=cov.matrix, 
	converged = final.MM$converged)
	return(z)
}

