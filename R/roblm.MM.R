"roblm.MM" <-
function(x, y, beta.initial, scale, control)
{
	n <- nrow(x)
	p <- ncol(x)
	tuning.psi <- control$tuning.psi
	max.it <- control$max.it
	b <- .C("R_roblm_MM",
			x = as.double(x),
			y = as.double(y),
			n = as.integer(n),
			p = as.integer(p),
			beta.initial = as.double(beta.initial),
			scale = as.double(scale),
			coef = double(p),
			as.integer(max.it),
			tuning.psi = as.double(tuning.psi),
			converged = integer(1),
			PACKAGE='roblm'
			)
	c.psi <- control$tuning.psi
	c.chi <- control$tuning.chi
	bb <- control$bb
	sigma <- scale
	r <- as.vector(y - x %*% b$coef)
	r2 <- as.vector(y - x %*% beta.initial)
	w <- roblm.Psi.prime(r/sigma, cc=c.psi)
	A <- solve(  ( t(x) %*% (x * w) ) / n / sigma )
	w <- w * r / sigma
	a <- ( t(x) %*% w ) / n
	a <- a / ( mean(roblm.Chi.prime(r2/sigma, cc=c.chi)*r2/sigma) )
	a <- A %*% a
	w  <- roblm.Psi(r/sigma, cc=c.psi)
	w2 <- roblm.Chi(r2/sigma, cc=c.chi)
	u1 <- ( t(x) %*% (x * (w^2) ) ) / n
	u1 <- A %*% u1 %*% A
	u2 <- a %*% t( t(x) %*% (w*w2) ) %*% A / n
	u3 <- A %*% (t(x) %*% (w*w2) ) %*% t(a) / n
	u4 <- mean( roblm.Chi(r2/sigma, cc=c.chi)^2 - bb^2 ) * a %*% t(a) 
	b$av <- (u1 - u2 - u3 + u4)/n
	return(list(coef = b$coef, cov = b$av, control=control,
				scale = b$scale, seed = b$seed, converged = 
				(b$converged == 1) ) )
}

