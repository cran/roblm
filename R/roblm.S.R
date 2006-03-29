"roblm.S" <-
function(x, y, control)
{
	n <- nrow(x)
	p <- ncol(x)
	Nres <- control$Nres
	seed <- control$seed
	bb <- control$bb
	tuning.chi <- control$tuning.chi
	groups <- control$groups
	n.group <- control$n.group
	k.fast.s <- control$k.fast.s
	b <- .C("R_roblm_S",
			x = as.double(x),
			y = as.double(y),
			n = as.integer(n),
			p = as.integer(p),
			Nres = as.integer(Nres),
			scale = as.double(0),
			coef = double(p),
			seed = as.integer(seed),
			tuning.chi = as.double(tuning.chi),
			as.double(bb),
			groups = as.integer(groups),
			n.group = as.integer(n.group),
			k.fast.s = as.integer(k.fast.s),
			PACKAGE='roblm'
			)
	sigma <- b$scale
	r2 <- as.vector(y - x %*% b$coef)
	w <- roblm.Chi.prime.2(r2/sigma, cc=tuning.chi)
	A <- solve(  ( t(x) %*% (x * w) ) / n / sigma )
	w <- w * r2 / sigma
	a <- ( t(x) %*% w ) / n
	a <- a / ( mean(roblm.Chi.prime(r2/sigma, cc=tuning.chi)*r2/sigma) )
	a <- A %*% a
	w  <- roblm.Chi.prime(r2/sigma, cc=tuning.chi)
	w2 <- roblm.Chi(r2/sigma, cc=tuning.chi)
	u1 <- ( t(x) %*% (x * (w^2) ) ) / n
	u1 <- A %*% u1 %*% A
	u2 <- a %*% t( t(x) %*% (w*w2) ) %*% A / n
	u3 <- A %*% (t(x) %*% (w*w2) ) %*% t(a) / n
	u4 <- mean( roblm.Chi(r2/sigma, cc=tuning.chi)^2 - bb^2 ) * a %*% t(a) 
	b$av <- (u1 - u2 - u3 + u4)/n
	return(list(coef = b$coef, cov = b$av, control=control,
				scale = b$scale, seed = b$seed) )
}

