"roblm.fit.MM" <-
function(x, y, control=control)
{
	M <- control$M
	Nres <- control$Nres
	n <- nrow(x)
	p <- ncol(x)
	if(is.na(Nres)) 
		Nres <- min(floor(-log(.01)/(1-.5)^p), 10000)
	seed <- control$seed
	fixed <- control$fixed
	tuning.chi <- control$tuning.chi
	tuning.psi <- control$tuning.psi
	compute.roboot <- control$compute.roboot
	groups <- control$groups
	n.group <- control$n.group
	k.fast.s <- control$k.fast.s
	max.it <- control$max.it
	if(fixed)
		a <- .C("R_rlm_fixed",
			x = as.double(x),
			y = as.double(y),
			n = as.integer(n),
			p = as.integer(p),
			Nres = as.integer(Nres),
			M = as.integer(M),
			max.it = as.integer(max.it),
			ours = double(M * p),
			av = double(p*p),
			coef = double(p),
			s = double(p),
			scale = as.double(0),
			converged.mm = as.integer(0),
			seed = as.integer(seed),
			tuning.chi = as.double(tuning.chi),
			tuning.psi = as.double(tuning.psi),
			compute.roboot = as.integer(compute.roboot),
			groups = as.integer(groups),
			n.group = as.integer(n.group),
			k.fast.s = as.integer(k.fast.s),
			PACKAGE='roblm'
			)
	else a <- .C("R_rlm_rand",
			x = as.double(x),
			y = as.double(y),
			n = as.integer(n),
			p = as.integer(p),
			Nres = as.integer(Nres),
			M = as.integer(M),
			max.it = as.integer(max.it),
			ours = double(M * p),
			av = double(p*p),
			coef = double(p),
			s = double(p),
			scale = as.double(0),
			converged.mm = as.integer(0),
			seed = as.integer(seed),
			tuning.chi = as.double(tuning.chi),
			tuning.psi = as.double(tuning.psi),
			compute.roboot = as.integer(compute.roboot),
			groups = as.integer(groups),
			n.group = as.integer(n.group),
			k.fast.s = as.integer(k.fast.s),
			PACKAGE='roblm'
			)
	a$ours <- matrix(a$ours, nrow = M)
	a$av <- matrix(a$av, p, p)
	coef <- a$coef
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
	f <- x %*% as.matrix(a$coef)
	r <- as.matrix(y) - f
	if(compute.roboot) 
		cov.matrix <- var(a$ours)
	else
		cov.matrix <- a$av
	z <- list(fitted.value=as.vector(f), residuals=as.vector(r), 
	rank=rank, degree.freedom=n-rank, coefficients=coef, s=a$s, 
	scale=a$scale, seed=a$seed, cov=cov.matrix, av.cov = a$av,
	converged = (a$converged.mm == 1))
	return(z)
}
