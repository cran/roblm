"print.summary.roblm" <-
function (x, digits = max(3, getOption("digits") - 3), symbolic.cor=
          x$symbolic.cor, signif.stars = getOption("show.signif.stars"),
          ...) 
{
    cat("\nCall:\n")
    cat(paste(deparse(x$call), sep = "\n", collapse = "\n"), 
        "\n\n", sep = "")
    resid <- x$residuals
    df <- x$df
    #df
    cat(if (!is.null(x$w) && diff(range(x$w))) 
        "Weighted ", "Residuals:\n", sep = "")
    if (df > 5) {
        nam <- c("Min", "1Q", "Median", "3Q", "Max")
        if (length(dim(resid)) == 2) 
            rq <- structure(apply(t(resid), 1, quantile), dimnames = list(nam, 
                dimnames(resid)[[2]]))
        else rq <- structure(quantile(resid), names = nam)
        print(rq, digits = digits, ...)
    }
    else print(resid, digits = digits, ...)
	if( !(x$converged) ) {
		cat("\nAlgorithm did not converge\n")
    	cat("\nCoefficients of *initial* estimator:\n")
    	printCoefmat(x$coef, digits = digits, signif.stars = signif.stars, 
       	 ...)
		cat("\n")
	} else {
    cat("\nCoefficients:\n")
    printCoefmat(x$coef, digits = digits, signif.stars = signif.stars, 
        ...)
    cat("\nRobust residual standard error:", format(signif(x$scale, 
        digits)),"\n")
    correl <- x$correlation
    if (!is.null(correl)) {
        p <- NCOL(correl)
        if (p > 1) {
            cat("\nCorrelation of Coefficients:\n")
            if (is.logical(symbolic.cor) && symbolic.cor) {
                print(symnum(correl), abbr.col=NULL)
            } 
        else {correl <- format(round(correl, 2), nsmall = 2, 
			digits = digits)
              correl[!lower.tri(correl)] <- ""
              print(correl[-1, -p, drop = FALSE], quote=FALSE)
            }
        }
    }
    cat("\n")
    invisible(x)
	}
}

