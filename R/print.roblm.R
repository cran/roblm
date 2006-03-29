"print.roblm" <-
function(x, digits = max(3, getOption("digits") - 3), ...) 
{
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
	u <- "Coefficients:\n"
	if( !(x$converged) ) {
		cat("Algorithm did not converge\n\n")
    	u <- "Coefficients of the *initial* estimator:\n"
	}
    cat(u)
    print.default(format(coef(x), digits = digits), print.gap = 2, 
        quote = FALSE)
    cat("\n")
    invisible(x)
}

