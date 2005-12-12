"summary.roblm" <-
function (object, correlation = FALSE, 
		symbolic.cor=FALSE, ...) 
{
    z <- object
    if (is.null(z$terms)) 
        stop("invalid 'roblm' object:  no terms component")
    p <- z$rank
    df <- z$degree.freedom
    n <- p + df
    r <- z$residuals
    f <- z$fitted.value
    w <- z$weights
    se <- sqrt(diag(z$cov))
    est <- z$coefficients
    tval <- est/se
    ans <- z[c("call", "terms")]
    ans$df<-df
    ans$residuals <- r
    ans$coefficients <- cbind(est, se, tval, 2 * pt(abs(tval), 
        df, lower.tail = FALSE))
    dimnames(ans$coefficients) <- list(names(z$coefficients), 
        c("Estimate", "Std. Error", "t value", "Pr(>|t|)"))
    ans$scale <- z$scale
    ans$cov.unscaled <- z$cov
    dimnames(ans$cov.unscaled) <- dimnames(ans$coefficients)[c(1,1)]
    if (correlation) {
        ans$correlation <- (z$cov) / outer(se, se)
        dimnames(ans$correlation) <- dimnames(ans$cov.unscaled)
	ans$symbolic.cor <- symbolic.cor
    }
    class(ans) <- "summary.roblm"
    ans
}
