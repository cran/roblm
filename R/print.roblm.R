"print.roblm" <-
function(x, digits = max(3, getOption("digits") - 3), ...) 
{
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    cat("Coefficients:\n")
    print.default(format(coef(x), digits = digits), print.gap = 2, 
        quote = FALSE)
    cat("\n")
    invisible(x)
}
