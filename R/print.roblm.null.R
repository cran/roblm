"print.roblm.null" <-
function (x, digits = max(3, getOption("digits") - 3), ...) 
{
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
    cat("No coefficients:\n\n")
    invisible(x)
}

