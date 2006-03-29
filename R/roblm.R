"roblm" <-
function (formula, data = list(), weights, na.action,
    model = TRUE, x = FALSE, y = FALSE,
    singular.ok = TRUE, contrasts = NULL, offset = NULL, 
    control=roblm.control())
{
    ret.x <- x
    ret.y <- y
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    mf$singular.ok <- mf$model <- NULL
    mf$x <- mf$y <- mf$contrasts <- mf$control <- NULL
    mf$drop.unused.levels <- TRUE
    mf[[1]] <- as.name("model.frame")
    mf <- eval(mf, parent.frame())
    mt <- attr(mf, "terms")
    na.act <- attr(mf, "na.action")
    xvars <- as.character(attr(mt, "variables"))[-1]
    x <- as.matrix(mf[-1])
    if ((yvar <- attr(mt, "response")) > 0) 
        xvars <- xvars[-yvar]
    xlev <- if (length(xvars) > 0) {
        xlev <- lapply(mf[xvars], levels)
        xlev[!sapply(xlev, is.null)]
    }
    if (!singular.ok) 
        warning("only `singular.ok = TRUE' is currently implemented.")
    y <- model.response(mf, "numeric")
    w <- model.weights(mf)
    offset <- model.offset(mf)
    if (!is.null(offset) && length(offset) != NROW(y)) 
        stop(paste("Number of offsets is", length(offset), ", should equal", 
            NROW(y), "(number of observations)"))
    if (is.empty.model(mt)) {
        xx <- NULL
        z <- list(coefficients = numeric(0), residuals = y, fitted.values = 0 * 
            y + offset, weights = w, rank = 0, df.residual = length(y))
        class(z) <- c("roblm.null", "roblm")
    }
    else {
        xx <- model.matrix(mt, mf, contrasts)
        z <- if (is.null(w)) 
            roblm.fit.MM(xx, y, control=control)
        else stop("Weights are not implemented for this estimator")
        class(z) <- c("roblm")
    }
    if (!is.null(na.act)) 
        z$na.action <- na.act
    z$offset <- offset
    z$contrasts <- attr(xx, "contrasts")
    z$xlevels <- xlev
    z$call <- cl
    z$terms <- mt
	if( control$compute.rd ) {
    	try( expr = {
			rob <- MASS::cov.rob(x, method='mcd');
			z$MD <- sqrt( mahalanobis(x, rob$center, rob$cov) )
			}, 
			silent = TRUE )
	}
    if (model) 
        z$model <- mf
    if (ret.x) 
        z$x <- xx
    if (ret.y) 
        z$y <- y
	z$control <- control
    z
}


.First.lib <- function(lib, pkg) {

    where <- match(paste("package:", pkg, sep = ""), search())
    ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
    ver <- as.character(ver)
    title <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Title")
    title <- as.character(title)
    cat(paste(title, " (version ", ver, ")\n", sep = ""))

    library.dynam("roblm", pkg, lib)
}


